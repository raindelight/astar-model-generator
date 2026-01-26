package com.beepboop.app.dataprovider

import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps
import spray.json.*
import com.beepboop.app.MinizincDznVisitor
import com.beepboop.app.logger.LogTrait
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser}

import scala.collection.JavaConverters.asScalaBufferConverter
import scala.util.matching.Regex


object DataImporter extends DefaultJsonProtocol, LogTrait {

  def prepareSets(data: String, dataItems: List[DataItem]): Unit = {
    dataItems.filter(i => i.detailedDataType != null && i.detailedDataType.isSet && (i.value == null || i.value == None)).foreach { item =>
      val expr = item.expr

      if (expr != null && expr.trim.nonEmpty) {
        try {
          if (expr.contains("..")) {
            val parts = parseRange(expr)

            val minVal = resolveBound(parts(0), dataItems)
            val maxVal = resolveBound(parts(1), dataItems)

            val setValues = (minVal to maxVal).toList
            item.value = setValues

            info(s"Set ${item.name} resolved: $minVal..$maxVal (size: ${setValues.size})")
          }
        } catch {
          case e: Exception =>
            warn(s"Failed to resolve set '${item.name}' with expr '$expr': ${e.getMessage}")
        }
      }
    }
  }

  def resolveBound(boundStr: String, dataItems: List[DataItem]): Int = {
    val raw = boundStr.trim

    if (raw.matches("-?\\d+")) {
      return raw.toInt
    }

    val SumPattern = """sum\s*\(\s*(.*)\s*\)""".r
    val MinPattern = """min\s*\(\s*(.*)\s*\)""".r
    val MaxPattern = """max\s*\(\s*(.*)\s*\)""".r

    raw match {
      case SumPattern(varName) =>
        getAsIntList(varName, dataItems).sum

      case MinPattern(varName) =>
        val list = getAsIntList(varName, dataItems)
        if (list.nonEmpty) list.min else 0

      case MaxPattern(varName) =>
        val list = getAsIntList(varName, dataItems)
        if (list.nonEmpty) list.max else 0

      case varName =>
        lookupDependency(varName, dataItems) match {
          case i: Int => i
          case s: String => Try(s.toInt).getOrElse(throw new IllegalArgumentException(s"Value '$s' for '$varName' is not an integer."))
          case other => throw new IllegalArgumentException(s"Dependency '$varName' is not a scalar integer (found ${other.getClass.getSimpleName}).")
        }
    }
  }

  private def getAsIntList(name: String, dataItems: List[DataItem]): List[Int] = {
    lookupDependency(name, dataItems) match {
      case l: List[_] => l.map {
        case i: Int => i
        case s: String => Try(s.toInt).getOrElse(0)
        case d: Double => d.toInt
        case other => throw new IllegalArgumentException(s"List element in '$name' is not numeric: $other")
      }
      case other => throw new IllegalArgumentException(s"Dependency '$name' is not a list (found ${other.getClass.getSimpleName}).")
    }
  }

  def lookupDependency(name: String, dataItems: List[DataItem]): Any = {
    dataItems.find(d => d.name == name && !d.isVar) match {
      case Some(foundItem) =>
        if (foundItem.value == null || foundItem.value == None) {
          throw new IllegalArgumentException(s"Dependency '$name' found but has no value yet.")
        }
        foundItem.value
      case None =>
        throw new IllegalArgumentException(s"Dependency '$name' not found in dataItems.")
    }
  }
  def resolveValue(value: Any): Any = {
    value match {
      case key: String => DataProvider.getValue(key)
      case other => other
    }
  }
  def parseRange(expr: String): Array[String] = {
    val parts = expr.split("\\.\\.")
    require(parts.length == 2, s"Invalid range format for expr: $expr")
    parts
  }


  private def extractJsonValue(jsValue: JsValue): Any = jsValue match {
    case JsObject(fields) if fields.contains("set") =>
      extractJsonValue(fields("set"))
    case JsArray(elements) =>
      elements.map(extractJsonValue).toList
    case JsNumber(num) =>
      if (num.isValidInt) num.toInt else num.toDouble
    case JsBoolean(b) => b
    case JsString(s) => s
    case _ => throw new DeserializationException(s"Unexpected JSON format: $jsValue")
  }

  def parseJson(data: String, dataItems: List[DataItem], parseVars: Boolean = false): Unit = {
    val jsonObject = data.parseJson.asJsObject
    val targetItems = if (parseVars) dataItems.filter(_.isVar) else dataItems.filter(!_.isVar)

    targetItems.foreach { item =>
      (jsonObject.fields.get(item.name), item.detailedDataType) match {
        case (Some(jsValue), details) if details != null =>
          try {
            val rawValue = extractJsonValue(jsValue)
            val typeName = Option(details.dataType).getOrElse("int").toLowerCase

            val convertedValue: Any = typeName match {
              case t if t.contains("float") || t.contains("decimal") =>
                rawValue match {
                  case n: Number => n.doubleValue()
                  case l: List[_] => l.map {
                    case n: Number => n.doubleValue()
                    case other => other
                  }
                  case other => other
                }

              case t if t.contains("bool") => rawValue

              case t if t.contains("string") => rawValue

              case _ =>
                rawValue match {
                  case n: Number => n.intValue()
                  case l: List[_] =>
                    if (l.isEmpty) l
                    else {
                      l.head match {
                        case _: List[_] => l.asInstanceOf[List[List[Int]]]
                        case _ =>
                          l.map {
                            case n: Number => n.intValue()
                            case s: String => scala.util.Try(s.toInt).getOrElse(0)
                            case other => other
                          }.asInstanceOf[List[Int]]
                      }
                    }
                  case other => other
                }
            }
            item.value = convertedValue
          } catch {
            case e: Exception =>
              warn(s"Error parsing ${item.name}: ${e.getMessage}")
          }
        case _ =>
      }
    }
  }


  def extractAndRemove2DMatrices(raw: String, dataItems: List[DataItem]): String = {
    val pattern = new Regex("(?s)([a-zA-Z0-9_]+)\\s*=\\s*\\[\\|(.*?)\\|\\]\\s*;")

    var processedData = raw

    pattern.findAllMatchIn(raw).foreach { m =>
      val name = m.group(1)
      val content = m.group(2)

      dataItems.find(_.name == name).foreach { item =>
        try {
          val rows = content.split("\\|").map(_.trim).filter(_.nonEmpty)

          if (rows.nonEmpty) {
            val matrix = rows.map { rowStr =>
              rowStr.split(",")
                .map(_.trim)
                .filter(_.nonEmpty)
                .map(_.toInt)
                .toList
            }.toList

            info(s"Manually parsed 2D Matrix '$name' with dimensions ${matrix.length}x${matrix.headOption.map(_.length).getOrElse(0)}")
            item.value = matrix
          }
        } catch {
          case e: Exception =>
            error(s"Failed to manual parse matrix $name: ${e.getMessage}")
        }
      }
    }

    pattern.replaceAllIn(raw, "")
  }

  def parseDzn(data: String, dataItems: List[DataItem]): Unit = {
    try {
      val cleanData = extractAndRemove2DMatrices(data, dataItems)

      val charStream = CharStreams.fromString(cleanData)
      val lexer = new NewMinizincLexer(charStream)
      val tokens = new CommonTokenStream(lexer)
      val parser = new NewMinizincParser(tokens)

      val tree = parser.model()

      val visitor = new MinizincDznVisitor(dataItems)

      tree.item().asScala.foreach { itemCtx =>
        if (itemCtx.assign_item() != null) {
          visitor.visitAssign_item(itemCtx.assign_item())
        }
      }

      dataItems.filter(_.value != None).foreach { item =>
        val valStr = item.value match {
          case l: List[_] if l.size > 10 => s"List (size=${l.size})"
          case other => other.toString
        }
        info(s"Parsed DZN value for ${item.name}: $valStr")
      }

    } catch {
      case e: Exception =>
        error(s"Error parsing DZN file: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def tryAutoDetectType(values: Array[String], isArray: Boolean): Any = {
    debug(values.head)
    if (isArray) {
      val intList = Try(values.map(_.toInt).toList).toOption
      if (intList.isDefined) return intList.get

      val doubleList = Try(values.map(_.toDouble).toList).toOption
      if (doubleList.isDefined) return doubleList.get

      val boolList = Try(values.map(_.toLowerCase match {
        case "true" | "1" | "yes"  => true
        case "false" | "0" | "no"  => false
        case _ => throw new IllegalArgumentException("Not a boolean")
      }).toList).toOption
      debug(boolList.toString)
      if (boolList.isDefined) return boolList.get

      return values.toList
    } else {
      val value = values.head

      Try(value.toInt).toOption.foreach(v => return v)
      Try(value.toDouble).toOption.foreach(v => return v)
      Try(value.toLowerCase match {
        case "true" | "1" | "yes" => true
        case "false" | "0" | "no" => false
        case _ => throw new IllegalArgumentException("Not a boolean")
      }).toOption.foreach(v => return v)

      value
    }
  }

  def parseCsv(data: String, dataItems: List[DataItem], parseVars: Boolean = false): Unit = {
    val lines = data.split("\n").map(_.trim).filter(_.nonEmpty)
    if (lines.isEmpty) {
      error("CSV file is empty")
      return
    }

    val headers = lines.head.split(";").map(_.trim)
    val dataRows = lines.tail

    val targetItems = if (parseVars) dataItems.filter(_.isVar) else dataItems.filter(!_.isVar)

    targetItems.foreach { item =>
      headers.zipWithIndex.find(_._1 == item.name) match {
        case Some((_, colIdx)) =>
          try {
            val columnValues = dataRows.map(_.split(";").apply(colIdx).trim)

            val isArray = if (item.detailedDataType != null) item.detailedDataType.isArray else true

            val convertedValue: Any = if (item.detailedDataType != null) {
              item.detailedDataType.dataType match {
                case "int" =>
                  val lst = columnValues.map(_.toInt).toList
                  if (isArray) lst else lst.head
                case "float" =>
                  val lst = columnValues.map(_.toDouble).toList
                  if (isArray) lst else lst.head
                case "bool" =>
                  val lst = columnValues.map {
                    case "true" | "1" | "yes" => true
                    case "false" | "0" | "no" => false
                    case other => throw new IllegalArgumentException(s"Cannot convert '$other' to boolean")
                  }.toList
                  if (isArray) lst else lst.head
                case "string" =>
                  val lst = columnValues.toList
                  if (isArray) lst else lst.head
                case otherType =>
                  warn(s"Unknown data type $otherType for item ${item.name}, attempting auto-detection")
                  tryAutoDetectType(columnValues, isArray = isArray)
              }
            } else {
              warn(s"Missing detailedDataType for item ${item.name} in CSV, attempting auto-detection")
              tryAutoDetectType(columnValues, isArray = isArray)
            }

            item.value = convertedValue

            info(s"Parsed CSV data for ${item.name}: ${item.value}")

          } catch {
            case e: Exception =>
              warn(s"Error parsing CSV data for ${item.name}: ${e.getMessage}")
          }

        case None =>
          warn(s"No column found for key: ${item.name}")
      }
    }
  }

  def importDataFile(filename: String, dataItems: List[DataItem], importVar: Boolean = false): Unit = {
    val dataContent: String = Source.fromFile(filename).mkString
    debug(filename)
    filename.trim.split("\\.").last match {
      case "json" => parseJson(dataContent, dataItems, importVar)
      case "dzn" => parseDzn(dataContent, dataItems)
      case "csv" => parseCsv(dataContent, dataItems, importVar)
      case ext => warn(s"Unsupported file extension: $ext")
    }
    prepareSets(dataContent, dataItems)
  }
}