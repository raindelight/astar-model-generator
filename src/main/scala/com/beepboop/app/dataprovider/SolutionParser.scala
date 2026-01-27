package com.beepboop.app.dataprovider
import com.beepboop.app.dataprovider.{DataItem, DataType}
import com.beepboop.app.logger.LogTrait

import scala.io.Source
import scala.util.Using
import scala.collection.mutable.ListBuffer

object SolutionParser extends LogTrait {

  def parse(filePath: String, separator: Char = ';', vars: List[DataItem]): List[Map[String, Any]] = {
    var source: Source = null
    try {
      source = Source.fromFile(filePath)
      val lines = source.getLines().filter(_.trim.nonEmpty).toList

      val schema: Map[String, DataType] = vars
        .filter(_.name != null)
        .map(v => v.name -> v.detailedDataType)
        .toMap


      lines match {
        case headerLine :: dataLines if dataLines.nonEmpty =>
          val headers = headerLine.split(separator).map(_.trim)

          dataLines.flatMap { line =>
            val values = line.split(separator).map(_.trim)

            if (headers.length == values.length) {
              val typedMap = headers.zip(values).map { case (header, valueStr) =>
                val dataTypeOption = schema.get(header).flatMap(Option(_))
                val parsedValue = dataTypeOption match {
                  case Some(dataType) => parseValue(valueStr, dataType)
                  case None =>
                    if (valueStr.startsWith("[") && valueStr.endsWith("]")) {
                      parseComplexArray(valueStr, "unknown")
                    } else {
                      parseScalar(valueStr, "unknown")
                    }
                }
                header -> parsedValue
              }.toMap
              Some(typedMap)
            } else {
              warn(s"Skipping row due to mismatched column count. Expected ${headers.length}, got ${values.length}: $line")
              None
            }
          }
        case _ =>
          warn("Solutions file is empty or contains only a header.")
          List.empty
      }
    } catch {
      case e: Exception =>
        error(s"reading solutions file [$filePath]: ${e.getMessage}")
        e.printStackTrace()
        List.empty
    } finally {
      if (source != null) source.close()
    }
  }

  private def parseValue(valueStr: String, dataType: DataType): Any = {
    if (dataType == null) return valueStr

    try {
      if (dataType.isArray) {
        parseComplexArray(valueStr, dataType.dataType)
      } else {
        parseScalar(valueStr, dataType.dataType)
      }
    } catch {
      case e: Exception =>
        warn(s"Failed to parse '$valueStr' as ${dataType.dataType}: ${e.getMessage}")
        valueStr
    }
  }

  private def parseComplexArray(arrayStr: String, typeName: String): List[Any] = {
    val cleanStr = arrayStr.trim.stripPrefix("[").stripSuffix("]")
    if (cleanStr.isEmpty) return List.empty

    val elements = splitIgnoringBraces(cleanStr, ',')

    elements.map { rawElem =>
      val elem = rawElem.trim
      if (elem.startsWith("{")) {
        parseSet(elem)
      } else if (elem.contains("..")) {
        parseRangeToSet(elem)
      } else {
        parseScalar(elem, typeName)
      }
    }


  }

  private def parseSet(setStr: String): List[Int] = {
    val inner = setStr.stripPrefix("{").stripSuffix("}").trim
    if (inner.isEmpty) List.empty[Int]
    else {
      inner.split(',').map(_.trim).flatMap { part =>
        if (part.contains("..")) parseRangeToSet(part)
        else scala.util.Try(part.toInt).toOption
      }.toList
    }
  }

  private def parseRangeToSet(rangeStr: String): List[Int] = {
    val parts = rangeStr.split("\\.\\.")
    if (parts.length == 2) {
      val start = parts(0).toInt
      val end = parts(1).toInt
      (start to end).toList
    } else {
      List.empty
    }
  }

  private def splitIgnoringBraces(str: String, separator: Char): List[String] = {
    val result = ListBuffer[String]()
    var current = new StringBuilder
    var braceDepth = 0

    for (c <- str) {
      c match {
        case '{' =>
          braceDepth += 1
          current += c
        case '}' =>
          braceDepth -= 1
          current += c
        case `separator` if braceDepth == 0 =>
          result += current.toString()
          current = new StringBuilder
        case _ =>
          current += c
      }
    }
    if (current.nonEmpty) result += current.toString()
    result.toList
  }

  private def parseScalar(value: String, typeName: String): Any = {
    val lowerType = Option(typeName).getOrElse("string").toLowerCase
    val cleanValue = value.trim

    if (lowerType.contains("bool")) {
      cleanValue.toLowerCase match {
        case "true" | "1" | "yes" => true
        case "false" | "0" | "no" => false
        case _ => cleanValue.toBooleanOption.getOrElse(cleanValue == "1")
      }
    }
    else if (lowerType.contains("float") || lowerType.contains("decimal")) {
      cleanValue.toDouble
    }
    else if (lowerType.contains("int") || lowerType.matches(".*\\d+\\.\\.\\d+.*") || lowerType.contains("var")) {
      scala.util.Try(cleanValue.toInt).getOrElse(cleanValue)
    }
    else {
      scala.util.Try(cleanValue.toInt)
        .orElse(scala.util.Try(cleanValue.toDouble))
        .getOrElse(cleanValue)
    }
  }
}