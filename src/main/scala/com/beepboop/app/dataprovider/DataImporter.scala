package com.beepboop.app.dataprovider


import scala.io.Source
import scala.util.{Failure, Success, Try}
import scala.language.postfixOps
import spray.json.*
import DefaultJsonProtocol.*
import com.beepboop.app.logger.LogTrait

object DataImporter extends DefaultJsonProtocol, LogTrait {
  def parseJson(data: String, dataItems: List[DataItem], parseVars: Boolean = false): Unit = {
    val jsonObject = data.parseJson.asJsObject
    if (parseVars) {
      dataItems.filter(_.isVar).foreach { item =>
        (jsonObject.fields.get(item.name), item.detailedDataType) match {
          case (Some(jsValue), details) if details != null =>
            try {
              val convertedValue: Any = (details.isArray, details.dataType) match {
                case (true, "int") =>
                  Try(jsValue.convertTo[List[List[Int]]]).getOrElse(jsValue.convertTo[List[Int]])
                case (true, "float") =>
                  Try(jsValue.convertTo[List[List[Double]]]).getOrElse(jsValue.convertTo[List[Double]])
                case (true, "bool") =>
                  Try(jsValue.convertTo[List[List[Boolean]]]).getOrElse(jsValue.convertTo[List[Boolean]])
                case (true, "string") => jsValue.convertTo[List[String]]
                case (false, "int") => jsValue.convertTo[Int]
                case (false, "float") => jsValue.convertTo[Double]
                case (false, "bool") => jsValue.convertTo[Boolean]
                case (false, "string") => jsValue.convertTo[String]
                case (_, unknownType) =>
                  warn(s"Unsupported data type: $unknownType for item ${item.name}")
                  None
              }
              item.value = convertedValue
              info(s"Successfully parsed data for ${item.name}: ${item.value}")
            } catch {
              case e: DeserializationException =>
                error(s"Error converting data for ${item.name}: ${e.getMessage}")
            }

          case (None, _) =>
            warn(s"No data found for key: ${item.name}")

          case (Some(_), null) =>
            warn(s"No detailed data type found for item: ${item.name}")
        }
      }


    } else {
      dataItems.filter(!_.isVar).foreach { item =>
        (jsonObject.fields.get(item.name), item.detailedDataType) match {
          case (Some(jsValue), details) if details != null =>
            try {
              val convertedValue: Any = (details.isArray, details.dataType) match {
                case (true, "int") => jsValue.convertTo[List[Int]]
                case (true, "float") => jsValue.convertTo[List[Double]]
                case (true, "bool") => jsValue.convertTo[List[Boolean]]
                case (true, "string") => jsValue.convertTo[List[String]]
                case (false, "int") => jsValue.convertTo[Int]
                case (false, "float") => jsValue.convertTo[Double]
                case (false, "bool") => jsValue.convertTo[Boolean]
                case (false, "string") => jsValue.convertTo[String]
                case (_, unknownType) =>
                  warn(s"Unsupported data type: $unknownType for item ${item.name}") // todo: add deduction based on previously defined var
                  None
              }
              item.value = convertedValue
              info(s"Successfully parsed data for ${item.name}: ${item.value}")
            } catch {
              case e: DeserializationException =>
                warn(s"Error converting data for ${item.name}: ${e.getMessage}")
            }

          case (None, _) =>
            warn(s"No data found for key: ${item.name}")

          case (Some(_), null) =>
            warn(s"No detailed data type found for item: ${item.name}")
        }
      }

    }
  }
  def parseDzn(data: String, dataItems: List[DataItem]): Unit = {
    return

  }


  def tryAutoDetectType(values: Array[String], isArray: Boolean): Any = {
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

            val convertedValues: List[Any] = if (item.detailedDataType != null) {
              item.detailedDataType.dataType match {
                case "int" =>
                  columnValues.map(_.toInt).toList
                case "float" =>
                  columnValues.map(_.toDouble).toList
                case "bool" =>
                  columnValues.map {
                    case "true" | "1" | "yes"  => true
                    case "false" | "0" | "no"  => false
                    case other => throw new IllegalArgumentException(s"Cannot convert '$other' to boolean")
                  }.toList
                case "string" =>
                  columnValues.toList
                case otherType =>
                  warn(s"Unknown data type $otherType for item ${item.name}, attempting auto-detection")
                  tryAutoDetectType(columnValues, isArray = true).asInstanceOf[List[Any]]
              }
            } else {
              warn(s"Missing detailedDataType for item ${item.name} in CSV, attempting auto-detection")
              tryAutoDetectType(columnValues, isArray = true).asInstanceOf[List[Any]]
            }

            item.value = convertedValues

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
  }
}