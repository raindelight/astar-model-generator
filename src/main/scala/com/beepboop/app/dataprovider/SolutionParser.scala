package com.beepboop.app.dataprovider

import com.beepboop.app.dataprovider.{DataItem, DataType}
import com.beepboop.app.logger.LogTrait

import scala.io.Source
import scala.util.{Failure, Success, Try}

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
                  case None           => valueStr
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
        parseArray(valueStr, dataType.dataType)
      } else {
        parseScalar(valueStr, dataType.dataType)
      }
    } catch {
      case e: Exception =>
        warn(s"Failed to parse '$valueStr' as ${dataType.dataType}: ${e.getMessage}")
        valueStr
    }
  }

  private def parseArray(arrayStr: String, typeName: String): List[Any] = {
    val cleanStr = arrayStr.trim.stripPrefix("[").stripSuffix("]")
    if (cleanStr.isEmpty) {
      List.empty
    } else {
      cleanStr.split(',')
        .map(_.trim)
        .map(elem => parseScalar(elem, typeName))
        .toList
    }
  }

  private def parseScalar(value: String, typeName: String): Any = {
    val lowerType = Option(typeName).getOrElse("string").toLowerCase

    if (lowerType.contains("bool")) {
      value.toLowerCase match {
        case "true" | "1" | "yes" => true
        case "false" | "0" | "no" => false
        case _ => value.toBooleanOption.getOrElse(value == "1")
      }
    }
    else if (lowerType.contains("float") || lowerType.contains("decimal")) {
      value.toDouble
    }
    else if (lowerType.contains("int") || lowerType.matches(".*\\d+\\.\\.\\d+.*") || lowerType.contains("var")) {
      value.toInt
    }
    else {
      value
    }
  }
}