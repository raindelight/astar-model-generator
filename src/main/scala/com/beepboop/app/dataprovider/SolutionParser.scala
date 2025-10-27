package com.beepboop.app.dataprovider

import com.beepboop.app.logger.LogTrait

import scala.io.Source
import scala.util.Try

object SolutionParser extends LogTrait {

  def parse(filePath: String, separator: Char = ';'): List[Map[String, Any]] = {
    Try {
      val lines = Source.fromFile(filePath).getLines().filter(_.trim.nonEmpty).toList

      lines match {
        case headerLine :: dataLines if dataLines.nonEmpty =>
          val headers = headerLine.split(separator).map(_.trim)

          dataLines.flatMap { line =>
            val values = line.split(separator).map(_.trim)
            if (headers.length == values.length) {
              val typedMap = headers.zip(values).map { case (header, valueStr) =>
                header -> parseValue(valueStr)
              }.toMap
              Some(typedMap)
            } else {
              warn(s"Skipping row due to mismatched column count: $line")
              None
            }
          }
        case _ =>
          warn("Solutions file is empty or contains only a header.")
          List.empty
      }
    }.getOrElse {
      error(s"An error occurred while reading the file: $filePath")
      List.empty
    }
  }

  private def parseValue(value: String): Any = {
    if (value.startsWith("[") && value.endsWith("]")) {
      Try {
        value.stripPrefix("[").stripSuffix("]").split(',').map(_.trim.toInt).toList
      }.getOrElse(value)
    }
    else if (value.matches("""^-?\d+\.\d+$""")) {
      Try(value.toFloat).getOrElse(value)
    }
    else if (value.matches("""^-?\d+$""")) {
      Try(value.toInt).getOrElse(value)
    }
    else {
      value
    }
  }
}