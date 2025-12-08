package com.beepboop.app.logger

import scala.collection.mutable
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

object Profiler {

  private val timings = new ConcurrentHashMap[String, mutable.ListBuffer[Long]]().asScala
  private val values = new ConcurrentHashMap[String, mutable.ListBuffer[Long]]().asScala

  /**
   * profiles the execution time of a block
   * @param name name of the block
   * @param block executable block, scope
   * @tparam T return type of the block
   * @example  profile("test") { long_func() }
   */
  def profile[T](name: String)(block: => T): T = {
    val startTime = System.nanoTime()

    val result = try {
      block
    } finally {
      val endTime = System.nanoTime()
      val duration = endTime - startTime

      val durations = timings.getOrElseUpdate(name, mutable.ListBuffer.empty[Long])

      durations.synchronized {
        durations += duration
      }
    }

    result
  }

  /**
   * Sums given value to the variable with the name
   * @param name name of the variable
   * @param value value to add
   */
  def recordValue(name: String, value: Long): Unit = {
    val list = values.getOrElseUpdate(name, mutable.ListBuffer.empty[Long])
    list.synchronized {
      list += value
    }
  }


  def reset(): Unit = {
    timings.clear()
    values.clear()
  }

  def report(): Unit = {
    val headerFmt = "| %-50s | %8s | %14s | %14s | %14s | %14s |"
    val rowFmt    = "| %-50s | %8d | %14.2f | %14.2f | %14.2f | %14.2f |"
    val separator = "|%s|%s|%s|%s|%s|%s|"
      .format("-" * 52, "-" * 10, "-" * 16, "-" * 16, "-" * 16, "-" * 16)

    def printTable(
                    title: String,
                    data: mutable.Map[String, mutable.ListBuffer[Long]],
                    unitConverter: Double => Double,
                    unitName: String
                  ): Unit = {
      println(s"\n--- $title ---")
      println(headerFmt.format("Name", "Calls", s"Total ($unitName)", s"Avg ($unitName)", s"Min ($unitName)", s"Max ($unitName)"))
      println(separator)

      if (data.isEmpty) {
        println(s"| No $title recorded.".padTo(129, ' ') + "|")
        println(separator)
        return
      }

      val sortedData = data.toSeq.sortBy(_._1)

      for ((name, records) <- sortedData) {
        val (count, total, minVal, maxVal) = records.synchronized {
          if (records.isEmpty) (0, 0L, 0L, 0L)
          else (records.length, records.sum, records.min, records.max)
        }

        if (count > 0) {
          val avg = total.toDouble / count
          println(rowFmt.format(
            name,
            count,
            unitConverter(total.toDouble),
            unitConverter(avg),
            unitConverter(minVal.toDouble),
            unitConverter(maxVal.toDouble)
          ))
        }
      }
      println(separator)
    }

    printTable("Profiler Timings", timings, ns => ns / 1_000_000.0, "ms")

    printTable("Custom Metrics", values, v => v, "val")

    println()
  }
}