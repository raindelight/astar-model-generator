package com.beepboop.app.logger

import scala.collection.mutable
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

object Profiler {

  private val timings = new ConcurrentHashMap[String, mutable.ListBuffer[Long]]().asScala

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

  def reset(): Unit = {
    timings.clear()
  }

  def report(): Unit = {
    val headerFmt = "| %-50s | %8s | %14s | %14s | %14s | %14s |"
    val rowFmt    = "| %-50s | %8d | %14.2f | %14.2f | %14.2f | %14.2f |"
    val separator = "|%s|%s|%s|%s|%s|%s|"
      .format("-" * 52, "-" * 10, "-" * 16, "-" * 16, "-" * 16, "-" * 16)

    def toMs(nano: Double): Double = nano / 1_000_000.0

    println("\n--- 📊 Profiler Report ---")
    println(headerFmt.format("Function Name", "Calls", "Total (ms)", "Average (ms)", "Min (ms)", "Max (ms)"))
    println(separator)

    if (timings.isEmpty) {
      println("| No data recorded.                                                                                      |")
    }

    val sortedTimings = timings.toSeq.sortBy(_._1)

    for ((name, durations) <- sortedTimings) {
      val (count, totalNanos, minNanos, maxNanos) = durations.synchronized {
        if (durations.isEmpty) (0, 0L, 0L, 0L)
        else (durations.length, durations.sum, durations.min, durations.max)
      }

      if (count > 0) {
        val avgNanos = totalNanos.toDouble / count
        println(rowFmt.format(
          name,
          count,
          toMs(totalNanos),
          toMs(avgNanos),
          toMs(minNanos),
          toMs(maxNanos)
        ))
      }
    }
    println(separator)
    println()
  }
}