package com.beepboop.app.cpicker

import scala.math._

object DistributionScorer {
  val DefaultMean: Double = 8.0
  val DefaultStd: Double = 4.0

  private val ProblemStats = Map(
    "accap" -> (7.75, 3.77),
    "community" -> (8.60, 3.20),
    "concert" -> (7.00, 1.90),
    "hoist" -> (10.22, 7.73),
    "network" -> (5.80, 3.06),
    "efm" -> (5.80, 3.06)
  )

  def getParams(modelPath: String): (Double, Double) = {
    val lowerPath = modelPath.toLowerCase
    ProblemStats.find { case (key, _) => lowerPath.contains(key) }
      .map(_._2)
      .getOrElse((DefaultMean, DefaultStd))
  }

  def scoreNormal(symbolCount: Int, mean: Double = DefaultMean, stdDev: Double = DefaultStd): Double = {
    if (stdDev == 0) return if (symbolCount == mean) 1.0 else 0.0

    val variance = stdDev * stdDev
    exp(-pow(symbolCount - mean, 2) / (2 * variance))
  }
}