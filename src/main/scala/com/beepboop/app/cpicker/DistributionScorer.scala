package com.beepboop.app.cpicker

import scala.math._

object DistributionScorer {
  val DefaultMean: Double = 20.0
  val DefaultStd: Double = 10.0

  def scoreNormal(length: Int, mean: Double = DefaultMean, stdDev: Double = DefaultStd): Double = {
    if (stdDev == 0) return if (length == mean) 1.0 else 0.0

    val variance = stdDev * stdDev
    exp(-pow(length - mean, 2) / (2 * variance))
  }
}