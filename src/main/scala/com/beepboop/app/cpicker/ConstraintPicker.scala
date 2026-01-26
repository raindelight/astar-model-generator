package com.beepboop.app.cpicker

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.components.Expression
import com.beepboop.app.dataprovider.PersistenceManager
import com.beepboop.app.logger.{LogTrait, Profiler}
import com.beepboop.app.utils.AppConfig
import com.beepboop.app.cpicker.DistributionScorer

import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable

case class ConstraintData(
                           constraint: List[Expression[?]],
                           heuristics: Double,
                           solCount: Long,
                           nestingDepth: Int,
                           symbolCount: Int,
                           distributionScore: Double
                         )

object ExpressionOrdering extends Ordering[Expression[?]] {
  def compare(x: Expression[?], y: Expression[?]): Int = x.toString.compareTo(y.toString)
}

object ConstraintPicker extends LogTrait {
  var config: AppConfig = AppConfig.get


  private def order(item: ConstraintData): Double = {
    item.solCount.toDouble * item.distributionScore
  }

  var queue: mutable.PriorityQueue[ConstraintData] = mutable.PriorityQueue[ConstraintData]()(Ordering.by(order))

  def runInitial(nodes: mutable.Set[ModelNodeTMP], maxRounds: Int = 3, keepRatio: Double = 0.5, threshold: Int = 500): Unit = {
    val (distMean, distStd) = DistributionScorer.getParams(config.modelPath)
    info(s"Distribution Params for '${config.modelPath}': Mean=$distMean, Std=$distStd")

    val initialWorkload = nodes.par
    val validSingles = new mutable.ListBuffer[Expression[?]]()
    val round1Results = new mutable.ListBuffer[ConstraintData]()

    info("Starting single evaluation using minizinc with Symbol Count Scoring")

    initialWorkload.foreach { tmpNode =>
      val expr = tmpNode.constraint

      val depth = expr.exprDepth
      val symbols = expr.symbolCount
      val distScore = DistributionScorer.scoreNormal(symbols, distMean, distStd)

      val tmpFile = ConstraintSaver.save(expr)
      val runner = new Runner(this.config)
      val solCount = runner.run(tmpFile)

      solCount match {
        case Some(value) if (value >= threshold) => {
          val data = ConstraintData(List(expr), tmpNode.f, value, depth, symbols, distScore)

          queue.synchronized {
            queue.enqueue(data)
          }
          validSingles.synchronized {
            validSingles += expr
          }
          round1Results.synchronized {
            round1Results += data
          }
        }
        case Some(value) => Profiler.recordValue("Discarded by threshold", 1)
        case None =>
      }
    }
    info(s"ValidSingles: ${validSingles.size}")

    val baseExpressions = validSingles.toList

    var currentRoundGroups = round1Results
      .sortBy(d => -d.distributionScore)
      .take((round1Results.size * keepRatio).toInt.max(1))
      .map(_.constraint)
      .toList

    for (round <- 2 to maxRounds) {
      info(s"Starting round $round with ${currentRoundGroups.size} groups")
      if (currentRoundGroups.nonEmpty) {
        val candidates = currentRoundGroups.flatMap { group =>
          baseExpressions.map { single =>
            if (group.contains(single)) group else (single :: group).sorted(ExpressionOrdering)
          }
        }.toSet.filter(_.size == round)

        val roundResults = new mutable.ListBuffer[ConstraintData]()
        val batchWorkload = candidates.toList.par

        batchWorkload.foreach { group =>

          val totalDepth = group.map(_.exprDepth).sum
          val totalSymbols = group.map(_.symbolCount).sum

          val avgDistScore = group.map(expr =>
            DistributionScorer.scoreNormal(expr.symbolCount, distMean, distStd)
          ).sum / group.size

          val tmpFile = ConstraintSaver.save(group: _*)
          val runner = new Runner(this.config)
          val solCount = runner.run(tmpFile)

          solCount match {
            case Some(value) if (value >= threshold) => {
              val data = ConstraintData(group, 0.0, value, totalDepth, totalSymbols, avgDistScore)
              queue.synchronized {
                queue.enqueue(data)
              }
              roundResults.synchronized {
                roundResults += data
              }
            }
            case Some(value) => Profiler.recordValue("Discarded by threshold", 1)
            case None =>
          }
        }

        currentRoundGroups = roundResults
          .sortBy(d => -d.distributionScore)
          .take((roundResults.size * Math.pow(keepRatio, round)).toInt.max(1))
          .map(_.constraint)
          .toList
        info(s"CurrentRoundGroups count ${currentRoundGroups.size}")
      }
    }

    PersistenceManager.saveConstraintsToCSV(queue.clone().dequeueAll, this.config.pickedOutputCsv)
  }
}