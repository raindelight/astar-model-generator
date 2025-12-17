package com.beepboop.app.cpicker

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.components.Expression
import com.beepboop.app.dataprovider.PersistenceManager
import com.beepboop.app.logger.{LogTrait, Profiler}
import com.beepboop.app.utils.AppConfig

import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable

case class ConstraintData(constraint: List[Expression[?]], heuristics: Double, solCount: Long)

object ExpressionOrdering extends Ordering[Expression[?]] {
  def compare(x: Expression[?], y: Expression[?]): Int = x.toString.compareTo(y.toString)
}

object ConstraintPicker extends LogTrait {
  var config: AppConfig = null

  def setConfig(config: AppConfig): Unit = {
    this.config = config
  }

  private def order(item: ConstraintData) = -item.solCount

  var queue: mutable.PriorityQueue[ConstraintData] = mutable.PriorityQueue[ConstraintData]()(Ordering.by(order))

  def runInitial(nodes: mutable.Set[ModelNodeTMP], maxRounds: Int = 3, keepRatio: Double = 0.5, threshold: Int = 500): Unit = {
    val initialWorkload = nodes.par
    val validSingles = new mutable.ListBuffer[Expression[?]]()
    val round1Results = new mutable.ListBuffer[ConstraintData]()
    info("Starting single evaluation using minizinc")
    initialWorkload.foreach { tmpNode =>
      val expr = tmpNode.constraint
      val tmpFile = ConstraintSaver.save(expr)
      val runner = new Runner(this.config)
      val solCount = runner.run(tmpFile)
      solCount match {
        case Some(value) if (value >= threshold) => {
          val data = ConstraintData(List(expr), tmpNode.f, value)
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
      .sortBy(_.solCount)
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
          val tmpFile = ConstraintSaver.save(group: _*)
          val runner = new Runner(this.config)
          val solCount = runner.run(tmpFile)
          solCount match {
            case Some(value) if (value >= threshold) => {
              val data = ConstraintData(group, 0.0, value)
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
          .sortBy(_.solCount)
          .take((roundResults.size * Math.pow(keepRatio, round)).toInt.max(1))
          .map(_.constraint)
          .toList
        info(s"CurrentRoundGroups count ${currentRoundGroups.size}")
      }
    }

    PersistenceManager.saveConstraintsToCSV(queue.clone().dequeueAll, "queue.csv")
  }
}