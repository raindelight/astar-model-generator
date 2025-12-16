package com.beepboop.app.cpicker

import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.components.Expression
import com.beepboop.app.dataprovider.PersistenceManager
import com.beepboop.app.utils.AppConfig
import scala.collection.parallel.CollectionConverters._

import scala.collection.mutable


import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._

case class ConstraintData(constraint: List[Expression[?]], heuristics: Double, sol_count: Long)

object ExpressionOrdering extends Ordering[Expression[?]] {
  def compare(x: Expression[?], y: Expression[?]): Int = x.toString.compareTo(y.toString)
}

object ConstraintPicker {
  var config: AppConfig = null

  def setConfig(config: AppConfig): Unit = {
    this.config = config
  }

  private def order(item: ConstraintData) = -item.sol_count

  var queue: mutable.PriorityQueue[ConstraintData] = mutable.PriorityQueue[ConstraintData]()(Ordering.by(order))

  def runInitial(nodes: mutable.Set[ModelNodeTMP], maxRounds: Int = 3, keepRatio: Double = 0.5): Unit = {
    val initialWorkload = nodes.par
    val validSingles = new mutable.ListBuffer[Expression[?]]()
    val round1Results = new mutable.ListBuffer[ConstraintData]()

    initialWorkload.foreach { tmpNode =>
      val expr = tmpNode.constraint
      val tmpFile = ConstraintSaver.save(expr)
      val runner = new Runner(this.config)
      val sol_count = runner.run(tmpFile)

      if (sol_count >= 500) {
        val data = ConstraintData(List(expr), tmpNode.f, sol_count)
        queue.synchronized { queue.enqueue(data) }
        validSingles.synchronized { validSingles += expr }
        round1Results.synchronized { round1Results += data }
      }
    }

    val baseExpressions = validSingles.toList
    var currentRoundGroups = round1Results
      .sortBy(_.sol_count)
      .take((round1Results.size * keepRatio).toInt.max(1))
      .map(_.constraint)
      .toList

    for (round <- 2 to maxRounds) {
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
          val sol_count = runner.run(tmpFile)

          if (sol_count >= 500) {
            val data = ConstraintData(group, 0.0, sol_count)
            queue.synchronized { queue.enqueue(data) }
            roundResults.synchronized { roundResults += data }
          }
        }

        currentRoundGroups = roundResults
          .sortBy(_.sol_count)
          .take((roundResults.size * keepRatio).toInt.max(1))
          .map(_.constraint)
          .toList
      }
    }

    PersistenceManager.saveConstraintsToCSV(queue.clone().dequeueAll, "queue.csv")
  }
}