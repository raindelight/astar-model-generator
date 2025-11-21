package com.beepboop.app.astar

import com.beepboop.app.*
import com.beepboop.app.components.{BinaryExpression, Expression}
import com.beepboop.app.dataprovider.{DataItem, DataProvider}
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.mutations.{AllMutations, MutationEngine}
import com.beepboop.parser.{ModelConstraintGrammarLexer, ModelConstraintGrammarParser}
import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.mutable
import scala.util.Random
import scala.util.control.NonFatal


case class ModelNode(
                      constraints: List[Expression[?]],
                      g: Int,
                      h: Int,
                      parent: Option[ModelNode]
                    ) {
  val f: Int = g + h
}


case class ModelNodeTMP(
                       constraint: Expression[?],
                       g: Int,
                       h: Int,
                       ) {
  val f: Int = g + h
}

object ModelNodeOrdering extends Ordering[ModelNode] {
  def compare(a: ModelNode, b: ModelNode): Int = b.f.compare(a.f)
}

object ModelNodeTMPOrdering extends Ordering[ModelNodeTMP] {
  def compare(a: ModelNodeTMP, b: ModelNodeTMP): Int = b.f.compare(a.f)
}


case class DerivationNode(
                           sequence: List[GrammarSymbol],
                           g: Int,
                           h: Int,
                           parent: Option[DerivationNode]
                         ) {
  val f: Int = g + h
}

object DerivationNodeOrdering extends Ordering[DerivationNode] {
  def compare(a: DerivationNode, b: DerivationNode): Int = b.f.compare(a.f)
}


class AStar(grammar: ParsedGrammar) extends LogTrait {
  private val numSolutions = DataProvider.solutionCount
  private val mutationEngine = new MutationEngine(AllMutations.mutations)

  def findOptimalModel(
                        initialConstraint: Expression[?],
                        availableVars: List[DataItem],
                        dataPars: List[DataItem]
                      ): Option[mutable.Set[ModelNodeTMP]] = {

    val openSet = mutable.PriorityQueue[ModelNodeTMP]()(ModelNodeTMPOrdering)
    val visited = mutable.Set[ModelNodeTMP]()

    val initial_h = calculateHeuristic(initialConstraint)
    val startNode = ModelNodeTMP(initialConstraint, 1, initial_h)
    warn(s"Start Node: g=${startNode.g}, h=${startNode.h}, f=${startNode.f}")
    openSet.enqueue(startNode)

    var iterations = 0
    val maxIterations = 5000

    while (openSet.nonEmpty && iterations < maxIterations) {
      info(s"Iteration: $iterations. Queue items: ${openSet.size}. Visited: ${visited.size}")
      iterations += 1
      val currentNode = openSet.dequeue()
      info(s"Dequeuing node: $currentNode")

      visited.add(currentNode)
      debug(visited.toString)
      val generated = generateNeighbors(currentNode)
      info(s"Generated neighbors: ${generated.size}")
      generated.foreach { neighborConstraint =>
        if(!visited.map(m => m.constraint).contains(neighborConstraint)) {
          val h = calculateHeuristic(neighborConstraint)
          val neighbourNode = ModelNodeTMP(neighborConstraint, currentNode.g + 1, h)
          debug(s"Enqueuing node: $neighbourNode")
          openSet.enqueue(neighbourNode)
        }
      }
    }
    warn(s"Search finished after $iterations iterations without finding a solution.")
    Some(visited)
  }

  private def calculateHeuristic(constraint: Expression[?]): Int = {
    if (numSolutions == 0) return Int.MaxValue

    var satisfiedCount = 0
    var totalNormalizedDistance: Double = 0.0
    val beta = 2.0
    val betaSq = beta * beta

    val SCALING_FACTOR = 1000

    var i = 0
    while (i < numSolutions) {
      try {
        val context = DataProvider.createSolutionContext(i)
        val rawDist = constraint.distance(context).asInstanceOf[Int]

        if (rawDist == 0) {
          satisfiedCount += 1
        } else {
          totalNormalizedDistance += rawDist.toDouble / (1.0 + rawDist.toDouble)
        }
      } catch {
        case NonFatal(e) =>
          totalNormalizedDistance += 1.0
      }
      i += 1
    }

    val satisfactionRate = satisfiedCount.toDouble / numSolutions.toDouble
    val avgNormDist = totalNormalizedDistance / numSolutions.toDouble
    val closenessRate = 1.0 - avgNormDist

    if (satisfactionRate == 0.0 && closenessRate == 0.0) {
      return SCALING_FACTOR
    }

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val denominator = (betaSq * satisfactionRate) + closenessRate
    val fScoreDenominator = (betaSq * closenessRate) + satisfactionRate
    val fScore = if (fScoreDenominator == 0) 0.0 else numerator / fScoreDenominator

    ((1.0 - fScore) * SCALING_FACTOR).toInt
  }


  private def calculateGlobalHeuristic(constraints: List[Expression[?]], availableVars: List[DataItem], dataPars: List[DataItem]): Int = {
    val numSolutions = availableVars.headOption
      .flatMap(item => Option(item.value))
      .collect { case l: List[?] => l.size }
      .getOrElse(0)

    if (constraints.isEmpty) return numSolutions
    if (numSolutions == 0) return 0


    constraints.map(c =>
      val totalDistance = (0 until numSolutions).map { solutionIndex =>  {
          val context = DataProvider.createSolutionContext(solutionIndex)
          constraints.map(c => c.distance(context)).sum
        }
      }.min
      info(s"Max distance for expression ${c.toString}: $totalDistance")
    )
    return 0;

  }

  private def generateNeighbors(node: ModelNodeTMP): List[Expression[?]] = {
    val constraint = node.constraint

    val possibleMutations = mutationEngine.collectPossibleMutations(constraint)
    debug(possibleMutations.toString)
    val neighbors = possibleMutations.map((e, m) => mutationEngine.replaceNodeInTree(constraint, e, m(e).getOrElse(constraint)))
    debug(neighbors.toString)
    neighbors.toSet.toList
  }


  private def computeMinStepsHeuristic(): Map[String, Int] = {
    val costs = mutable.Map[String, Int]()
    grammar.rules.keys.foreach(name => costs(name) = Int.MaxValue)

    var changed = true
    while (changed) {
      changed = false
      for ((ruleName, productionRule) <- grammar.rules) {
        val minAltCostOption = productionRule.alternatives.map { alt =>
          alt.symbols.collect {
            case nt: NonTerminal => costs.getOrElse(nt.name, Int.MaxValue)
          }.foldLeft(0L)(_ + _).toInt
        }.minOption

        minAltCostOption.foreach { minAltCost =>
          if (minAltCost != Int.MaxValue) {
            val newCost = 1 + minAltCost
            if (newCost < costs.getOrElse(ruleName, Int.MaxValue)) {
              costs(ruleName) = newCost
              changed = true
            }
          }
        }
      }
    }
    costs.toMap
  }

  private def calculateDerivationH(sequence: List[GrammarSymbol], minStepsHeuristic: Map[String, Int]): Int = {
    sequence.collect { case nt: NonTerminal =>
      minStepsHeuristic.getOrElse(nt.name, Int.MaxValue)
    }.sum
  }
}