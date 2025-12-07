package com.beepboop.app.astar

import com.beepboop.app.*
import com.beepboop.app.components.{BinaryExpression, Expression}
import com.beepboop.app.dataprovider.{DataItem, DataProvider}
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.logger.Profiler
import com.beepboop.app.mutations.{AllMutations, MutationEngine}
import com.beepboop.parser.{ModelConstraintGrammarLexer, ModelConstraintGrammarParser}
import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import com.beepboop.app.dataprovider.{AStarSnapshot, PersistenceManager}
import com.beepboop.app.policy.{Compliant, EnsureVarExists, NonCompliant, Scanner}

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
                       ) extends Serializable {
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

  private val openSet = mutable.PriorityQueue[ModelNodeTMP]()(ModelNodeTMPOrdering)
  private val visited = mutable.Set[Expression[?]]()
  private var isInitialized = false

  def getSnapshot: AStarSnapshot = {
    val visitedNodes = visited.map(expr => ModelNodeTMP(expr, 0, 0)).toSet
    AStarSnapshot(openSet.toList, visitedNodes)
  }

  def restoreState(snapshot: AStarSnapshot): Unit = {
    this.openSet.clear()
    this.visited.clear()
    snapshot.openSetItems.foreach(item => this.openSet.enqueue(item))
    this.visited ++= snapshot.visitedItems.map(_.constraint)
    this.isInitialized = true
    warn(s"State restored! Queue: ${openSet.size}, Visited: ${visited.size}")
  }

  def findOptimalModel(
                        initialConstraint: Expression[?],
                        availableVars: List[DataItem],
                        dataPars: List[DataItem],
                        maxIterations: Int,
                        saveInterval: Int,
                        checkpointFile: String,
                        outputCsvFile: String
                      ): Option[mutable.Set[ModelNodeTMP]] = {

    if (!isInitialized) {
      openSet.clear()
      visited.clear()

      val initial_h = calculateHeuristic(initialConstraint)
      val startNode = ModelNodeTMP(initialConstraint, 1, initial_h)
      warn(s"Start Node: g=${startNode.g}, h=${startNode.h}, f=${startNode.f}")
      openSet.enqueue(startNode)
    }
    else {
      warn("Resuming search from loaded state...")
    }

    var iterations = 0

    while (openSet.nonEmpty && iterations < maxIterations) {
      if (iterations % saveInterval == 0 && iterations > 0) {
        PersistenceManager.saveCheckpoint(getSnapshot, checkpointFile, outputCsvFile)
        warn(s"Checkpoint saved at iteration $iterations.")
      }

      Some(visited)

      info(s"Iteration: $iterations. Queue items: ${openSet.size}. Visited: ${visited.size}")
      iterations += 1
      val currentNode = openSet.dequeue()
      info(s"Dequeuing node: $currentNode")

      visited.add(currentNode.constraint)
      debug(visited.toString)
      val generated = generateNeighbors(currentNode)
      info(s"Generated neighbors: ${generated.size}")
      generated.foreach { neighborConstraint =>
        if (!visited.contains(neighborConstraint)) {
          val h = calculateHeuristic(neighborConstraint)
          val neighbourNode = ModelNodeTMP(neighborConstraint, currentNode.g + 1, h)
          debug(s"Enqueuing node: $neighbourNode")
          openSet.enqueue(neighbourNode)
        }
      }
    }

    info("Saving final state checkpoint...")
    PersistenceManager.saveCheckpoint(getSnapshot, checkpointFile, outputCsvFile)
    info(s"Search finished after $iterations iterations.")
    isInitialized = false
    warn(s"Search finished after $iterations iterations without finding a solution.")
    val resultNodes = visited.map(c => ModelNodeTMP(c, 0, 0))
    Some(resultNodes)
  }

  private def calculateHeuristic(constraint: Expression[?]): Int = {

    Profiler.profile("calculateHeuristic") {
      if (numSolutions == 0) return Int.MaxValue

      val beta = 2.0
      val betaSq = beta * beta

      val SCALING_FACTOR = 1000

      val results = (0 until numSolutions).map { i => // Usuń '.par' dla czytelności logów debugowania
        try {
          val context = DataProvider.createSolutionContext(i)
          val rawDist = constraint.distance(context)

          // println(s"[DEBUG] Sol #$i | Constraint: $constraint | RawDist: $rawDist | Context: $context")

          if (rawDist == 0) (1, 0.0)
          else (0, rawDist.toDouble / (1.0 + rawDist.toDouble))
        } catch {
          case scala.util.control.NonFatal(e) => (0, 1.0)
        }
      }

      var i = 0
      val (satisfiedCount, totalNormalizedDistance) = (0 until numSolutions).par.map { i =>
        try {
          val context = DataProvider.createSolutionContext(i)
          val rawDist = constraint.distance(context)

          if (rawDist == 0) (1, 0.0)
          else (0, rawDist.toDouble / (1.0 + rawDist.toDouble))
        } catch {
          case scala.util.control.NonFatal(e) => (0, 1.0)
        }
      }.fold((0, 0.0)) { (acc, elem) =>
        (acc._1 + elem._1, acc._2 + elem._2)
      }

      val satisfactionRate = satisfiedCount.toDouble / numSolutions.toDouble
      val avgNormDist = totalNormalizedDistance / numSolutions.toDouble
      val closenessRate = 1.0 - avgNormDist

      if (satisfactionRate == 0.0 && closenessRate == 0.0) {
        return SCALING_FACTOR
      }

      val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
      //val denominator = (betaSq * satisfactionRate) + closenessRate
      val fScoreDenominator = (betaSq * closenessRate) + satisfactionRate
      val fScore = if (fScoreDenominator == 0) 0.0 else numerator / fScoreDenominator

      ((1.0 - fScore) * SCALING_FACTOR).toInt
    }
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
        val context = DataProvider.getSolutionContext(solutionIndex)
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
    val neighbors = possibleMutations.flatMap {
      case (targetNode, mutationFunc) =>
        mutationFunc(targetNode).flatMap { replacement =>
          val candidateTree = mutationEngine.replaceNodeInTree(constraint, targetNode, replacement)
          val result = Scanner.visitAll(candidateTree, EnsureVarExists())

          if (result.isAllowed) {
            Some(candidateTree)
          } else {
            debug(s"Expr: ${candidateTree.toString} - ${result.toString}")
            None
          }
        }
    }
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