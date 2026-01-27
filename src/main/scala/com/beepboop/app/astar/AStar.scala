package com.beepboop.app.astar

import com.beepboop.app.*
import com.beepboop.app.components.{BinaryExpression, BoolType, Expression}
import com.beepboop.app.dataprovider.{DataItem, DataProvider}
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.logger.Profiler
import com.beepboop.app.mutations.{AllMutations, MutationEngine}
import com.beepboop.parser.{ModelConstraintGrammarLexer, ModelConstraintGrammarParser}
import com.typesafe.scalalogging.LazyLogging
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

import scala.collection.parallel.CollectionConverters.RangeIsParallelizable
import com.beepboop.app.dataprovider.{AStarSnapshot, PersistenceManager}
import com.beepboop.app.policy.{Compliant, DenyDiffnInsideQuantifier, DenyDivByZero, EnsureAnyVarExists, MaxDepth, NonCompliant, Scanner}
import com.beepboop.app.postprocessor.Postprocessor

import scala.collection.mutable
import scala.reflect.ClassTag
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

  override def toString: String = {
    s"[f=$f, g=$g, h=$h] ${constraint.toString}"
  }

  override def equals(obj: Any): Boolean = obj match {
    case that: ModelNodeTMP => this.constraint == that.constraint
    case _ => false
  }

  override def hashCode(): Int = constraint.hashCode()
}

case class HeuristicStats(
                           satisfiedCount: Int,
                           totalNormalizedDistance: Double,
                           minNormalizedDistance: Double,
                           maxNormalizedDistance: Double,
                           sumSquaredNormalizedDistance: Double,
                           numSolutions: Int
                         )

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


class AStar(grammar: ParsedGrammar, heuristicMode: String = "avg") extends LogTrait {
  private val numSolutions = DataProvider.solutionCount
  private val mutationEngine = new MutationEngine(AllMutations.mutations)

  private val openSet = mutable.PriorityQueue[ModelNodeTMP]()(ModelNodeTMPOrdering)
  private val visited = mutable.Map[Expression[?], ModelNodeTMP]()
  private var isInitialized = false

  def getSnapshot: AStarSnapshot = {
    val visitedNodes = visited.values.toSet
    AStarSnapshot(openSet.toList, visitedNodes)
  }

  def restoreState(snapshot: AStarSnapshot): Unit = {
    this.openSet.clear()
    this.visited.clear()
    snapshot.openSetItems.foreach(item => this.openSet.enqueue(item))

    snapshot.visitedItems.foreach(node => visited(node.constraint) = node)

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
    require(initialConstraint.signature.output == BoolType,
      s"Initial constraint must return BoolType, but got ${initialConstraint.signature.output}")
    val gScore = mutable.Map[Expression[?], Int]()

    if (!isInitialized) {
      openSet.clear()
      visited.clear()

      val initial_h = calculateHeuristic(initialConstraint)
      val startNode = ModelNodeTMP(initialConstraint, 1, initial_h)

      gScore(initialConstraint) = 1
      openSet.enqueue(startNode)

      warn(s"Start Node: g=${startNode.g}, h=${startNode.h}, f=${startNode.f}")
    } else {
      warn("Resuming search from loaded state...")
      visited.values.foreach(n => gScore(n.constraint) = n.g)
      openSet.iterator.foreach(n => {
        val currentBest = gScore.getOrElse(n.constraint, Int.MaxValue)
        if (n.g < currentBest) gScore(n.constraint) = n.g
      })
    }

    var iterations = 0

    while (openSet.nonEmpty && iterations < maxIterations) {

      if (iterations % saveInterval == 0 && iterations > 0) {
        PersistenceManager.saveCheckpoint(getSnapshot, checkpointFile, outputCsvFile)
        warn(s"Checkpoint saved at iteration $iterations.")
      }

      iterations += 1

      val currentNode = openSet.dequeue()

      val bestG = gScore.getOrElse(currentNode.constraint, Int.MaxValue)
      if (currentNode.g > bestG) {
        debug(s"Skipping stale node (g=${currentNode.g} > best=$bestG): ${currentNode.constraint}")
      } else {

        info(s"Processing Iteration $iterations | Queue: ${openSet.size} | Visited: ${visited.size}")
        info(s"Dequeued: $currentNode")

        visited(currentNode.constraint) = currentNode

        val generated = generateNeighbors(currentNode)
        info(s"Generated neighbors: ${generated.size}")

        generated.foreach { neighborConstraint =>
          val isStructurallyIdentical = neighborConstraint.structuralSignature == currentNode.constraint.structuralSignature

          val boredomPenalty = if (isStructurallyIdentical) 10 else 0

          val complexityPenalty = neighborConstraint.complexity
          val tentativeG = currentNode.g + 1 + (complexityPenalty * 1) + (boredomPenalty * 8)
          val existingG = gScore.getOrElse(neighborConstraint, Int.MaxValue)

          if (tentativeG < existingG) {

            gScore(neighborConstraint) = tentativeG

            val h = calculateHeuristic(neighborConstraint)
            val neighborNode = ModelNodeTMP(neighborConstraint, tentativeG, h)

            openSet.enqueue(neighborNode)
          }
        }
      }
    }

    info("Saving final state checkpoint...")
    PersistenceManager.saveCheckpoint(getSnapshot, checkpointFile, outputCsvFile)

    if (iterations >= maxIterations) {
      warn(s"Search finished after $iterations iterations (Max Limit Reached).")
    } else {
      warn(s"Search finished because Queue is empty.")
    }

    isInitialized = false
    val resultNodes = scala.collection.mutable.Set.from(visited.values)
    Some(resultNodes)
  }
  private def calculateHeuristic(constraint: Expression[?]): Int = Profiler.profile("calculateHeuristic") {
    if (numSolutions == 0) return Int.MaxValue

    val resultsTry = scala.util.Try {
      (0 until numSolutions).par.map { i =>
        val context = DataProvider.getSolutionContext(i)
        val isSatisfied = try {
          constraint.eval(context).asInstanceOf[Boolean]
        } catch {
          case e: IllegalArgumentException =>
            Profiler.recordValue(s"Discarded due to IllegalArgumentException: $e", 1)
            throw e
          case e: ClassCastException =>
            Profiler.recordValue(s"ClassCastExceptions: $e", 1)
            false
          case e: IndexOutOfBoundsException =>
            false
          case e: Exception =>
            Profiler.recordValue(s"UnknownException: $e", 1)
            false
        }

        val rawDist = constraint.distance(context)
        val normDist = rawDist.toDouble / (1.0 + rawDist.toDouble)
        val currentSatisfaction = if (isSatisfied) 1 else 0

        (currentSatisfaction, normDist, normDist, normDist, normDist * normDist)
      }
    }

    resultsTry match {

      case scala.util.Failure(_: IllegalArgumentException) =>
        return Int.MaxValue / 2
      case scala.util.Failure(_: IndexOutOfBoundsException) =>
        return 500
      case scala.util.Failure(e) =>
        //warn(s"Heuristic evaluation failed: ${e.getMessage}")
        return Int.MaxValue / 2

      case scala.util.Success(results) =>
        val (satisfiedCount, totalNormDist, minNormDist, maxNormDist, sumSqNormDist) =
          results.fold((0, 0.0, 1.0, 0.0, 0.0)) { (acc, elem) =>
            (
              acc._1 + elem._1,
              acc._2 + elem._2,
              math.min(acc._3, elem._3),
              math.max(acc._4, elem._4),
              acc._5 + elem._5
            )
          }

        val noiseSamples = 200
        val noiseSatisfiedCount = (0 until noiseSamples).count { _ =>
          val noiseCtx = DataProvider.createRandomContext()
          try {
            constraint.eval(noiseCtx).asInstanceOf[Boolean]
          } catch {
            case _: IllegalArgumentException => false
            case _: Exception => false
          }
        }

        val noiseRate = noiseSatisfiedCount.toDouble / noiseSamples
        val entropyWeight = math.max(0.1, 1.0 - noiseRate)

        if (noiseRate > 0.95) {
          Profiler.recordValue("tautology_detected", 1)
          debug(s"Tautology Detected: $constraint (Noise Rate: $noiseRate)")
          return Int.MaxValue / 2
        }

        val stats = HeuristicStats(satisfiedCount, totalNormDist, minNormDist, maxNormDist, sumSqNormDist, numSolutions)

        heuristicMode.toLowerCase match {
          case "min" => computeHeuristicScoreMinDist(stats)
          case "max" => computeHeuristicScoreMaxDist(stats)
          case "mse" => computeHeuristicScoreMSE(stats)
          case "var" => computeHeuristicScoreVariance(stats)
          case "avg" | _ => computeHeuristicScoreAverage(stats)
        }
    }
  }

  private def computeHeuristicScoreAverage(stats: HeuristicStats): Int = {
    val beta = 2.0
    val betaSq = beta * beta
    val SCALING_FACTOR = 1000

    val satisfactionRate = stats.satisfiedCount.toDouble / stats.numSolutions.toDouble
    val avgNormDist = stats.totalNormalizedDistance / stats.numSolutions.toDouble
    val closenessRate = 1.0 - avgNormDist

    if (satisfactionRate == 0.0 && closenessRate == 0.0) {
      return SCALING_FACTOR
    }

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val fScoreDenominator = (betaSq * closenessRate) + satisfactionRate

    val fScore = if (fScoreDenominator == 0) 0.0 else numerator / fScoreDenominator
    ((1.0 - fScore) * SCALING_FACTOR).toInt
  }

  private def computeHeuristicScoreMinDist(stats: HeuristicStats): Int = {
    val beta = 2.0
    val betaSq = beta * beta
    val SCALING_FACTOR = 1000

    val satisfactionRate = stats.satisfiedCount.toDouble / stats.numSolutions.toDouble

    val closenessRate = 1.0 - stats.minNormalizedDistance

    if (satisfactionRate == 0.0 && closenessRate == 0.0) {
      return SCALING_FACTOR
    }

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val fScoreDenominator = (betaSq * closenessRate) + satisfactionRate

    val fScore = if (fScoreDenominator == 0) 0.0 else numerator / fScoreDenominator
    ((1.0 - fScore) * SCALING_FACTOR).toInt
  }

  private def computeHeuristicScoreMaxDist(stats: HeuristicStats): Int = {
    val beta = 2.0
    val betaSq = beta * beta
    val SCALING_FACTOR = 1000

    val satisfactionRate = stats.satisfiedCount.toDouble / stats.numSolutions.toDouble

    val closenessRate = 1.0 - stats.maxNormalizedDistance

    if (satisfactionRate == 0.0 && closenessRate == 0.0) return SCALING_FACTOR

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val denominator = (betaSq * closenessRate) + satisfactionRate

    val fScore = if (denominator == 0) 0.0 else numerator / denominator
    ((1.0 - fScore) * SCALING_FACTOR).toInt
  }

  private def computeHeuristicScoreMSE(stats: HeuristicStats): Int = {
    val beta = 2.0
    val betaSq = beta * beta
    val SCALING_FACTOR = 1000

    val satisfactionRate = stats.satisfiedCount.toDouble / stats.numSolutions.toDouble

    val meanSquaredError = stats.sumSquaredNormalizedDistance / stats.numSolutions.toDouble
    val rootMeanSquaredError = math.sqrt(meanSquaredError)

    val closenessRate = 1.0 - rootMeanSquaredError

    if (satisfactionRate == 0.0 && closenessRate <= 0.0) return SCALING_FACTOR

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val denominator = (betaSq * closenessRate) + satisfactionRate

    val fScore = if (denominator == 0) 0.0 else numerator / denominator
    ((1.0 - fScore) * SCALING_FACTOR).toInt
  }

  private def computeHeuristicScoreVariance(stats: HeuristicStats): Int = {
    val beta = 2.0
    val betaSq = beta * beta
    val SCALING_FACTOR = 1000

    val satisfactionRate = stats.satisfiedCount.toDouble / stats.numSolutions.toDouble

    val mean = stats.totalNormalizedDistance / stats.numSolutions.toDouble
    val meanSq = stats.sumSquaredNormalizedDistance / stats.numSolutions.toDouble
    val variance = math.max(0.0, meanSq - (mean * mean))
    val stdDev = math.sqrt(variance)

    val lambda = 0.5
    val penalizedDist = math.min(1.0, mean + (lambda * stdDev))

    val closenessRate = 1.0 - penalizedDist

    if (satisfactionRate == 0.0 && closenessRate <= 0.0) return SCALING_FACTOR

    val numerator = (1.0 + betaSq) * (closenessRate * satisfactionRate)
    val denominator = (betaSq * closenessRate) + satisfactionRate

    val fScore = if (denominator == 0) 0.0 else numerator / denominator
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

    val neighbors = possibleMutations.flatMap {
      case (targetNode, mutationFunc, ctx, replacement) =>

        if (replacement.signature.output != targetNode.signature.output) {
          debug(s"Type Mismatch: ${targetNode.signature.output} vs ${replacement.signature.output}")
          None
        } else {
          val candidateTree = mutationEngine.replaceNodeInTree(constraint, targetNode, replacement)

          val simplifiedTree = candidateTree match {
            case expr: Expression[t] =>
              implicit val tag: ClassTag[t] = expr.ct
              Postprocessor.simplify(expr)
          }

          debug(s"Generated: $candidateTree to simplified $simplifiedTree")
          val result = Scanner.visitAll(simplifiedTree, EnsureAnyVarExists(), DenyDivByZero(), MaxDepth(9), DenyDiffnInsideQuantifier())

          if (result.isAllowed) {
            Profiler.recordValue("accepted", 1)
            Some(simplifiedTree)
          } else {
            debug(s"Expr: ${candidateTree.toString} - ${result.toString}")
            Profiler.recordValue("discarded", 1)
            result match {
              case nc: NonCompliant => Profiler.recordValue(nc.message, 1)
              case _ =>
            }
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