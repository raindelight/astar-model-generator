package com.beepboop.app.experiment

import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}
import com.beepboop.app.astar.ModelNodeTMP
import com.beepboop.app.components.{ComposableExpression, Constant, Expression, OperatorContainer, Signature, Variable}
import com.beepboop.app.dataprovider.AStarSnapshot
import com.beepboop.app.logger.LogTrait


object UniversalExpressionMetric {
  def calculate(
                 found: Expression[?],
                 target: Expression[?],
                 policy: SimilarityPolicy = SimilarityPolicy(),
                 bindings: Map[String, String] = Map.empty
               ): Double = {

    if (found.getClass != target.getClass) return 0.0

    val operatorScore = (found, target) match {
      case (fOp: OperatorContainer, tOp: OperatorContainer) =>
        if (fOp.operator.getClass == tOp.operator.getClass) 1.0 else 0.2
      case _ => 1.0
    }

    val structuralScore = (found, target) match {
      case (f: ComposableExpression, t: ComposableExpression) =>
        val operatorName = found match {
          case oc: OperatorContainer => oc.operator.getClass.getSimpleName
          case _ => found.getClass.getSimpleName
        }

        val childSim = if (policy.swappableOperators.exists(operatorName.contains)) {
          calculateSwappableSimilarity(f.children, t.children, policy, bindings)
        } else {
          calculateOrderedSimilarity(f.children, t.children, policy, bindings)
        }
        childSim

      case (f: Variable[?], t: Variable[?]) =>
        val isBoundMatch = bindings.get(f.name).contains(t.name)
        if (isBoundMatch || f.name == t.name || policy.ignoreVariableNames) 1.0 else 0.5

      case _ => if (found.toString == target.toString) 1.0 else 0.0
    }

    (operatorScore * 0.2) + (structuralScore * 0.8)
  }

  private def calculateSwappableSimilarity(fChildren: List[Expression[?]], tChildren: List[Expression[?]], policy: SimilarityPolicy, bindings: Map[String, String]): Double = {
    if (fChildren.size != tChildren.size) return 0.0
    val bestTotal = fChildren.map { f =>
      tChildren.map(t => calculate(f, t, policy, bindings)).max
    }.sum
    bestTotal / fChildren.size
  }

  private def calculateOrderedSimilarity(fChildren: List[Expression[?]], tChildren: List[Expression[?]], policy: SimilarityPolicy, bindings: Map[String, String]): Double = {
    if (fChildren.size != tChildren.size) return 0.0
    val scores = fChildren.zip(tChildren).map { case (f, t) => calculate(f, t, policy, bindings) }
    scores.sum / scores.size
  }
}


case class ComparisonResult(
                             targetIndex: Int,
                             targetClassName: String,
                             similarityScore: Double
                           )

case class SnapshotMetric(
                           node: ModelNodeTMP,
                           nodeClassName: String,
                           bestMatch: ComparisonResult,
                           structuralRank: Double
                         )

case class NodeSimilarityRow(
                              nodeString: String,
                              nodeClass: String,
                              hValue: Int,
                              scores: Map[String, Double]
                            )

class MultiTargetMetricReporter(model: TargetModel) {
  private val policy = model.similarityPolicy

  def generateFullReport(snapshot: AStarSnapshot): List[NodeSimilarityRow] = {
    snapshot.visitedItems.map { node =>
      val nodeExpr = node.constraint

      val scoreMap = model.targetConstraints.zipWithIndex.map { case (target, idx) =>
        val targetLabel = s"${target.getClass.getSimpleName}_$idx"
        val score = UniversalExpressionMetric.calculate(nodeExpr, target, policy, Map.empty)
        targetLabel -> score
      }.toMap

      NodeSimilarityRow(
        nodeString = nodeExpr.toString,
        nodeClass = nodeExpr.getClass.getSimpleName,
        hValue = node.h,
        scores = scoreMap
      )
    }.toList
  }
}


object ModelLoader extends LogTrait {
  def loadModel(className: String): TargetModel = {
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val result = Try {
      val moduleSymbol = runtimeMirror.staticModule(className)
      runtimeMirror.reflectModule(moduleSymbol).instance.asInstanceOf[TargetModel]
    }.recoverWith {
      case _: Exception => Try {
        val classSymbol = runtimeMirror.staticClass(className)
        val classMirror = runtimeMirror.reflectClass(classSymbol)
        val constructor = classSymbol.primaryConstructor.asMethod
        classMirror.reflectConstructor(constructor)().asInstanceOf[TargetModel]
      }
    }

    result match {
      case Success(m) => m
      case Failure(e) => throw e
    }
  }
}

import java.io.{File, PrintWriter}

object SimilarityCSVExporter {
  def dumpToCSV(report: List[NodeSimilarityRow], targetLabels: List[String], filePath: String): Unit = {
    val writer = new PrintWriter(new File(filePath))

    val header = (List("Node", "Class", "H_Value") ++ targetLabels).mkString(",")
    writer.println(header)

    report.foreach { row =>
      val scores = targetLabels.map(label => "%.4f".format(row.scores.getOrElse(label, 0.0)))
      val cleanNode = s"\"${row.nodeString.replace("\"", "'")}\""
      val line = (List(cleanNode, row.nodeClass, row.hValue) ++ scores).mkString(",")
      writer.println(line)
    }

    writer.close()
  }
}