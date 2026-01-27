package com.beepboop.app.experiment

import java.io.{File, PrintWriter}
import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}
import com.beepboop.app.components.{ComposableExpression, Constant, Expression, OperatorContainer, Variable, IteratorDef, ForAllExpression, ExistsExpression}
import com.beepboop.app.dataprovider.AStarSnapshot
import com.beepboop.app.logger.LogTrait

object SimpleSimilarity {

  private val symmetricOps = Set("+", "*", "=", "!=", "and", "or", "xor", "diffn", "all_different")

  private val comparisonOps = Set("<", ">", "<=", ">=", "=", "!=")
  private val arithmeticOps = Set("+", "-", "*", "/", "mod", "dist", "sum", "AddOperator", "SubOperator")
  private val logicOps = Set("and", "or", "xor", "implies", "equivalent")

  def calculate(
                 a: Expression[?],
                 b: Expression[?],
                 bindings: Map[String, String] = Map.empty
               ): Double = {

    if (a.getClass != b.getClass) return 0.0

    (a, b) match {
      case (v1: Variable[_], v2: Variable[_]) =>
        val targetName = bindings.getOrElse(v1.name, v1.name)
        if (targetName == v2.name) 1.0 else 0.0

      case (c1: Constant[_], c2: Constant[_]) =>
        if (c1.value == c2.value) 1.0 else 0.0

      case (loopA: ForAllExpression[_], loopB: ForAllExpression[_]) =>
        val colScore = calculate(loopA.iteratorDef.collection, loopB.iteratorDef.collection, bindings)
        if (colScore < 0.9) return 0.0

        val newBindings = bindings + (loopA.iteratorDef.variableName -> loopB.iteratorDef.variableName)

        val bodyScore = calculate(loopA.body, loopB.body, newBindings)

        (colScore * 0.2) + (bodyScore * 0.8)

      case (comp1: ComposableExpression, comp2: ComposableExpression) =>
        val op1 = getOperatorName(comp1)
        val op2 = getOperatorName(comp2)

        var opScore = 0.0
        if (op1 == op2) opScore = 1.0
        else if (comparisonOps(op1) && comparisonOps(op2)) opScore = 0.5
        else if (arithmeticOps(op1) && arithmeticOps(op2)) opScore = 0.5
        else if (logicOps(op1) && logicOps(op2)) opScore = 0.5
        else return 0.0

        val childrenA = comp1.children
        val childrenB = comp2.children

        if (childrenA.size != childrenB.size) return 0.0

        val structScore = if (childrenA.isEmpty) 1.0 else {
          if (symmetricOps.contains(op1) || symmetricOps.contains(op2)) {
            var pool = childrenB
            val scores = childrenA.map { childA =>
              val bestMatch = pool.map(b => (b, calculate(childA, b, bindings))).maxBy(_._2)
              pool = pool.filterNot(_ eq bestMatch._1)
              bestMatch._2
            }
            scores.sum / childrenA.size
          } else {
            childrenA.zip(childrenB).map {
              case (cA, cB) => calculate(cA, cB, bindings)
            }.sum / childrenA.size
          }
        }

        (opScore * 0.2) + (structScore * 0.8)

      case _ => 0.0
    }
  }

  private def getOperatorName(expr: ComposableExpression): String = expr match {
    case c: OperatorContainer => c.operator.toString
    case _ => expr.getClass.getSimpleName
  }
}





case class ComparisonResult(
                             targetIndex: Int,
                             targetClassName: String,
                             similarityScore: Double
                           )

case class NodeSimilarityRow(
                              nodeString: String,
                              nodeClass: String,
                              hValue: Int,
                              complexity: Int,
                              scores: Map[String, Double]
                            )


class MultiTargetMetricReporter(model: TargetModel) {
  private val policy = model.similarityPolicy

  def generateFullReport(snapshot: AStarSnapshot): List[NodeSimilarityRow] = {
    (snapshot.visitedItems ++ snapshot.openSetItems).map { node =>
      val nodeExpr = node.constraint

      val scoreMap = model.targetConstraints.zipWithIndex.map { case (target, idx) =>
        val targetLabel = s"${target.getClass.getSimpleName}_$idx"
        val score = SimpleSimilarity.calculate(nodeExpr, target)
        targetLabel -> score
      }.toMap

      NodeSimilarityRow(
        nodeString = nodeExpr.toString,
        nodeClass = nodeExpr.getClass.getSimpleName,
        hValue = node.h,
        scores = scoreMap,
        complexity = node.constraint.complexity
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


object SimilarityCSVExporter {
  def dumpToCSV(report: List[NodeSimilarityRow], targetLabels: List[String], filePath: String): Unit = {
    val writer = new PrintWriter(new File(filePath))
    val header = (List("Node", "Class", "H_Value", "complexity") ++ targetLabels).mkString(";")
    writer.println(header)

    report.foreach { row =>
      val scores = targetLabels.map(label => "%.4f".format(row.scores.getOrElse(label, 0.0)))
      val cleanNode = s"\"${row.nodeString.replace("\"", "'")}\""
      val line = (List(cleanNode, row.nodeClass, row.hValue, row.complexity) ++ scores).mkString(";")
      writer.println(line)
    }
    writer.close()
  }
}