package com.beepboop.app.experiment

import java.io.{File, PrintWriter}
import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}
import com.beepboop.app.components.{ComposableExpression, Constant, Expression, OperatorContainer, Variable, IteratorDef, ForAllExpression, ExistsExpression}
import com.beepboop.app.dataprovider.AStarSnapshot
import com.beepboop.app.logger.LogTrait

object SimpleSimilarity {

  // --- Configuration ---
  private val symmetricOps = Set("+", "*", "=", "!=", "and", "or", "xor", "diffn", "all_different")

  // Operator Families for "Partial Credit"
  private val comparisonOps = Set("<", ">", "<=", ">=", "=", "!=")
  private val arithmeticOps = Set("+", "-", "*", "/", "mod", "dist", "sum", "AddOperator", "SubOperator")
  private val logicOps = Set("and", "or", "xor", "implies", "equivalent")

  /**
   * Main Calculation Function
   *
   * @param a        The generated expression
   * @param b        The target expression
   * @param bindings Mapping of variable names (e.g., "f" -> "i") for loop bodies
   * @return Score between 0.0 and 1.0
   */
  def calculate(
                 a: Expression[?],
                 b: Expression[?],
                 bindings: Map[String, String] = Map.empty
               ): Double = {

    // 0. Type Check (Fast Fail)
    if (a.getClass != b.getClass) return 0.0

    (a, b) match {
      // --- Case 1: Variables (Respect Bindings) ---
      case (v1: Variable[_], v2: Variable[_]) =>
        // If v1 is a local iterator (e.g. 'f'), it must map to 'i'.
        // If v1 is global (e.g. 'yCoor'), it must match 'yCoor'.
        val targetName = bindings.getOrElse(v1.name, v1.name)
        if (targetName == v2.name) 1.0 else 0.0

      // --- Case 2: Constants ---
      case (c1: Constant[_], c2: Constant[_]) =>
        if (c1.value == c2.value) 1.0 else 0.0

      // --- Case 3: Quantifiers (Loops) ---
      case (loopA: ForAllExpression[_], loopB: ForAllExpression[_]) =>
        // 1. Check Collection (e.g. are both iterating over FLIGHT?)
        val colScore = calculate(loopA.iteratorDef.collection, loopB.iteratorDef.collection, bindings)
        if (colScore < 0.9) return 0.0 // Different domain -> 0 match

        // 2. Bind Iterator Variable (e.g. map 'f' -> 'i')
        val newBindings = bindings + (loopA.iteratorDef.variableName -> loopB.iteratorDef.variableName)

        // 3. Compare Bodies
        val bodyScore = calculate(loopA.body, loopB.body, newBindings)

        // Weighted: 20% for getting the right set, 80% for the logic inside
        (colScore * 0.2) + (bodyScore * 0.8)

      // --- Case 4: Operators (The Meat) ---
      case (comp1: ComposableExpression, comp2: ComposableExpression) =>
        val op1 = getOperatorName(comp1)
        val op2 = getOperatorName(comp2)

        // A. Operator Score
        var opScore = 0.0
        if (op1 == op2) opScore = 1.0
        else if (comparisonOps(op1) && comparisonOps(op2)) opScore = 0.5
        else if (arithmeticOps(op1) && arithmeticOps(op2)) opScore = 0.5
        else if (logicOps(op1) && logicOps(op2)) opScore = 0.5
        else return 0.0 // Incompatible families (e.g. "+" vs "<") -> No similarity

        // B. Structure Score
        val childrenA = comp1.children
        val childrenB = comp2.children

        // Arity Mismatch (e.g. Binary vs Unary) -> 0.0
        if (childrenA.size != childrenB.size) return 0.0

        val structScore = if (childrenA.isEmpty) 1.0 else {
          if (symmetricOps.contains(op1) || symmetricOps.contains(op2)) {
            // SYMMETRIC: Greedy Bag Match (A+B matches B+A)
            var pool = childrenB
            val scores = childrenA.map { childA =>
              val bestMatch = pool.map(b => (b, calculate(childA, b, bindings))).maxBy(_._2)
              pool = pool.filterNot(_ eq bestMatch._1)
              bestMatch._2
            }
            scores.sum / childrenA.size
          } else {
            // ORDERED: Position-wise Match (A-B != B-A)
            childrenA.zip(childrenB).map {
              case (cA, cB) => calculate(cA, cB, bindings)
            }.sum / childrenA.size
          }
        }

        // C. Final Weighted Score
        // 20% Operator, 80% Children
        (opScore * 0.2) + (structScore * 0.8)

      case _ => 0.0
    }
  }

  // Helper to extract string name of operator
  private def getOperatorName(expr: ComposableExpression): String = expr match {
    case c: OperatorContainer => c.operator.toString
    case _ => expr.getClass.getSimpleName
  }
}

object UniversalExpressionMetric extends LogTrait {

  private val symmetricOperators: Set[String] = Set(
    "*", "=", "!=", "and", "or", "xor", "diffn", "all_different", "AddOperator", "MultiplyOperator"
  )
  private val additiveOps: Set[String] = Set("+", "-", "AddOperator", "SubOperator")

  def calculate(
                 found: Expression[?],
                 target: Expression[?],
                 policy: SimilarityPolicy = SimilarityPolicy(),
                 bindings: Map[String, String] = Map.empty
               ): Double = {

    // 1. Calculate Base Structural Similarity (0.0 - 1.0)
    val structuralScore = calculateStructural(found, target, policy, bindings)

    // 2. Calculate Ingredient Match (Variables used)
    val foundIngredients = extractIngredients(found)
    val targetIngredients = extractIngredients(target)

    if (targetIngredients.nonEmpty) {
      val intersection = foundIngredients.intersect(targetIngredients).size
      val union = foundIngredients.union(targetIngredients).size
      val jaccardIndex = if (union == 0) 0.0 else intersection.toDouble / union

      // Weighted Average: 70% Structure + 30% Ingredients
      (structuralScore * 0.7) + (jaccardIndex * 0.3)
    } else {
      structuralScore
    }
  }

  // --- Helper Methods ---

  private def extractIngredients(expr: Expression[?]): Set[String] = {
    expr match {
      case v: Variable[_] => Set(v.name)
      case c: ComposableExpression => c.children.flatMap(extractIngredients).toSet
      case f: ForAllExpression[_] => extractIngredients(f.body) ++ extractIngredients(f.iteratorDef.collection)
      case e: ExistsExpression[_] => extractIngredients(e.body) ++ extractIngredients(e.iteratorDef.collection)
      case _ => Set.empty
    }
  }

  private def calculateStructural(
                                   found: Expression[?],
                                   target: Expression[?],
                                   policy: SimilarityPolicy,
                                   bindings: Map[String, String]
                                 ): Double = {
    if (found.getClass != target.getClass) return 0.0

    (found, target) match {
      case (fVar: Variable[_], tVar: Variable[_]) =>
        if (bindings.contains(fVar.name)) {
          if (bindings(fVar.name) == tVar.name) 1.0 else 0.0
        } else {
          if (fVar.name == tVar.name) 1.0 else 0.0
        }

      case (fConst: Constant[_], tConst: Constant[_]) =>
        if (fConst.value == tConst.value) 1.0 else 0.5

      case (fLoop: ForAllExpression[_], tLoop: ForAllExpression[_]) =>
        compareQuantifier(fLoop.iteratorDef, fLoop.body, tLoop.iteratorDef, tLoop.body, policy, bindings)

      case (fComp: ComposableExpression, tComp: ComposableExpression) =>
        val fOp = getOperatorName(fComp)
        val tOp = getOperatorName(tComp)

        // Flatten A+B-C chains
        if (additiveOps.contains(fOp) && additiveOps.contains(tOp)) {
          val fTerms = flattenAdditive(fComp)
          val tTerms = flattenAdditive(tComp)
          if (fTerms.size != tTerms.size) return 0.0

          // Greedy match
          var available = tTerms
          var sum = 0.0
          fTerms.foreach { case (fE, fS) =>
            val matchOpt = available.map { case (tE, tS) =>
              val sim = calculateStructural(fE, tE, policy, bindings)
              val score = sim * (if (fS == tS) 1.0 else 0.0)
              ((tE, tS), score)
            }.maxByOption(_._2)

            matchOpt.foreach { case (best, score) =>
              sum += score
              available = available.filterNot(_ == best)
            }
          }
          return sum / fTerms.size
        }

        if (fOp != tOp) return 0.0

        val fCh = fComp.children
        val tCh = tComp.children

        if (symmetricOperators.contains(fOp)) calculateSymmetric(fCh, tCh, policy, bindings)
        else calculateOrdered(fCh, tCh, policy, bindings)

      case _ => 0.0
    }
  }

  private def compareQuantifier(
                                 fIter: IteratorDef[_], fBody: Expression[_],
                                 tIter: IteratorDef[_], tBody: Expression[_],
                                 p: SimilarityPolicy,
                                 bindings: Map[String, String]
                               ): Double = {
    val colSim = calculateStructural(fIter.collection, tIter.collection, p, bindings)
    if (colSim < 0.9) return 0.0
    val newBindings = bindings + (fIter.variableName -> tIter.variableName)
    (colSim * 0.2) + (calculateStructural(fBody, tBody, p, newBindings) * 0.8)
  }

  private def flattenAdditive(expr: Expression[?], sign: Int = 1): List[(Expression[?], Int)] = expr match {
    case c: ComposableExpression =>
      getOperatorName(c) match {
        case "+" | "AddOperator" => flattenAdditive(c.children(0), sign) ++ flattenAdditive(c.children(1), sign)
        case "-" | "SubOperator" => flattenAdditive(c.children(0), sign) ++ flattenAdditive(c.children(1), -sign)
        case _ => List((expr, sign))
      }
    case _ => List((expr, sign))
  }

  // FIXED: Renamed parameters to avoid shadowing 'b' (expression) with 'bindings' (map)
  private def calculateOrdered(
                                f: List[Expression[?]],
                                t: List[Expression[?]],
                                p: SimilarityPolicy,
                                bindings: Map[String, String]
                              ): Double = {
    if (f.size != t.size) return 0.0
    f.zip(t).map { case (foundExpr, targetExpr) =>
      calculateStructural(foundExpr, targetExpr, p, bindings)
    }.sum / f.size
  }

  // FIXED: Renamed parameters here as well for consistency
  private def calculateSymmetric(
                                  f: List[Expression[?]],
                                  t: List[Expression[?]],
                                  p: SimilarityPolicy,
                                  bindings: Map[String, String]
                                ): Double = {
    if (f.size != t.size) return 0.0
    var pool = t
    f.map { fCh =>
      val (best, score) = pool.map(tCh => (tCh, calculateStructural(fCh, tCh, p, bindings))).maxBy(_._2)
      pool = pool.filterNot(_ eq best)
      score
    }.sum / f.size
  }

  private def getOperatorName(e: ComposableExpression): String = e match {
    case o: OperatorContainer => o.operator.toString
    case _ => e.getClass.getSimpleName
  }
}


// ---------------------------------------------------------
// 2. Data Structures for Reporting
// ---------------------------------------------------------

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

// ---------------------------------------------------------
// 3. The Reporter Class
// ---------------------------------------------------------

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

// ---------------------------------------------------------
// 4. The Model Loader
// ---------------------------------------------------------

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

// ---------------------------------------------------------
// 5. The CSV Exporter
// ---------------------------------------------------------

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