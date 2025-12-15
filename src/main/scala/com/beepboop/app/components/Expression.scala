package com.beepboop.app.components

import com.beepboop.app.components.*
import com.beepboop.app.dataprovider.{DataProvider, VarNameGenerator}
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.policy.{EnsureSpecificVarExists, Policy}
import com.beepboop.app.postprocessor.Postprocessor
import com.beepboop.app.utils.Implicits.integerNumeric

import java.lang.Integer.sum
import java.util
import scala.reflect.{ClassTag, classTag}

def stringWithSpaces(strings: String*): String = strings.mkString(" ")

trait ScopeModifier { def getAdditionalPolicies: List[Policy] }
trait ComposableExpression {
  def children: List[Expression[?]]
  def withNewChildren(newChildren: List[Expression[?]]): Expression[?]
}
trait OperatorContainer {
  def operator: Operator[?]
  def withNewOperator(newOp: Operator[?]): Expression[?]
}
trait Creatable {
  def templateSignature: Signature
  def create(children: List[Expression[?]]): Expression[?]
}

abstract class Expression[ReturnT](implicit val ct: ClassTag[ReturnT]) extends LogTrait with Serializable {
  def toString: String
  def eval(context: Map[String, Any]): ReturnT
  def evalToString: String
  def signature: Signature
  def distance(context: Map[String, Any]): Int = 0
}

case class Variable[ReturnT : ClassTag ](name: String) extends Expression[ReturnT] {
  override def toString: String = name
  override def evalToString: String = eval.toString
  override def signature: Signature = Signature(Nil, scalaTypeToExprType(classTag[ReturnT].runtimeClass))
  override def eval(context: Map[String, Any]): ReturnT = context.getOrElse(name, throw new NoSuchElementException(s"Variable '$name' not found")).asInstanceOf[ReturnT]
}

case class Constant[ReturnT : ClassTag](value: ReturnT) extends Expression[ReturnT] {
  override def toString: String = value match {
    case list: List[_] => list.mkString("[", ", ", "]")
    case arr: Array[_] => arr.mkString("[", ", ", "]")
    case _ => value.toString
  }
  override def eval(context: Map[String, Any]): ReturnT = value
  override def evalToString: String = value.toString
  override def signature: Signature = Signature(Nil, scalaTypeToExprType(classTag[ReturnT].runtimeClass))
}

object Constant {
  def asCreatable[T: ClassTag](randomValueGenerator: () => T): Creatable = new Creatable {
    override def templateSignature: Signature = Signature(Nil, scalaTypeToExprType(classTag[T].runtimeClass))
    override def create(children: List[Expression[?]]): Expression[?] = Constant(randomValueGenerator())
  }
}

case class IteratorDef[IterT : ClassTag](variableName: String, collection: Expression[List[IterT]]) extends Expression[(String, List[IterT])] with ComposableExpression {
  override def children: List[Expression[?]] = List(collection)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(collection = newChildren.head.asInstanceOf[Expression[List[IterT]]])
  override def eval(context: Map[String, Any]): (String, List[IterT]) = (variableName, collection.eval(context))
  override def toString: String = s"$variableName in $collection"
  override def evalToString: String = s"${variableName} in ${collection.evalToString}"
  override def signature: Signature = Signature(Nil, UnknownType)
}


case class BinaryExpression[ReturnT : ClassTag](
                                                 left: Expression[?],
                                                 operator: BinaryOperator[ReturnT],
                                                 right: Expression[?]
                                               ) extends Expression[ReturnT] with OperatorContainer with ComposableExpression {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = this.copy(operator = newOp.asInstanceOf[BinaryOperator[ReturnT]])
  override def children: List[Expression[?]] = List(left,right)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2)
    this.copy(left = newChildren.head, right = newChildren(1))
  }
  override def toString: String = stringWithSpaces("(", left.toString, operator.toString, right.toString, ")")
  override def evalToString: String = stringWithSpaces("(", left.evalToString, operator.toString, right.evalToString, ")")
  override def eval(context: Map[String, Any]): ReturnT = operator.eval(left.eval(context), right.eval(context))
  override def signature: Signature = operator.signature

  override def distance(context: Map[String, Any]): Int = {
    operator match {
      case _: AndOperator[_] =>
        left.distance(context) + right.distance(context)
      case _: OrOperator[_] =>
        Math.min(left.distance(context), right.distance(context))
      case _: ImpliesOperator[_] =>
        if (left.eval(context).asInstanceOf[Boolean]) right.distance(context) else 0
      case _ =>
        operator.distance(left.eval(context), right.eval(context))
    }
  }
}

object BinaryExpression {
  def asCreatable[T](op: BinaryOperator[T]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      require(children.length == 2)
      implicit val tag: ClassTag[T] = op.ct
      BinaryExpression(children(0).asInstanceOf[Expression[T]] , op, children(1).asInstanceOf[Expression[T]])
    }
  }
}

case class UnaryExpression[ReturnT : ClassTag](expr: Expression[?], operator: UnaryOperator[ReturnT]) extends Expression[ReturnT] with OperatorContainer with ComposableExpression {
  override def withNewOperator(newOp: Operator[?]): Expression[?] = this.copy(operator = newOp.asInstanceOf[UnaryOperator[ReturnT]])
  override def eval(context: Map[String, Any]): ReturnT = operator.eval(expr.eval(context))
  override def children: List[Expression[?]]  = List(expr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(expr = newChildren(0))
  override def toString: String =  operator.toString + "(" + expr.toString + ")"
  override def evalToString: String = operator.toString + expr.evalToString
  override def signature: Signature = operator.signature
}

object UnaryExpression {
  def asCreatable[T](op: UnaryOperator[T]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      implicit val tag: ClassTag[T] = op.ct
      UnaryExpression(children(0), op)
    }
  }
}

case class SumExpression[ReturnT : Numeric : ClassTag](expr: Expression[List[ReturnT]]) extends Expression[ReturnT] with ComposableExpression {
  override def children: List[Expression[?]]  = List(expr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(expr = newChildren.head.asInstanceOf[Expression[List[ReturnT]]])
  override def toString: String =  "sum(" + expr.toString + ")"
  override def evalToString: String =  "sum(" + expr.evalToString + ")"
  override def eval(context: Map[String, Any]): ReturnT = {
    val numeric = implicitly[Numeric[ReturnT]]
    expr.eval(context).foldLeft(numeric.zero)(numeric.plus)
  }
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)), scalaTypeToExprType(classTag[ReturnT].runtimeClass))
}
object SumExpression {
  object IntListSumFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType), IntType)
    override def create(children: List[Expression[?]]): Expression[?] = SumExpression[Integer](children.head.asInstanceOf[Expression[List[Integer]]])
  }
}

case class CountExpression(expr: Expression[? <: List[Any]]) extends Expression[Integer] with ComposableExpression {
  override def children: List[Expression[?]] = List(expr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(expr = newChildren.head.asInstanceOf[Expression[List[?]]])
  override def toString: String = s"count(${expr.toString})"
  override def evalToString: String = s"count(${expr.evalToString})"
  override def eval(context: Map[String, Any]): Integer = expr.eval(context).length
  override def signature: Signature = Signature(List(ListAnyType), IntType)
}
object CountExpression {
  object IntListCountFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType), IntType)
    override def create(children: List[Expression[?]]): Expression[?] = CountExpression(children.head.asInstanceOf[Expression[List[Any]]])
  }
}

case class ForAllExpression[IterT](iteratorDef: IteratorDef[IterT], body: Expression[Boolean]) extends Expression[Boolean] with ComposableExpression with ScopeModifier {
  override def children: List[Expression[?]] = List(iteratorDef, body)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(iteratorDef = newChildren(0).asInstanceOf[IteratorDef[IterT]], body = newChildren(1).asInstanceOf[Expression[Boolean]])
  override def getAdditionalPolicies: List[Policy] = List(EnsureSpecificVarExists(iteratorDef.variableName))
  override def toString: String = s"forall($iteratorDef)($body)"
  override def evalToString: String = s"forall($iteratorDef)($body)"
  override def eval(context: Map[String, Any]): Boolean = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.forall(item => body.eval(context.+(variableName -> item)))
  }
  override def distance(context: Map[String, Any]): Int = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.map(item => body.distance(context.+(variableName -> item))).sum
  }
  override def signature: Signature = Signature(List(ListIntType, BoolType), BoolType)
}
object ForAllExpression {
  object ForAllIntListFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType, BoolType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = ForAllExpression(IteratorDef(VarNameGenerator.generateUniqueName(), children(0).asInstanceOf[Expression[List[Integer]]]), children(1).asInstanceOf[Expression[Boolean]])
  }
}

case class ExistsExpression[IterT](iteratorDef: IteratorDef[IterT], body: Expression[Boolean]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(iteratorDef, body)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(iteratorDef = newChildren(0).asInstanceOf[IteratorDef[IterT]], body = newChildren(1).asInstanceOf[Expression[Boolean]])
  override def toString: String = s"exists($iteratorDef)($body)"
  override def evalToString: String = s"exists($iteratorDef)($body)"
  override def eval(context: Map[String, Any]): Boolean = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.exists(item => body.eval(context.+(variableName -> item)))
  }
  override def distance(context: Map[String, Any]): Int = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    if (itemsToIterate.isEmpty) 1 else itemsToIterate.map(item => body.distance(context.+(variableName -> item))).min
  }
  override def signature: Signature = Signature(List(ListAnyType, BoolType), BoolType)
}
object ExistsExpression {
  object ExistsIntListFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListAnyType, BoolType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = ExistsExpression(IteratorDef("j", children(0).asInstanceOf[Expression[List[Integer]]]), children(1).asInstanceOf[Expression[Boolean]])
  }
}

case class AllDifferentExpression(expr: Expression[? <: List[?]]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(expr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(expr = newChildren.head.asInstanceOf[Expression[List[?]]])
  override def toString: String = s"alldifferent(${expr.toString})"
  override def evalToString: String = s"alldifferent(${expr.evalToString})"
  override def eval(context: Map[String, Any]): Boolean = {
    val list = expr.eval(context)
    list.nonEmpty && (list.size == list.toSet.size)
  }
  override def distance(context: Map[String, Any]): Int = {
    val list = expr.eval(context)
    if (list.isEmpty) 0
    else {
      val counts = list.groupBy(identity).view.mapValues(_.size).toMap
      counts.values.map(count => if (count > 1) count - 1 else 0).sum
    }
  }
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[List[?]].runtimeClass)), BoolType)
}
object AllDifferentExpression {
  object ListAllDifferentFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = AllDifferentExpression(children.head.asInstanceOf[Expression[List[?]]])
  }
}

case class LexicographicalExpression[T : Ordering : ClassTag](
                                                               leftExpr: Expression[List[T]],
                                                               operator: BinaryOperator[Boolean],
                                                               rightExpr: Expression[List[T]]
                                                             ) extends Expression[Boolean] with OperatorContainer with ComposableExpression {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = this.copy(operator = newOp.asInstanceOf[BinaryOperator[Boolean]])
  override def children: List[Expression[?]] = List(leftExpr, rightExpr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    this.copy(leftExpr = newChildren(0).asInstanceOf[Expression[List[T]]], rightExpr = newChildren(1).asInstanceOf[Expression[List[T]]])
  }
  override def toString: String = s"lex(${leftExpr}, ${operator}, ${rightExpr})"
  override def evalToString: String = s"lex(${leftExpr.evalToString}, ${operator}, ${rightExpr.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val leftList = leftExpr.eval(context)
    val rightList = rightExpr.eval(context)
    implicit val ordering: Ordering[List[T]] = Ordering.Implicits.seqOrdering
    val res = ordering.compare(leftList, rightList)

    operator match {
      case _: LessOperator[_] => res < 0
      case _: LessEqualOperator[_] => res <= 0
      case _: GreaterOperator[_] => res > 0
      case _: GreaterEqualOperator[_] => res >= 0
      case _: EqualOperator[_] => res == 0
      case _: NotEqualOperator[_] => res != 0
      case _ => false
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    if (eval(context)) return 0
    val leftList = leftExpr.eval(context)
    val rightList = rightExpr.eval(context)

    val pairs = leftList.zip(rightList)
    val idx = pairs.indexWhere { case (a, b) => a != b }

    if (idx == -1) {
      if (leftList.length != rightList.length) 1 else 0
    } else {
      val a = leftList(idx)
      val b = rightList(idx)
      operator.distance(a, b)
    }
  }

  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[List[T]].runtimeClass), scalaTypeToExprType(classTag[List[T]].runtimeClass)), BoolType)
}

object LexicographicalExpression {
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[?] = {
      LexicographicalExpression[Integer](children(0).asInstanceOf[Expression[List[Integer]]], op, children(1).asInstanceOf[Expression[List[Integer]]])
    }
  }
}

case class MinimumExpression[ReturnT : Numeric : ClassTag](elements: Expression[List[ReturnT]]) extends Expression[ReturnT] with ComposableExpression {
  override def children: List[Expression[?]] = List(elements)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(elements = newChildren.head.asInstanceOf[Expression[List[ReturnT]]])
  override def toString: String = s"min(${elements.toString})"
  override def evalToString: String = s"min(${elements.evalToString})"
  override def eval(context: Map[String, Any]): ReturnT = elements.eval(context).min(implicitly[Numeric[ReturnT]])
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)), scalaTypeToExprType(classTag[ReturnT].runtimeClass))
}
object MinimumExpression {
  object ListMinimumFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType), IntType)
    override def create(children: List[Expression[?]]): Expression[?] = MinimumExpression[Integer](children.head.asInstanceOf[Expression[List[Integer]]])
  }
}
case class MaximumExpression[ReturnT : Numeric : ClassTag](elements: Expression[List[ReturnT]]) extends Expression[ReturnT] with ComposableExpression {
  override def children: List[Expression[?]] = List(elements)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(elements = newChildren.head.asInstanceOf[Expression[List[Integer]]])
  override def toString: String = s"max(${elements.toString})"
  override def evalToString: String = s"max(${elements.evalToString})"
  override def eval(context: Map[String, Any]): ReturnT = elements.eval(context).max(implicitly[Numeric[ReturnT]])
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)), scalaTypeToExprType(classTag[ReturnT].runtimeClass))
}
object MaximumExpression {
  object ListMaximumFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType), IntType)
    override def create(children: List[Expression[?]]): Expression[?] = MaximumExpression[Integer](children.head.asInstanceOf[Expression[List[Integer]]])
  }
}
case class Task(start: Expression[Integer], duration: Expression[Integer], demand: Expression[Integer]) {
  override def toString: String = s"Task(${start.toString}, ${duration.toString}, ${demand.toString})"
  def evalToString: String = s"Task(${start.evalToString}, ${duration.evalToString}, ${demand.evalToString})"
}

case class CumulativeExpression(
                                 tasks: Expression[List[Task]],
                                 operator: BinaryOperator[Boolean],
                                 limit: Expression[Integer]
                               ) extends Expression[Boolean] with OperatorContainer with ComposableExpression {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = this.copy(operator = newOp.asInstanceOf[BinaryOperator[Boolean]])
  override def children: List[Expression[?]] = List(tasks, limit)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(tasks = newChildren(0).asInstanceOf[Expression[List[Task]]], limit = newChildren(1).asInstanceOf[Expression[Integer]])
  override def toString: String = s"cumulative(${tasks.toString}, ${operator.toString}, ${limit.toString})"
  override def evalToString: String = s"cumulative(${tasks.evalToString}, ${operator.toString}, ${limit.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val taskList = tasks.eval(context)
    val cap = limit.eval(context).intValue()
    if (taskList.isEmpty) return true

    val evaluatedTasks = taskList.map { task =>
      (task.start.eval(context).intValue(), task.duration.eval(context).intValue(), task.demand.eval(context).intValue())
    }

    val minStart = evaluatedTasks.map(_._1).min
    val maxEnd = evaluatedTasks.map { case (s, d, _) => s + d }.max

    (minStart until maxEnd).forall { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) => time >= start && time < start + duration }.map(_._3).sum
      operator.eval(currentLoad, cap)
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    val taskList = tasks.eval(context)
    val cap = limit.eval(context).intValue()
    if (taskList.isEmpty) return 0

    val evaluatedTasks = taskList.map { task =>
      (task.start.eval(context).intValue(), task.duration.eval(context).intValue(), task.demand.eval(context).intValue())
    }
    val minStart = evaluatedTasks.map(_._1).min
    val maxEnd = evaluatedTasks.map { case (s, d, _) => s + d }.max

    (minStart until maxEnd).map { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) => time >= start && time < start + duration }.map(_._3).sum
      operator.distance(currentLoad, cap)
    }.sum
  }
  override def signature: Signature = Signature(List(ListTaskType, IntType), BoolType)
}
object CumulativeExpression {
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable {
    override def templateSignature: Signature = Signature(List(ListTaskType, IntType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = CumulativeExpression(children(0).asInstanceOf[Expression[List[Task]]], op, children(1).asInstanceOf[Expression[Integer]])
  }
}

case class GlobalCardinalityExpression(variablesExpr: Expression[List[Integer]], expectedCountsExpr: Expression[Map[Integer, Integer]]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(variablesExpr, expectedCountsExpr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(variablesExpr = newChildren(0).asInstanceOf[Expression[List[Integer]]], expectedCountsExpr = newChildren(1).asInstanceOf[Expression[Map[Integer, Integer]]])
  override def toString: String = s"global_cardinality(${variablesExpr.toString}, ${expectedCountsExpr.toString})"
  override def evalToString: String = s"global_cardinality(${variablesExpr.evalToString}, ${expectedCountsExpr.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val list = variablesExpr.eval(context)
    val requiredCounts = expectedCountsExpr.eval(context)
    val actualFrequencies = list.filter(requiredCounts.contains).groupBy(identity).view.mapValues(_.size).toMap
    requiredCounts.forall { case (value, requiredCount) => actualFrequencies.getOrElse(value, 0) == requiredCount.intValue() }
  }

  override def distance(context: Map[String, Any]): Int = {
    val list = variablesExpr.eval(context)
    val requiredCounts = expectedCountsExpr.eval(context)
    val actualFrequencies = list.filter(requiredCounts.contains).groupBy(identity).view.mapValues(_.size).toMap
    requiredCounts.map { case (value, requiredCount) => Math.abs(actualFrequencies.getOrElse(value, 0) - requiredCount.intValue()) }.sum
  }
  override def signature: Signature = Signature(List(ListIntType, MapIntToIntType), BoolType)
}
object GlobalCardinalityExpression {
  object GlobalCardinalityFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListAnyType, MapIntToIntType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = GlobalCardinalityExpression(children(0).asInstanceOf[Expression[List[Integer]]], children(1).asInstanceOf[Expression[Map[Integer, Integer]]])
  }
}

case class RectDescriptor(x: Expression[Integer], y: Expression[Integer], width: Expression[Integer], height: Expression[Integer]) {
  override def toString: String = s"RectDescriptor(${x.toString}, ${y.toString}, ${width.toString}, ${height.toString})"
  def evalToString: String = s"RectDescriptor(${x.evalToString}, ${y.evalToString}, ${width.evalToString}, ${height.evalToString})"
}

case class DiffnExpression(rectsExpr: Expression[List[RectDescriptor]]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(rectsExpr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(rectsExpr = newChildren.head.asInstanceOf[Expression[List[RectDescriptor]]])
  override def toString: String = s"diffn(${rectsExpr.toString})"
  override def evalToString: String = s"diffn(${rectsExpr.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val rects = rectsExpr.eval(context)
    val evaluatedRects = rects.map { r => (r.x.eval(context).intValue(), r.y.eval(context).intValue(), r.width.eval(context).intValue(), r.height.eval(context).intValue()) }
    evaluatedRects.combinations(2).forall {
      case List((x1, y1, w1, h1), (x2, y2, w2, h2)) =>
        (x1 + w1 <= x2) || (x2 + w2 <= x1) || (y1 + h1 <= y2) || (y2 + h2 <= y1)
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    val rects = rectsExpr.eval(context)
    val evaluatedRects = rects.map { r => (r.x.eval(context).intValue(), r.y.eval(context).intValue(), r.width.eval(context).intValue(), r.height.eval(context).intValue()) }

    evaluatedRects.combinations(2).map {
      case List((x1, y1, w1, h1), (x2, y2, w2, h2)) =>
        val xOverlap = Math.max(0, Math.min(x1 + w1, x2 + w2) - Math.max(x1, x2))
        val yOverlap = Math.max(0, Math.min(y1 + h1, y2 + h2) - Math.max(y1, y2))

        if (xOverlap > 0 && yOverlap > 0) {
          Math.min(xOverlap, yOverlap)
        } else {
          0
        }
    }.sum
  }
  override def signature: Signature = Signature(List(ListRectType), BoolType)
}
object DiffnExpression {
  object DiffnFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListRectType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = DiffnExpression(children.head.asInstanceOf[Expression[List[RectDescriptor]]])
  }
}

case class ValuePrecedesChainExpression(variablesExpr: Expression[List[Integer]], valuesToPrecedeExpr: Expression[List[Integer]]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(variablesExpr, valuesToPrecedeExpr)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(variablesExpr = newChildren(0).asInstanceOf[Expression[List[Integer]]], valuesToPrecedeExpr = newChildren(1).asInstanceOf[Expression[List[Integer]]])
  override def toString: String = s"value_precede_chain(${variablesExpr.toString}, ${valuesToPrecedeExpr.toString})"
  override def evalToString: String = s"value_precede_chain(${variablesExpr.evalToString}, ${valuesToPrecedeExpr.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val list = variablesExpr.eval(context)
    val values = valuesToPrecedeExpr.eval(context)
    values.sliding(2).forall {
      case List(v1, v2) =>
        val idx1 = list.indexOf(v1)
        val idx2 = list.indexOf(v2)
        if (idx2 == -1) true
        else (idx1 != -1 && idx1 < idx2)
      case _ => true
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    if (eval(context)) return 0
    val list = variablesExpr.eval(context)
    val values = valuesToPrecedeExpr.eval(context)
    values.sliding(2).map {
      case List(v1, v2) =>
        val idx1 = list.indexOf(v1)
        val idx2 = list.indexOf(v2)
        if (idx2 != -1) {
          if (idx1 == -1) 1
          else if (idx1 > idx2) (idx1 - idx2)
          else 0
        } else 0
      case _ => 0
    }.sum
  }
  override def signature: Signature = Signature(List(ListIntType, ListIntType), BoolType)
}
object ValuePrecedesChainExpression {
  object ValuePrecedesChainFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(ListIntType, ListIntType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = ValuePrecedesChainExpression(children(0).asInstanceOf[Expression[List[Integer]]], children(1).asInstanceOf[Expression[List[Integer]]])
  }
}

case class RedundantConstraint[T : ClassTag](innerConstraint: Expression[T]) extends Expression[T] with ComposableExpression {
  override def children: List[Expression[?]] = List(innerConstraint)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(innerConstraint = newChildren.head.asInstanceOf[Expression[T]])
  override def toString: String = s"redundant_constraint(${innerConstraint.toString})"
  override def evalToString: String = s"redundant_constraint(${innerConstraint.evalToString})"
  override def eval(context: Map[String, Any]): T = innerConstraint.eval(context)
  override def distance(context: Map[String, Any]): Int = innerConstraint.distance(context)
  override def signature: Signature = Signature(List(UnknownType), BoolType)
}
object RedundantConstraint {
  object RedundantConstraintFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(UnknownType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = RedundantConstraint(children.head.asInstanceOf[Expression[Any]])
  }
}

case class StrEqExpression(s1: Expression[String], s2: Expression[String]) extends Expression[Boolean] with ComposableExpression {
  override def children: List[Expression[?]] = List(s1, s2)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = this.copy(s1 = newChildren(0).asInstanceOf[Expression[String]], s2 = newChildren(1).asInstanceOf[Expression[String]])
  override def toString: String = s"str_eq(${s1.toString}, ${s2.toString})"
  override def evalToString: String = s"str_eq(${s1.evalToString}, ${s2.evalToString})"
  override def eval(context: Map[String, Any]): Boolean = s1.eval(context).equals(s2.eval(context))
  override def distance(context: Map[String, Any]): Int = if (eval(context)) 0 else 1
  override def signature: Signature = Signature(List(StringType, StringType), BoolType)
}
object StrEqExpression {
  object StrEqFactory extends Creatable {
    override def templateSignature: Signature = Signature(List(StringType, StringType), BoolType)
    override def create(children: List[Expression[?]]): Expression[?] = StrEqExpression(children(0).asInstanceOf[Expression[String]], children(1).asInstanceOf[Expression[String]])
  }
}