package com.beepboop.app.components

import com.beepboop.app.components.*
import com.beepboop.app.dataprovider.{DataProvider, VarNameGenerator}
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.utils.Implicits.integerNumeric

import java.lang.Integer.sum


/* third party modules */
import scala.reflect.{ClassTag, classTag}

def stringWithSpaces(strings: String*): String = {
  strings.mkString(" ")
}


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


sealed trait Expression[ReturnT] extends LogTrait with Serializable {
  def toString: String
  def eval(context: Map[String, Any]): ReturnT
  def evalToString: String
  def signature: Signature
  def distance(context: Map[String, Any]): Int = {
    0
  }
}

case class Variable[ReturnT : ClassTag ](name: String) extends Expression[ReturnT] {
  override def toString: String = name
  override def evalToString: String = eval.toString
  override def signature: Signature = {
    val outputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = Nil, output = outputType)
  }
  override def eval(context: Map[String, Any]): ReturnT = {
    context.get(name) match {
      case Some(value) => value.asInstanceOf[ReturnT]
      case None => throw new NoSuchElementException(s"Variable '$name' not found in evaluation context.")
    }
  }
}


case class Constant[ReturnT : ClassTag](value: ReturnT) extends Expression[ReturnT] {
  override def toString: String = value match {
    case list: List[_] => list.mkString("[", ", ", "]")
    case arr: Array[_] => arr.mkString("[", ", ", "]")
    case _ => value.toString
  }
  override def eval(context: Map[String, Any]): ReturnT = value
  override def evalToString: String = value.toString
  override def signature: Signature = {
    val outputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = Nil, output = outputType)
  }

}

object Constant {
  def asCreatable[T: ClassTag](randomValueGenerator: () => T): Creatable = new Creatable {
    override def templateSignature: Signature = {
      val outputType = scalaTypeToExprType(classTag[T].runtimeClass)
      Signature(inputs = Nil, output = outputType)
    }

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.isEmpty, "Constant requires no children.")
      Constant(randomValueGenerator())
    }
  }
}

case class IteratorDef[IterT](
                             variableName: String,
                             collection: Expression[List[IterT]]
                             ) extends Expression[(String, List[IterT])] with ComposableExpression {
  override def children: List[Expression[?]] = List(collection)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "IteratorDef requires exactly one child (the collection).")
    this.copy(collection = newChildren.head.asInstanceOf[Expression[List[IterT]]])
  }

  override def eval(context: Map[String, Any]): (String, List[IterT]) = {
    (variableName, collection.eval(context))
  }

  override def toString: String = s"$variableName in $collection"
  override def evalToString: String = s"${variableName} in ${collection.evalToString}"

  override def signature: Signature = {
    Signature(inputs = Nil, output = UnknownType)
  }

}

case class BinaryExpression[ReturnT](
                                      left: Expression[?],
                                      operator: BinaryOperator[ReturnT],
                                      right: Expression[?]
                                    ) extends Expression[ReturnT]
                                      with OperatorContainer
                                      with ComposableExpression
                                      {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = newOp match {
    case op: BinaryOperator[?] =>
      this.copy(operator = newOp.asInstanceOf[BinaryOperator[ReturnT]])
    case _ =>
      throw new IllegalArgumentException(s"Cannot replace operator in BinaryExpression with non-binary operator ${newOp}")
  }

  override def children: List[Expression[?]] = List(left,right)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "BinaryExpression requires exactly two children for reconstruction")

    require(
      newChildren.head.signature.output == operator.signature.inputs.head,
      s"Left child output data type doesn't match required operator signature input, ${newChildren.head.signature.output} != ${operator.signature.inputs.head} for ${this.operator.toString} (withNewChildren)"
    )

    require(
      newChildren(1).signature.output == operator.signature.inputs(1),
      s"Right child output data type doesn't match required operator signature input, ${newChildren(1).signature.output} != ${operator.signature.inputs(1)} for ${this.operator.toString} (withNewChildren)"
    )
    this.copy(left = newChildren.head, right = newChildren(1))
  }

  override def toString: String = stringWithSpaces("(", left.toString, operator.toString, right.toString, ")")
  override def evalToString: String = stringWithSpaces("(", left.evalToString, operator.toString, right.evalToString, ")")

  override def eval(context: Map[String, Any]): ReturnT = {
    val result = operator.eval(left.eval(context), right.eval(context))
    debug(s"result of eval for ${this.toString} is : ${left.eval(context).toString} ${operator.toString} ${right.eval(context).toString} = $result")
    result
  }
  override def signature: Signature = operator.signature


  override def distance(context: Map[String, Any]): Int = {
    operator.distance(left.eval(context), right.eval(context))
  }
}

object BinaryExpression {
  def asCreatable[T](op: BinaryOperator[T]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      require(children.length == 2)


      require(
        children(0).signature.output == op.signature.inputs(0),
        s"Left child output data type doesn't match required operator signature input, ${children(0).signature.output} != ${op.signature.inputs(0)} for ${op.toString}"
      )
      require(
        children(1).signature.output == op.signature.inputs(1),
        s"Right child output data type doesn't match required operator signature input, ${children(1).signature.output} != ${op.signature.inputs(1)} for ${op.toString}"
      )
      BinaryExpression(children(0), op, children(1))
    }
  }
}

case class UnaryExpression[ReturnT](
                                   expr: Expression[?],
                                   operator: UnaryOperator[ReturnT]
                                   ) extends Expression[ReturnT]
                                      with OperatorContainer
                                      with ComposableExpression {
  override def withNewOperator(newOp: Operator[?]): Expression[?] = newOp match {
    case op: UnaryOperator[ReturnT] =>
      this.copy(operator = newOp.asInstanceOf[UnaryOperator[ReturnT]])
    case _ =>
      throw new IllegalArgumentException(s"Cannot replace operator in BinaryExpression with non-binary operator ${newOp}")
  }

  override def eval(context: Map[String, Any]): ReturnT = operator.eval(expr.eval(context))

  override def children: List[Expression[?]]  = List(expr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "UnaryExpression requires exactly one children for reconstruction")
    this.copy(expr = newChildren(0))
  }

  override def toString: String =  operator.toString + "(" + expr.toString + ")"
  override def evalToString: String = operator.toString + expr.evalToString

  override def signature: Signature = operator.signature

}

object UnaryExpression {
  def asCreatable[T](op: UnaryOperator[T]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      require(children.length == 1)
      require(children(0).signature.output == op.signature.inputs(0), s"Child output data type doesn't match required operator signature input, ${children(0).signature.output} != ${op.signature.inputs(0)} for ${op.toString}")
      UnaryExpression(children(0), op)
    }
  }
}

/*
  Count ( expr: Expression[List[Any]] ) extends Expression [ReturnT]
  - has expression -> should extend ComposableExpression
    - children -> List(expr)
    - withNewChildren -> how it should replace arguments to his children
  - toString -> similiar to sumExpression |  count(listA)
  - evalToString ->  count([2,3,4,5])
  - eval(context)  expr(context).length
  - signature - input list
    outputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)

object CountExpression
  def asCreatable(List(Expr // similiar to SumExpression

  CountExpression[Integer](child) <- expr
*/





case class SumExpression[ReturnT : Numeric : ClassTag](
                expr: Expression[List[ReturnT]],
              ) extends Expression[ReturnT]
              with ComposableExpression {

  override def children: List[Expression[?]]  = List(expr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "SumExpression requires exactly one children for reconstruction")
    this.copy(expr = newChildren.head.asInstanceOf[Expression[List[ReturnT]]])
  }

  override def toString: String =  "sum(" + expr.toString + ")"
  override def evalToString: String =  "sum(" + expr.evalToString + ")"

  override def eval(context: Map[String, Any]): ReturnT = {
    val numeric = implicitly[Numeric[ReturnT]]
    var listToSum = expr.eval(context)
    listToSum.foldLeft(numeric.zero) { (accumulator, element) =>
      numeric.plus(accumulator, element)
    }
    numeric.asInstanceOf[ReturnT]

  }
  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)
    val singleOutputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = List(listInputType), output = singleOutputType)
  }

}

object SumExpression {
  object IntListSumFactory extends Creatable {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "SumExpression.create requires one child.")
      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      SumExpression[Integer](child)
    }
  }
}

case class CountExpression(
                            expr: Expression[? <: List[Any]]
                          ) extends Expression[Integer]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(expr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "CountExpression requires one child.")
    this.copy(expr = newChildren.head.asInstanceOf[Expression[List[?]]])
  }

  override def toString: String = s"count(${expr.toString})"

  override def evalToString: String = s"count(${expr.evalToString})"

  override def eval(context: Map[String, Any]): Integer = {
    val listToCount = expr.eval(context)
    listToCount.length
  }

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[Any]].runtimeClass)
    val singleOutputType = scalaTypeToExprType(classTag[Integer].runtimeClass)

    Signature(inputs = List(listInputType), output = singleOutputType)
  }
}

object CountExpression {
  object IntListCountFactory extends Creatable {
    override def templateSignature: Signature = {
      Signature(inputs = List(ListIntType), output = IntType)
    }

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "CountExpression.create requires one child (the list expression).")
      val child = children.head.asInstanceOf[Expression[List[Any]]]
      CountExpression(child)
    }
  }
}

/*
case Forall(variableName, start, end, body)
=>
val allTrue = (start to end).forall { i =>
  val newSolution = solution + (variableName -> i)
  eval(body, newSolution) != 0
}
if (allTrue) 1 else 0
*/

case class ForAllExpression[IterT](
                                    iteratorDef: IteratorDef[IterT], // This is now an Expression
                                    body: Expression[Boolean]
                                  ) extends Expression[Boolean] with ComposableExpression {

  override def children: List[Expression[?]] = List(iteratorDef, body)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2)
    this.copy(
      iteratorDef = newChildren(0).asInstanceOf[IteratorDef[IterT]],
      body = newChildren(1).asInstanceOf[Expression[Boolean]]
    )
  }

  override def toString: String = s"forall($iteratorDef)($body)"
  override def evalToString: String = s"forall($iteratorDef)($body)"

  override def eval(context: Map[String, Any]): Boolean = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.forall { item =>
      body.eval(context.+(variableName -> item))
    }
  }

  override def signature: Signature = Signature(inputs = List(ListIntType, BoolType), output = BoolType)
}


object ForAllExpression {
  object ForAllIntListFactory extends Creatable {

    override def templateSignature: Signature = Signature(inputs = List(ListIntType, BoolType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2)

      val collectionExpr = children(0).asInstanceOf[Expression[List[Integer]]]
      val bodyExpr = children(1).asInstanceOf[Expression[Boolean]]

      val iterator = IteratorDef(VarNameGenerator.generateUniqueName(), collectionExpr)

      ForAllExpression(iterator, bodyExpr)
    }
  }
}


//Based on ForAllExpression
case class ExistsExpression[IterT](
                                    iteratorDef: IteratorDef[IterT],
                                    body: Expression[Boolean]
                                  ) extends Expression[Boolean] with ComposableExpression {


  override def children: List[Expression[?]] = List(iteratorDef, body)


  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "ExistsExpression requires two children.")
    this.copy(
      iteratorDef = newChildren(0).asInstanceOf[IteratorDef[IterT]],
      body = newChildren(1).asInstanceOf[Expression[Boolean]]
    )
  }


  override def toString: String = s"exists($iteratorDef)($body)"

  override def evalToString: String = s"exists($iteratorDef)($body)"


  override def eval(context: Map[String, Any]): Boolean = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)


    itemsToIterate.exists { item =>
      body.eval(context.+(variableName -> item))
    }
  }

  override def signature: Signature = Signature(inputs = List(ListAnyType, BoolType), output = BoolType)
}

/* todo: based on forall add adding to context and globally unique name */
object ExistsExpression {
  object ExistsIntListFactory extends Creatable {

    override def templateSignature: Signature = Signature(inputs = List(ListAnyType, BoolType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "ExistsExpression.create requires two children.")

      val collectionExpr = children(0).asInstanceOf[Expression[List[Integer]]]
      val bodyExpr = children(1).asInstanceOf[Expression[Boolean]]

      val iterator = IteratorDef("j", collectionExpr)

      ExistsExpression(iterator, bodyExpr)
    }
  }
}



case class AllDifferentExpression(
                                   expr: Expression[? <: List[?]]
                                 ) extends Expression[Boolean]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(expr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "AllDifferentExpression requires one child.")
    this.copy(expr = newChildren.head.asInstanceOf[Expression[List[?]]])
  }

  override def toString: String = s"alldifferent(${expr.toString})"

  override def evalToString: String = s"alldifferent(${expr.evalToString})"

  override def eval(context: Map[String, Any]): Boolean = {
    val list = expr.eval(context)
    list.nonEmpty && (list.size == list.toSet.size)
  }

  override def distance(context: Map[String, Any]): Int = {
    val list = expr.eval(context)
    if (list.isEmpty) {
      0
    } else {
      val counts = list.groupBy(identity).view.mapValues(_.size).toMap
      counts.values.map(count => if (count > 1) count - 1 else 0).sum
    }
  }

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[?]].runtimeClass)
    Signature(inputs = List(listInputType), output = BoolType)
  }
}

object AllDifferentExpression {
  object ListAllDifferentFactory extends Creatable {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "AllDifferentExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[?]]]
      AllDifferentExpression(child)
    }
  }
}
/* todo: check this code
case class LexicographicalExpression[T : Ordering : ClassTag](
                                                               leftExpr: Expression[List[T]],
                                                               operator: BinaryOperator[Boolean],
                                                               rightExpr: Expression[List[T]]
                                                             ) extends Expression[Boolean]
  with OperatorContainer
  with ComposableExpression {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = newOp match {
    case op: BinaryOperator[Boolean] =>
      this.copy(operator = newOp.asInstanceOf[BinaryOperator[Boolean]])
    case _ =>
      throw new IllegalArgumentException(s"Cannot replace operator in LexicographicalExpression with non-boolean binary operator ${newOp}")
  }

  override def children: List[Expression[?]] = List(leftExpr, rightExpr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "LexicographicalExpression requires exactly two children for reconstruction")
    this.copy(
      leftExpr = newChildren(0).asInstanceOf[Expression[List[T]]],
      rightExpr = newChildren(1).asInstanceOf[Expression[List[T]]]
    )
  }

 
  override def toString: String = {
    val leftStr = leftExpr.toString
    val rightStr = rightExpr.toString
    
    operator.toString match {
      case "<" => s"lex_less($leftStr, $rightStr)"
      case "<=" => s"lex_leq($leftStr, $rightStr)"
      case ">" => s"lex_greater($leftStr, $rightStr)" 
      case ">=" => s"lex_greatereq($leftStr, $rightStr)"
      case "="  => s"$leftStr = $rightStr"
      case _ => throw new UnsupportedOperationException(s"Unsupported Lexicographical operator for export: ${operator.toString}")
    }
  }


  override def evalToString: String = {
    val leftStr = leftExpr.evalToString
    val rightStr = rightExpr.evalToString

    operator.toString match {
      case "<" => s"lex_less($leftStr, $rightStr)"
      case "<=" => s"lex_leq($leftStr, $rightStr)"
      case ">" => s"lex_greater($leftStr, $rightStr)"
      case ">=" => s"lex_greatereq($leftStr, $rightStr)"
      case "="  => s"$leftStr = $rightStr"
      case _ => throw new UnsupportedOperationException(s"Unsupported Lexicographical operator: ${operator.toString}")
    }
  }

  override def eval(context: Map[String, Any]): Boolean = {
    val leftList = leftExpr.eval(context)
    val rightList = rightExpr.eval(context)

    implicit val ordering: Ordering[List[T]] = Ordering.Implicits.seqOrdering
    val comparisonResult = ordering.compare(leftList, rightList)


    operator.eval(comparisonResult, 0)
  }

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[T]].runtimeClass)
    Signature(inputs = List(listInputType, listInputType), output = BoolType)
  }
}

object LexicographicalExpression {
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable {
    override def templateSignature: Signature = op.signature
    override def create(operator: BinaryOperator[Boolean], children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "LexicographicalExpression.create requires two children.")

      val left = children(0).asInstanceOf[Expression[List[Integer]]]
      val right = children(1).asInstanceOf[Expression[List[Integer]]]


      LexicographicalExpression[Integer](left, operator, right)
    }
  }
}
*/

case class MinimumExpression[ReturnT : Numeric : ClassTag](
                              elements: Expression[List[ReturnT]]
                            ) extends Expression[ReturnT]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(elements)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "MinimumExpression requires one child.")
    this.copy(elements = newChildren.head.asInstanceOf[Expression[List[ReturnT]]])
  }
  override def toString: String = s"min(${elements.toString})"

  override def evalToString: String = s"min(${elements.evalToString})"

  override def eval(context: Map[String, Any]): ReturnT = {

    val evaluatedList: List[ReturnT] = elements.eval(context)

    val numeric = implicitly[Numeric[ReturnT]]
    evaluatedList.min(numeric)
  }
  
  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)
    val outputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = List(listInputType), output = outputType)
  }
}


object MinimumExpression {
  object ListMinimumFactory extends Creatable {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "MinimumExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      MinimumExpression[Integer](child)
    }
  }
}

case class MaximumExpression[ReturnT : Numeric : ClassTag](
                              elements: Expression[List[ReturnT]]
                            ) extends Expression[ReturnT]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(elements)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "MaximumExpression requires one child.")
    this.copy(elements = newChildren.head.asInstanceOf[Expression[List[Integer]]])
  }

  override def toString: String = s"max(${elements.toString})"

  override def evalToString: String = s"max(${elements.evalToString})"

  override def eval(context: Map[String, Any]): ReturnT = {

    val evaluatedList: List[ReturnT] = elements.eval(context)
    val numeric = implicitly[Numeric[ReturnT]]

    evaluatedList.max(numeric)
  }

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)
    val outputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = List(listInputType), output = outputType)
  }
}

object MaximumExpression {
  object ListMaximumFactory extends Creatable {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "MaximumExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      MaximumExpression[Integer](child)
    }
  }
}


case class Task(
                 start: Expression[Integer],
                 duration: Expression[Integer],
                 demand: Expression[Integer]
               ) {

  override def toString: String =
    s"Task(${start.toString}, ${duration.toString}, ${demand.toString})"

  def evalToString: String =
    s"Task(${start.evalToString}, ${duration.evalToString}, ${demand.evalToString})"
}

/* todo: check this code
case class CumulativeExpression(
                                 tasks: Expression[List[Task]],
                                 operator: BinaryOperator[Boolean],
                                 limit: Expression[Integer]
                               ) extends Expression[Boolean]
  with OperatorContainer
  with ComposableExpression {

  override def withNewOperator(newOp: Operator[?]): Expression[?] = newOp match {
    case op: BinaryOperator[Boolean] =>
      this.copy(operator = newOp.asInstanceOf[BinaryOperator[Boolean]])
    case _ =>
      throw new IllegalArgumentException(s"Cannot replace operator in CumulativeExpression with non-binary operator ${newOp}")
  }

  override def children: List[Expression[?]] = List(tasks, limit)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "CumulativeExpression requires two children.")
    this.copy(
      tasks = newChildren(0).asInstanceOf[Expression[List[Task]]],
      limit = newChildren(1).asInstanceOf[Expression[Integer]]
    )
  }

  override def toString: String =
    s"cumulative(${tasks.toString}, ${operator.toString}, ${limit.toString})"

  override def evalToString: String =
    s"cumulative(${tasks.evalToString}, ${operator.toString}, ${limit.evalToString})"


  override def eval(context: Map[String, Any]): Boolean = {
    val taskList = tasks.eval(context)
    val cap = limit.eval(context).intValue()

    val evaluatedTasks = taskList.map { task =>
      (
        task.start.eval(context).intValue(),
        task.duration.eval(context).intValue(),
        task.demand.eval(context).intValue()
      )
    }

    if (evaluatedTasks.isEmpty) return true

    val s = evaluatedTasks.map(_._1)

    val minStart: Int = s.min
    val maxEnd: Int = evaluatedTasks.map { case (start, duration, _) => start + duration }.max

    (minStart to maxEnd).forall { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) =>
        time >= start && time < start + duration
      }.map { case (_, _, demand) => demand }.sum


      operator.eval(currentLoad, cap)
    }
  }


  override def distance(context: Map[String, Any]): Int = {

    val taskList = tasks.eval(context)
    val cap = limit.eval(context).intValue()

    val evaluatedTasks = taskList.map { task =>
      (
        task.start.eval(context).intValue(),
        task.duration.eval(context).intValue(),
        task.demand.eval(context).intValue()
      )
    }

    if (evaluatedTasks.isEmpty) return 0

    val s = evaluatedTasks.map(_._1)
    val minStart: Int = s.min
    val maxEnd: Int = evaluatedTasks.map { case (start, duration, _) => start + duration }.max


    (minStart to maxEnd).map { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) =>
        time >= start && time < start + duration
      }.map { case (_, _, demand) => demand }.sum

      Math.max(0, currentLoad - cap)
    }.sum
  }

  override def signature: Signature = {

    Signature(
      inputs = List(ListTaskType, IntType),
      output = BoolType
    )
  }
}

object CumulativeExpression {
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(ListTaskType, IntType),
        output = BoolType
      )

    override def create(operator: BinaryOperator[Boolean], children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "CumulativeExpression.create requires two children (Task list, limit).")

      val tasksExpr = children(0).asInstanceOf[Expression[List[Task]]]
      val limitExpr = children(1).asInstanceOf[Expression[Integer]]


      CumulativeExpression(tasksExpr, operator, limitExpr)
    }
  }
}

*/


case class GlobalCardinalityExpression(
                                        variablesExpr: Expression[List[Integer]],
                                        expectedCountsExpr: Expression[Map[Integer, Integer]]
                                      ) extends Expression[Boolean]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(variablesExpr, expectedCountsExpr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "GlobalCardinalityExpression requires two children.")
    this.copy(
      variablesExpr = newChildren(0).asInstanceOf[Expression[List[Integer]]],
      expectedCountsExpr = newChildren(1).asInstanceOf[Expression[Map[Integer, Integer]]]
    )
  }

  override def toString: String =
    s"global_cardinality(${variablesExpr.toString}, ${expectedCountsExpr.toString})"

  override def evalToString: String =
    s"global_cardinality(${variablesExpr.evalToString}, ${expectedCountsExpr.evalToString})"


  override def eval(context: Map[String, Any]): Boolean = {
    val list = variablesExpr.eval(context)
    val requiredCounts = expectedCountsExpr.eval(context)


    val actualFrequencies: Map[Integer, Int] =
      list.filter(requiredCounts.contains).groupBy(identity).view.mapValues(_.size).toMap


    requiredCounts.forall { case (value, requiredCount) =>
           val actualCount = actualFrequencies.getOrElse(value, 0)

      actualCount == requiredCount.intValue()
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    val list = variablesExpr.eval(context)
    val requiredCounts = expectedCountsExpr.eval(context)

    val actualFrequencies: Map[Integer, Int] =
      list.filter(requiredCounts.contains).groupBy(identity).view.mapValues(_.size).toMap

    requiredCounts.map { case (value, requiredCount) =>
      val actualCount = actualFrequencies.getOrElse(value, 0)

      val reqCountInt = requiredCount.intValue()

      Math.abs(actualCount - reqCountInt)
    }.sum
  }

  override def signature: Signature = {

    Signature(
      inputs = List(ListIntType, MapIntToIntType),
      output = BoolType
    )
  }
}

object GlobalCardinalityExpression {
  object GlobalCardinalityFactory extends Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(ListAnyType, MapIntToIntType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "GlobalCardinalityExpression.create requires two children.")

      val listExpr = children(0).asInstanceOf[Expression[List[Integer]]]
      val valuesExpr = children(1).asInstanceOf[Expression[Map[Integer, Integer]]]


      GlobalCardinalityExpression(listExpr, valuesExpr)
    }
  }
}


case class RectDescriptor(
                           x: Expression[Integer],
                           y: Expression[Integer],
                           width: Expression[Integer],
                           height: Expression[Integer]
                         ) {

  override def toString: String =
    s"RectDescriptor(${x.toString}, ${y.toString}, ${width.toString}, ${height.toString})"

  def evalToString: String =
    s"RectDescriptor(${x.evalToString}, ${y.evalToString}, ${width.evalToString}, ${height.evalToString})"
}



case class DiffnExpression(
                            rectsExpr: Expression[List[RectDescriptor]] // Lista symbolicznych obiektów RectDescriptor
                          ) extends Expression[Boolean]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(rectsExpr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "DiffnExpression requires one child.")
    this.copy(rectsExpr = newChildren.head.asInstanceOf[Expression[List[RectDescriptor]]])
  }

  override def toString: String = s"diffn(${rectsExpr.toString})"

  override def evalToString: String = s"diffn(${rectsExpr.evalToString})"


  override def eval(context: Map[String, Any]): Boolean = {
    val rects = rectsExpr.eval(context)


    val evaluatedRects = rects.map { rect =>
      (
        rect.x.eval(context).intValue(),
        rect.y.eval(context).intValue(),
        rect.width.eval(context).intValue(),
        rect.height.eval(context).intValue()
      )
    }


    evaluatedRects.combinations(2).forall {
      case List((x1, y1, dx1, dy1), (x2, y2, dx2, dy2)) =>

        val noOverlapX = (x1 + dx1 <= x2) || (x2 + dx2 <= x1)

        val noOverlapY = (y1 + dy1 <= y2) || (y2 + dy2 <= y1)

        noOverlapX || noOverlapY
    }
  }


  override def distance(context: Map[String, Any]): Int = {
    val rects = rectsExpr.eval(context)
    val evaluatedRects = rects.map { rect =>
      (
        rect.x.eval(context).intValue(),
        rect.y.eval(context).intValue(),
        rect.width.eval(context).intValue(),
        rect.height.eval(context).intValue()
      )
    }

    evaluatedRects.combinations(2).count {
      case List((x1, y1, dx1, dy1), (x2, y2, dx2, dy2)) =>

        val overlapX = (x1 < x2 + dx2) && (x2 < x1 + dx1)

        val overlapY = (y1 < y2 + dy2) && (y2 < y1 + dy1)

        overlapX && overlapY
    }
  }

  override def signature: Signature = {
    Signature(
      inputs = List(ListRectType),
      output = BoolType
    )
  }
}


object DiffnExpression {
  object DiffnFactory extends Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(ListRectType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "DiffnExpression.create requires one child.")

      val rectsExpr = children.head.asInstanceOf[Expression[List[RectDescriptor]]]
      DiffnExpression(rectsExpr)
    }
  }
}



case class ValuePrecedesChainExpression(
                                         variablesExpr: Expression[List[Integer]],
                                         valuesToPrecedeExpr: Expression[List[Integer]]
                                       ) extends Expression[Boolean]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(variablesExpr, valuesToPrecedeExpr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "ValuePrecedesChainExpression requires two children.")
    this.copy(
      variablesExpr = newChildren(0).asInstanceOf[Expression[List[Integer]]],
      valuesToPrecedeExpr = newChildren(1).asInstanceOf[Expression[List[Integer]]]
    )
  }

  override def toString: String =
    s"value_precede_chain(${variablesExpr.toString}, ${valuesToPrecedeExpr.toString})"

  override def evalToString: String = {
       val variablesStr = variablesExpr.evalToString
    val valuesStr = valuesToPrecedeExpr.evalToString
    s"value_precede_chain($variablesStr, $valuesStr)"
  }


  override def eval(context: Map[String, Any]): Boolean = {
    val list = variablesExpr.eval(context)
    val valuesToPrecede = valuesToPrecedeExpr.eval(context)

    valuesToPrecede.sliding(2).forall {
      case List(valA, valB) =>

        val lastAIndexValue: Int = list.lastIndexWhere(_ == valA)
        val lastAIndex: Option[Int] = lastAIndexValue match {
          case -1 => None
          case i => Some(i)
        }

        val firstBIndexValue: Int = list.indexOf(valB)
        val firstBIndex: Option[Int] = firstBIndexValue match {
          case -1 => None
          case i => Some(i)
        }

        (lastAIndex, firstBIndex) match {
          case (None, _) => true
          case (_, None) => true
          case (Some(lastA), Some(firstB)) =>

            lastA < firstB
        }
      case _ => true
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    if (eval(context)) return 0
    val list = variablesExpr.eval(context)
    val valuesToPrecede = valuesToPrecedeExpr.eval(context)

    valuesToPrecede.sliding(2).map {
      case List(valA, valB) =>

        val lastAIndexValue: Int = list.lastIndexWhere(_ == valA)
        val lastAIndex: Option[Int] = lastAIndexValue match {
          case -1 => None
          case i => Some(i)
        }

        val firstBIndexValue: Int = list.indexOf(valB)
        val firstBIndex: Option[Int] = firstBIndexValue match {
          case -1 => None
          case i => Some(i)
        }

        (lastAIndex, firstBIndex) match {
          case (Some(lastA), Some(firstB)) =>
            Math.max(0, lastA - firstB + 1)

          case _ => 0
        }
      case _ => 0
    }.sum
  }

  override def signature: Signature = {
    Signature(
      inputs = List(ListIntType, ListIntType),
      output = BoolType
    )
  }
}


object ValuePrecedesChainExpression {

  object ValuePrecedesChainFactory extends Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(ListIntType, ListIntType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "ValuePrecedesChainExpression.create requires two child.")

      val variablesExpr = children(0).asInstanceOf[Expression[List[Integer]]]
      val valuesToPrecedeExpr = children(1).asInstanceOf[Expression[List[Integer]]] // Drugie dziecko

      ValuePrecedesChainExpression(variablesExpr, valuesToPrecedeExpr)
    }
  }
}


case class RedundantConstraint[T](
                                   innerConstraint: Expression[T]
                                 ) extends Expression[T]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(innerConstraint)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1, "RedundantConstraint requires exactly one child.")
    this.copy(innerConstraint = newChildren.head.asInstanceOf[Expression[T]])
  }

  override def toString: String =
    s"redundant_constraint(${innerConstraint.toString})"

  override def evalToString: String =
    s"redundant_constraint(${innerConstraint.evalToString})"

  override def eval(context: Map[String, Any]): T = {
    innerConstraint.eval(context)
  }


  override def distance(context: Map[String, Any]): Int = {
    innerConstraint.distance(context)
  }

  override def signature: Signature = {
    Signature(
      inputs = List(UnknownType),
      output = BoolType
    )
  }
}

object RedundantConstraint {
  object RedundantConstraintFactory extends Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(UnknownType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "RedundantConstraintExpression.create requires one child.")

      val innerConstraintExpr = children.head.asInstanceOf[Expression[Any]]

      RedundantConstraint(innerConstraintExpr)
    }
  }
}


case class StrEqExpression(
                            s1: Expression[String],
                            s2: Expression[String]
                          ) extends Expression[Boolean]
  with ComposableExpression {

  override def children: List[Expression[?]] = List(s1, s2)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "StrEqExpression requires two children.")
    this.copy(
      s1 = newChildren(0).asInstanceOf[Expression[String]],
      s2 = newChildren(1).asInstanceOf[Expression[String]]
    )
  }

  override def toString: String =
    s"str_eq(${s1.toString}, ${s2.toString})"

  override def evalToString: String =
    s"str_eq(${s1.evalToString}, ${s2.evalToString})"


  override def eval(context: Map[String, Any]): Boolean = {
    val val1 = s1.eval(context)
    val val2 = s2.eval(context)

    val1.equals(val2)
  }


  override def distance(context: Map[String, Any]): Int = {
    if (eval(context)) 0 else 1
  }

  override def signature: Signature = {
    Signature(
      inputs = List(StringType, StringType),
      output = BoolType
    )
  }
}

object StrEqExpression {
  object StrEqFactory extends Creatable {
    override def templateSignature: Signature =
      Signature(
        inputs = List(StringType, StringType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "StrEqExpression.create requires two children.")

      val s1Expr = children(0).asInstanceOf[Expression[String]]
      val s2Expr = children(1).asInstanceOf[Expression[String]]

      StrEqExpression(s1Expr, s2Expr)
    }
  }
}