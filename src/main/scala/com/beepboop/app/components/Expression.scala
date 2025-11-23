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


sealed trait Expression[ReturnT] extends LogTrait{
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
  override def toString: String = value.toString
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
  override def evalToString: String = s"${variableName} in ${collection}"

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