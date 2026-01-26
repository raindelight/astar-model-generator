package com.beepboop.app.components

import com.beepboop.app.components.*
import com.beepboop.app.dataprovider.{DataProvider, VarNameGenerator}
import com.beepboop.app.logger.{LogTrait, Profiler}
import com.beepboop.app.mutations.{ContextAwareCreatable, GenerationContext}
import com.beepboop.app.policy.{EnsureSpecificVarExists, NoDuplicateVar, Policy}
import com.beepboop.app.postprocessor.Postprocessor
import com.beepboop.app.utils.Implicits.integerNumeric

import java.lang.Integer.sum
import java.util
import scala.reflect.ensureAccessible


/* third party modules */
import scala.reflect.{ClassTag, classTag}

def stringWithSpaces(strings: String*): String = {
  strings.mkString(" ")
}

trait ScopeModifier {
  def getAdditionalPolicies: List[Policy]
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

trait AutoNamed {
  protected def ownerClass: Class[_]
  override def toString: String = ownerClass.getSimpleName.stripSuffix("$")
}

abstract class Expression[ReturnT](implicit val ct: ClassTag[ReturnT]) extends LogTrait with Serializable {
  var creatorInfo: String = "Unknown"
  def toString: String
  def eval(context: Map[String, Any]): ReturnT
  def evalToString: String
  def signature: Signature
  def distance(context: Map[String, Any]): Int = {
    0
  }

  def complexity: Int = this match {
    case c: ComposableExpression =>
      1 + c.children.map (_.complexity).sum
    case _ => 1
  }

  def depth: Int = this match {
    case c: ComposableExpression if c.children.nonEmpty =>
      1 + c.children.map(_.depth).max
    case _ => 1
  }

  def structuralSignature: String = this match {
    case v: Variable[_] => "VAR"
    case c: Constant[_] => "CONST"
    case comp: ComposableExpression =>
      val opName = comp match {
        case oc: OperatorContainer => oc.operator.toString
        case _ => comp.getClass.getSimpleName
      }
      s"$opName(${comp.children.map(_.structuralSignature).mkString(",")})"
    case _ => "UNKNOWN"
  }
  def symbolCount: Int = this match {
    case c: ComposableExpression => c.children.map(_.symbolCount).sum
    case _ => 1
  }
}

case class Variable[ReturnT : ClassTag ](name: String, domain: Option[Expression[?]] = None) extends Expression[ReturnT] {
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

  def getOffset(context: Map[String, Any]): Int = domain match {
    case Some(expr) =>
      expr.eval(context) match {
        case (start: Int, _) => start
        case list: List[Int] @unchecked if list.nonEmpty => list.min
        case _ => 0
      }
    case None => 0
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
  def asCreatable[T: ClassTag](randomValueGenerator: () => T): Creatable = new Creatable with AutoNamed {
    override def templateSignature: Signature = {
      val outputType = scalaTypeToExprType(classTag[T].runtimeClass)
      Signature(inputs = Nil, output = outputType)
    }

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.isEmpty, "Constant requires no children.")
      Constant(randomValueGenerator())
    }

    override def ownerClass: Class[_] = Constant.getClass
  }
}

case class ArrayElement[ReturnT : ClassTag](
                                           variable: Expression[List[ReturnT]],
                                           index: Expression[Integer]
                                           ) extends Expression[ReturnT] with ComposableExpression {
  override def children: List[Expression[?]] = List(variable, index)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2, "ArrayElement requires exactly two children.")
    this.copy(variable = newChildren.head.asInstanceOf[Expression[List[ReturnT]]],
      index = newChildren(1).asInstanceOf[Expression[Integer]])
  }

  override def eval(context: Map[String, Any]): ReturnT = {
    try {
      val rawIndex = index.eval(context)
      val offset = index match {
        case v: Variable[Integer] @unchecked => v.getOffset(context)
        case _ => 0
      }
      val adjustedIndex = rawIndex - offset
      variable.eval(context).apply(adjustedIndex)
    } catch {
      case e: IndexOutOfBoundsException =>
        val hasDomain = index match {
          case v: Variable[_] => v.domain.isDefined
          case _ => false
        }

        if (!hasDomain) {
          Profiler.recordValue("Index out of bounds (ArrayElement without Domain)", 1)
        } else {
          Profiler.recordValue("Index out of bounds (ArrayElement with Domain)", 1)
        }
        throw e
      case e: Exception =>
        throw e
    }
  }
  override def toString: String = s"${variable.toString}[${index.toString}]"
  override def evalToString: String = s"${variable.toString}[${index.evalToString}]"

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)
    val intInputType = scalaTypeToExprType(classTag[Integer].runtimeClass)
    val singleOutputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = List(listInputType,  intInputType), singleOutputType)
  }

}

object ArrayElement {
  def asCreatable[T: ClassTag](): Creatable = new Creatable with AutoNamed {
    override def templateSignature: Signature = {
      val listInputType = scalaTypeToExprType(classTag[List[T]].runtimeClass)
      val intInputType = scalaTypeToExprType(classOf[Integer])
      val singleOutputType = scalaTypeToExprType(classTag[T].runtimeClass)
      Signature(inputs = List(listInputType, intInputType), output = singleOutputType)
    }

    override def create(children: List[Expression[?]]): Expression[T] = {
      require(children.length == 2, "ArrayElement requires two children.")

      val arrayExpr = children(0)
      val expectedListType = templateSignature.inputs.head

      if (arrayExpr.signature.output != expectedListType) {

        throw new ClassCastException(
          s"Cannot create ArrayElement returning ${templateSignature.output} " +
            s"using a collection of type ${arrayExpr.signature.output}"
        )
      }

      ArrayElement[T](
        arrayExpr.asInstanceOf[Expression[List[T]]],
        children(1).asInstanceOf[Expression[Integer]]
      )
    }

    override def ownerClass: Class[_] = ArrayElement.getClass // [cite: 180]
  }
}

case class IteratorDef[IterT : ClassTag](
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

case class BinaryExpression[ReturnT : ClassTag](
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
      s"Left child output data type doesn't match required operator signature input, (new)${newChildren.head.signature.output} != ${operator.signature.inputs.head} for ${this.operator.toString} [${this.toString}] (${newChildren}) | withNewChildren"
    )

    require(
      newChildren(1).signature.output == operator.signature.inputs(1),
      s"Right child output data type doesn't match required operator signature input, (new)${newChildren(1).signature.output} != ${operator.signature.inputs(1)} for ${this.operator.toString} " +
        s" [${this.toString}] (${newChildren}) | withNewChildren"
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
    val leftVal = left.eval(context)
    val rightVal = right.eval(context)
    val isSatisfied = operator.eval(leftVal, rightVal).asInstanceOf[Boolean]


    operator match {
      case _: XorOperator[_] | _: EqualOperator[_] | _: NotEqualOperator[_]
        if leftVal.isInstanceOf[Boolean] && rightVal.isInstanceOf[Boolean] =>

        val lDist = left.distance(context)
        val rDist = right.distance(context)

        if (isSatisfied) {
          operator match {
            case _: XorOperator[_] =>
              Math.min(lDist, rDist)
            case _ =>
              Math.max(lDist, rDist)
          }
        } else {
          1
        }
    case _: AndOperator[_] =>
        left.distance(context) + right.distance(context)
      case _: OrOperator[_] =>
        Math.min(left.distance(context), right.distance(context))
      case _: ImpliesOperator[_] =>
        val pVal = left.eval(context).asInstanceOf[Boolean]
        val qVal = right.eval(context).asInstanceOf[Boolean]

        val distToFlipP = left.distance(context)
        val distToFlipQ = right.distance(context)

        if (pVal && !qVal) {
          Math.min(distToFlipP, distToFlipQ)
        } else {
          Math.max(distToFlipP, distToFlipQ)
        }
      case _ =>
        operator.distance(left.eval(context), right.eval(context))
    }
  }
}

object BinaryExpression {
  def asCreatable[T](op: BinaryOperator[T]): Creatable = new Creatable with AutoNamed {
    override def toString: String = op.getClass.getSimpleName.stripSuffix("$")
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      require(children.length == 2)
      implicit val tag: ClassTag[T] = op.ct

      require(
        children(0).signature.output == op.signature.inputs(0),
        s"Left child output data type doesn't match required operator signature input, ${children(0).signature.output} != ${op.signature.inputs(0)} for ${op.toString}"
      )
      require(
        children(1).signature.output == op.signature.inputs(1),
        s"Right child output data type doesn't match required operator signature input, ${children(1).signature.output} != ${op.signature.inputs(1)} for ${op.toString}"
      )
      BinaryExpression(children(0).asInstanceOf[Expression[T]] , op, children(1).asInstanceOf[Expression[T]])
    }

    override def ownerClass: Class[_] = BinaryExpression.getClass
  }
}

case class UnaryExpression[ReturnT : ClassTag](
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
  def asCreatable[T](op: UnaryOperator[T]): Creatable = new Creatable with AutoNamed {
    override def toString: String = op.getClass.getSimpleName.stripSuffix("$")
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[T] = {
      implicit val tag: ClassTag[T] = op.ct
      require(children.length == 1)
      require(children(0).signature.output == op.signature.inputs(0), s"Child output data type doesn't match required operator signature input, ${children(0).signature.output} != ${op.signature.inputs(0)} for ${op.toString}")
      UnaryExpression(children(0), op)
    }
    override def ownerClass: Class[_] = UnaryExpression.getClass
  }
}

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
  }
  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[ReturnT]].runtimeClass)
    val singleOutputType = scalaTypeToExprType(classTag[ReturnT].runtimeClass)
    Signature(inputs = List(listInputType), output = singleOutputType)
  }

}

object SumExpression {
  object IntListSumFactory extends Creatable with AutoNamed {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "SumExpression.create requires one child.")
      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      SumExpression[Integer](child)
    }

    override protected def ownerClass: Class[_] = SumExpression.getClass
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
  object IntListCountFactory extends Creatable with AutoNamed {
    override def templateSignature: Signature = {
      Signature(inputs = List(ListIntType), output = IntType)
    }

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "CountExpression.create requires one child (the list expression).")
      val child = children.head.asInstanceOf[Expression[List[Any]]]
      CountExpression(child)
    }
    override protected def ownerClass: Class[_] = CountExpression.getClass
  }
}

case class ForAllExpression[IterT](
                            iteratorDef: IteratorDef[IterT], // This is now an Expression
                            body: Expression[Boolean]
                          ) extends Expression[Boolean]
                          with ComposableExpression
                          with ScopeModifier {

  override def children: List[Expression[?]] = List(iteratorDef, body)
  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 2)
    this.copy(
      iteratorDef = newChildren(0).asInstanceOf[IteratorDef[IterT]],
      body = newChildren(1).asInstanceOf[Expression[Boolean]]
    )
  }

  override def getAdditionalPolicies: List[Policy] = {
    List(EnsureSpecificVarExists(iteratorDef.variableName))
  }

  override def toString: String = s"forall($iteratorDef)($body)"
  override def evalToString: String = s"forall($iteratorDef)($body)"

  override def eval(context: Map[String, Any]): Boolean = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.forall { item =>
      body.eval(context.+(variableName -> item))
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    itemsToIterate.map(item => body.distance(context.+(variableName -> item))).sum
  }

  override def signature: Signature = Signature(inputs = List(ListIntType, BoolType), output = BoolType)
}


object ForAllExpression {
  object ForAllIntListFactory extends Creatable with AutoNamed with ContextAwareCreatable {

    override def templateSignature: Signature = Signature(List(ListIntType, BoolType), BoolType)

    override def create(children: List[Expression[?]]): Expression[?] =
      throw new UnsupportedOperationException("This factory requires context-aware generation.")

    override def generateExpression(
                                     ctx: GenerationContext,
                                     recurse: (ExpressionType, GenerationContext) => Option[Expression[?]]
                                   ): Option[Expression[?]] = {

      val collectionOpt = recurse(ListIntType, ctx).map(_.asInstanceOf[Expression[List[Integer]]])
      if (collectionOpt.isEmpty) return None

      val varName = VarNameGenerator.generateUniqueName("f")
      val newCtx = ctx.withVariable(varName, IntType, collectionOpt.get)

      val bodyOpt = recurse(BoolType, newCtx).map(_.asInstanceOf[Expression[Boolean]])

      if (bodyOpt.isEmpty) return None

      val iterator = IteratorDef(varName, collectionOpt.get)
      Some(ForAllExpression(iterator, bodyOpt.get))
    }

    override def ownerClass: Class[_] = ForAllExpression.getClass
  }
}



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

  override def distance(context: Map[String, Any]): Int = {
    val (variableName, itemsToIterate) = iteratorDef.eval(context)
    if (itemsToIterate.isEmpty) 1 else itemsToIterate.map(item => body.distance(context.+(variableName -> item))).min
  }

  override def signature: Signature = Signature(inputs = List(ListAnyType, BoolType), output = BoolType)
}

/* todo: based on forall add adding to context and globally unique name */
object ExistsExpression {
  object ExistsIntListFactory extends Creatable with AutoNamed {

    override def templateSignature: Signature = Signature(inputs = List(ListAnyType, BoolType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "ExistsExpression.create requires two children.")

      val collectionExpr = children(0).asInstanceOf[Expression[List[Integer]]]
      val bodyExpr = children(1).asInstanceOf[Expression[Boolean]]

      val iterator = IteratorDef("j", collectionExpr)

      ExistsExpression(iterator, bodyExpr)
    }
    override def ownerClass: Class[_] = ForAllExpression.getClass
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
  object ListAllDifferentFactory extends Creatable with AutoNamed {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "AllDifferentExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[?]]]
      AllDifferentExpression(child)
    }
    override def ownerClass: Class[_] = AllDifferentExpression.getClass
  }
}

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

  override def signature: Signature = {
    val listInputType = scalaTypeToExprType(classTag[List[T]].runtimeClass)
    Signature(inputs = List(listInputType, listInputType), output = BoolType)
  }
}

object LexicographicalExpression {
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable with AutoNamed {
    override def templateSignature: Signature = op.signature
    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "LexicographicalExpression.create requires two children.")

      val left = children(0).asInstanceOf[Expression[List[Integer]]]
      val right = children(1).asInstanceOf[Expression[List[Integer]]]


      LexicographicalExpression[Integer](left, op, right)
    }

    override def ownerClass: Class[_] = LexicographicalExpression.getClass

  }
}


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
  object ListMinimumFactory extends Creatable with AutoNamed {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "MinimumExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      MinimumExpression[Integer](child)
    }
    override def ownerClass: Class[_] = MinimumExpression.getClass
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
  object ListMaximumFactory extends Creatable with AutoNamed {
    override def templateSignature: Signature = Signature(inputs = List(ListIntType), output = IntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "MaximumExpression.create requires one child.")

      val child = children.head.asInstanceOf[Expression[List[Integer]]]
      MaximumExpression[Integer](child)
    }
    override def ownerClass: Class[_] = MaximumExpression.getClass
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

    if (taskList.isEmpty) return true

    val evaluatedTasks = taskList.map { task =>
      (
        task.start.eval(context).intValue(),
        task.duration.eval(context).intValue(),
        task.demand.eval(context).intValue()
      )
    }

    val s = evaluatedTasks.map(_._1)

    val minStart: Int = s.min
    val maxEnd: Int = evaluatedTasks.map { case (start, duration, _) => start + duration }.max

    (minStart until maxEnd).forall { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) =>
        time >= start && time < start + duration
      }.map { case (_, _, demand) => demand }.sum


      operator.eval(currentLoad, cap)
    }
  }


  override def distance(context: Map[String, Any]): Int = {

    val taskList = tasks.eval(context)
    val cap = limit.eval(context).intValue()

    if (taskList.isEmpty) return 0

    val evaluatedTasks = taskList.map { task =>
      (
        task.start.eval(context).intValue(),
        task.duration.eval(context).intValue(),
        task.demand.eval(context).intValue()
      )
    }

    val s = evaluatedTasks.map(_._1)
    val minStart: Int = s.min
    val maxEnd: Int = evaluatedTasks.map { case (start, duration, _) => start + duration }.max


    (minStart until maxEnd).map { time =>
      val currentLoad = evaluatedTasks.filter { case (start, duration, _) =>
        time >= start && time < start + duration
      }.map { case (_, _, demand) => demand }.sum

      operator.distance(currentLoad, cap)
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
  def asCreatable(op: BinaryOperator[Boolean]): Creatable = new Creatable with AutoNamed {
    override def templateSignature: Signature =
      Signature(
        inputs = List(ListTaskType, IntType),
        output = BoolType
      )

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 2, "CumulativeExpression.create requires two children (Task list, limit).")

      val tasksExpr = children(0).asInstanceOf[Expression[List[Task]]]
      val limitExpr = children(1).asInstanceOf[Expression[Integer]]


      CumulativeExpression(tasksExpr, op, limitExpr)
    }
    override def ownerClass: Class[_] = CumulativeExpression.getClass
  }
}


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
  object GlobalCardinalityFactory extends Creatable with AutoNamed {
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

    override def ownerClass: Class[_] = GlobalCardinalityExpression.getClass
  }
}


case class DiffnExpression(
                            x: Expression[List[Integer]],
                            y: Expression[List[Integer]],
                            dx: Expression[List[Integer]],
                            dy: Expression[List[Integer]]
                          ) extends Expression[Boolean]
  with ScopeModifier
  with ComposableExpression {

  override def children: List[Expression[?]] = List(x, y, dx, dy)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 4, "DiffnExpression requires exactly four children (x, y, dx, dy) for reconstruction")
    DiffnExpression(
      newChildren(0).asInstanceOf[Expression[List[Integer]]],
      newChildren(1).asInstanceOf[Expression[List[Integer]]],
      newChildren(2).asInstanceOf[Expression[List[Integer]]],
      newChildren(3).asInstanceOf[Expression[List[Integer]]]
    )
  }

  override def getAdditionalPolicies: List[Policy] = {
    List(NoDuplicateVar())
  }

  override def eval(context: Map[String, Any]): Boolean = {
    try {

      val rawX = x.eval(context)
      val rawY = y.eval(context)
      val rawDx = dx.eval(context)
      val rawDy = dy.eval(context)


      def getTypeName(l: List[Any]): String =
        if (l.isEmpty) "Empty" else l.head.getClass.getSimpleName



      val xVal = rawX.map(_.intValue())
      val yVal = rawY.map(_.intValue())
      val dxVal = rawDx.map(_.intValue())
      val dyVal = rawDy.map(_.intValue())

      val n = xVal.size

      require(yVal.size == n && dxVal.size == n && dyVal.size == n, "All input arrays for diffn must have the same length")

      for (i <- 0 until n; j <- (i + 1) until n) {
        if (rectanglesOverlap(
          xVal(i), yVal(i), dxVal(i), dyVal(i),
          xVal(j), yVal(j), dxVal(j), dyVal(j)
        )) {
            return false
        }
      }
      true
    } catch {
      case e: Exception =>
        error(e.getMessage)
        throw e
    }
  }

  override def distance(context: Map[String, Any]): Int = {
    try {
      val rawX = x.eval(context)
      val rawY = y.eval(context)
      val rawDx = dx.eval(context)
      val rawDy = dy.eval(context)

      val xVal = rawX.map(_.intValue())
      val yVal = rawY.map(_.intValue())
      val dxVal = rawDx.map(_.intValue())
      val dyVal = rawDy.map(_.intValue())

      val n = xVal.size
      var totalOverlapArea = 0

      for (i <- 0 until n; j <- (i + 1) until n) {
        totalOverlapArea += intersectionArea(
          xVal(i), yVal(i), dxVal(i), dyVal(i),
          xVal(j), yVal(j), dxVal(j), dyVal(j)
        )
      }

      totalOverlapArea

    } catch {
      case e: Exception =>
        throw e
    }
  }

  private def rectanglesOverlap(x1: Int, y1: Int, w1: Int, h1: Int,
                                x2: Int, y2: Int, w2: Int, h2: Int): Boolean = {
    if (x1 + w1 <= x2 || x2 + w2 <= x1) return false
    if (y1 + h1 <= y2 || y2 + h2 <= y1) return false
    true
  }

  private def intersectionArea(x1: Int, y1: Int, w1: Int, h1: Int,
                               x2: Int, y2: Int, w2: Int, h2: Int): Int = {
    val xOverlap = Math.max(0, Math.min(x1 + w1, x2 + w2) - Math.max(x1, x2))
    val yOverlap = Math.max(0, Math.min(y1 + h1, y2 + h2) - Math.max(y1, y2))
    xOverlap * yOverlap
  }

  override def toString: String = stringWithSpaces("diffn(", x.toString, ",", y.toString, ",", dx.toString, ",", dy.toString, ")")

  override def evalToString: String = stringWithSpaces("diffn(", x.evalToString, ",", y.evalToString, ",", dx.evalToString, ",", dy.evalToString, ")")

  override def signature: Signature =
    val outputType = scalaTypeToExprType(classTag[Boolean].runtimeClass)
    Signature(
      inputs = List(
        x.signature.output,
        y.signature.output,
        dx.signature.output,
        dy.signature.output
      ),
      output = outputType
    )
}

object DiffnExpression {
  object DiffnFactory extends Creatable with AutoNamed {

    override def templateSignature: Signature = {
      Signature(
        inputs = List(ListIntType, ListIntType, ListIntType, ListIntType),
        output = BoolType
      )
    }

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 4, "DiffnExpression.create requires exactly four children (x, y, dx, dy).")

      DiffnExpression(
        children(0).asInstanceOf[Expression[List[Integer]]],
        children(1).asInstanceOf[Expression[List[Integer]]],
        children(2).asInstanceOf[Expression[List[Integer]]],
        children(3).asInstanceOf[Expression[List[Integer]]]
      )
    }

    override protected def ownerClass: Class[_] = DiffnExpression.getClass
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
            if (lastA >= firstB) lastA - firstB + 1 else 0

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

  object ValuePrecedesChainFactory extends Creatable with AutoNamed {
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

    override def ownerClass: Class[_] = ValuePrecedesChainExpression.getClass
  }
}


case class RedundantConstraint[T : ClassTag](
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
  object StrEqFactory extends Creatable with AutoNamed {
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

    override def ownerClass: Class[_] = StrEqExpression.getClass
  }
}


case class SetComprehensionExpression[T: ClassTag](
                                                    head: Expression[T],
                                                    iteratorDef: IteratorDef[Integer],
                                                    filter: Expression[Boolean]
                                                  ) extends Expression[List[T]]
  with ComposableExpression
  with ScopeModifier {

  override def children: List[Expression[?]] = List(head, iteratorDef, filter)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 3, "SetComprehensionExpression requires 3 children")
    this.copy(
      head = newChildren(0).asInstanceOf[Expression[T]],
      iteratorDef = newChildren(1).asInstanceOf[IteratorDef[Integer]],
      filter = newChildren(2).asInstanceOf[Expression[Boolean]]
    )
  }

  override def getAdditionalPolicies: List[Policy] = {
    List(EnsureSpecificVarExists(iteratorDef.variableName))
  }

  override def eval(context: Map[String, Any]): List[T] = {
    val (varName, domain) = iteratorDef.eval(context)

    domain.flatMap { item =>
      val localContext = context + (varName -> item)
      if (filter.eval(localContext)) {
        Some(head.eval(localContext))
      } else {
        None
      }
    }
  }

  override def distance(context: Map[String, Any]): Int = 0

  override def toString: String = s"{ $head | $iteratorDef where $filter }"
  override def evalToString: String = s"{ $head | $iteratorDef where $filter }"

  override def signature: Signature = {
    val outputType = scalaTypeToExprType(classTag[List[T]].runtimeClass)
    Signature(inputs = Nil, output = outputType)
  }
}

object SetComprehensionExpression {
  object IntSetComprehensionFactory extends Creatable with AutoNamed with ContextAwareCreatable {

    override def templateSignature: Signature =
      Signature(
        inputs = List(ListIntType, BoolType, IntType),
        output = ListIntType
      )

    override def create(children: List[Expression[?]]): Expression[?] =
      throw new UnsupportedOperationException("SetComprehension requires context-aware generation.")

    override def generateExpression(
                                     ctx: GenerationContext,
                                     recurse: (ExpressionType, GenerationContext) => Option[Expression[?]]
                                   ): Option[Expression[?]] = {

      val collectionOpt = recurse(ListIntType, ctx).map(_.asInstanceOf[Expression[List[Integer]]])
      if (collectionOpt.isEmpty) return None

      val varName = VarNameGenerator.generateUniqueName("idx")
      val newCtx = ctx.withVariable(varName, IntType, collectionOpt.get)

      val filterOpt = recurse(BoolType, newCtx).map(_.asInstanceOf[Expression[Boolean]])
      if (filterOpt.isEmpty) return None

      val headOpt = recurse(IntType, newCtx).map(_.asInstanceOf[Expression[Integer]])
      if (headOpt.isEmpty) return None

      val iterator = IteratorDef(varName, collectionOpt.get)

      Some(SetComprehensionExpression(headOpt.get, iterator, filterOpt.get))
    }

    override def ownerClass: Class[_] = SetComprehensionExpression.getClass
  }
}

case class AllDifferentExceptZeroExpression(
                                             expr: Expression[List[Integer]]
                                           ) extends Expression[Boolean] with ComposableExpression {

  override def children: List[Expression[?]] = List(expr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1)
    AllDifferentExceptZeroExpression(newChildren.head.asInstanceOf[Expression[List[Integer]]])
  }

  override def eval(context: Map[String, Any]): Boolean = {
    val list = expr.eval(context)
    val nonZero = list.filter(_ != 0)
    nonZero.distinct.size == nonZero.size
  }

  override def distance(context: Map[String, Any]): Int = {
    val list = expr.eval(context)
    val nonZero = list.filter(_ != 0)

    val counts = nonZero.groupBy(identity).view.mapValues(_.size).toMap
    counts.values.map(c => if (c > 1) c - 1 else 0).sum
  }

  override def toString: String = s"alldifferent_except_0($expr)"
  override def evalToString: String = s"alldifferent_except_0(${expr.evalToString})"

  override def signature: Signature = Signature(inputs = List(ListIntType), output = BoolType)
}


object AllDifferentExceptZeroExpression {
  object Factory extends Creatable with AutoNamed {

    override def templateSignature: Signature =
      Signature(inputs = List(ListIntType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "AllDifferentExceptZero requires one child (list).")

      val listExpr = children.head.asInstanceOf[Expression[List[Integer]]]
      AllDifferentExceptZeroExpression(listExpr)
    }

    override def ownerClass: Class[_] = AllDifferentExceptZeroExpression.getClass
  }
}

case class ArgSortExpression(
                              listExpr: Expression[List[Integer]]
                            ) extends Expression[List[Integer]] with ComposableExpression {

  override def children: List[Expression[?]] = List(listExpr)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1)
    ArgSortExpression(newChildren.head.asInstanceOf[Expression[List[Integer]]])
  }

  override def eval(context: Map[String, Any]): List[Integer] = {
    val list = listExpr.eval(context)

    list.zipWithIndex
      .sortBy(_._1.intValue())
      .map { case (_, index) => (index + 1).asInstanceOf[Integer] }
  }

  override def toString: String = s"arg_sort($listExpr)"
  override def evalToString: String = s"arg_sort(${listExpr.evalToString})"

  override def signature: Signature = Signature(inputs = List(ListIntType), output = ListIntType)
}

object ArgSortExpression {
  object Factory extends Creatable with AutoNamed {

    override def templateSignature: Signature =
      Signature(inputs = List(ListIntType), output = ListIntType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "ArgSort requires one child (list).")

      val listExpr = children.head.asInstanceOf[Expression[List[Integer]]]
      ArgSortExpression(listExpr)
    }

    override def ownerClass: Class[_] = ArgSortExpression.getClass
  }
}


case class SymmetryBreakingExpression(
                                       constraint: Expression[Boolean]
                                     ) extends Expression[Boolean] with ComposableExpression {

  override def children: List[Expression[?]] = List(constraint)

  override def withNewChildren(newChildren: List[Expression[?]]): Expression[?] = {
    require(newChildren.length == 1)
    SymmetryBreakingExpression(newChildren.head.asInstanceOf[Expression[Boolean]])
  }

  override def eval(context: Map[String, Any]): Boolean = constraint.eval(context)

  override def distance(context: Map[String, Any]): Int = constraint.distance(context)

  override def toString: String = s"symmetry_breaking_constraint($constraint)"
  override def evalToString: String = s"symmetry_breaking_constraint(${constraint.evalToString})"

  override def signature: Signature = constraint.signature
}

object SymmetryBreakingExpression {
  object Factory extends Creatable with AutoNamed {

    override def templateSignature: Signature =
      Signature(inputs = List(BoolType), output = BoolType)

    override def create(children: List[Expression[?]]): Expression[?] = {
      require(children.length == 1, "SymmetryBreaking requires one child (boolean).")

      val boolExpr = children.head.asInstanceOf[Expression[Boolean]]
      SymmetryBreakingExpression(boolExpr)
    }

    override def ownerClass: Class[_] = SymmetryBreakingExpression.getClass
  }
}
