package com.beepboop.app.components

/* third party modules */
// intelij can mark this as non found, but it works

/* own modules */
import com.beepboop.app.components.Operator
import com.beepboop.app.components.Expression
import com.beepboop.app.components.SetIntContainsInt
import com.beepboop.app.components.StrEqExpression.StrEqFactory
import com.beepboop.app.dataprovider.DataProvider
import com.beepboop.app.logger.LogTrait
import com.beepboop.app.utils.AppConfig
import org.yaml.snakeyaml.Yaml

import scala.jdk.CollectionConverters.*
import java.io.FileInputStream
import java.util.Map as JMap

sealed trait ExpressionType
case object IntType extends ExpressionType
case object BoolType extends ExpressionType
case object ListIntType extends ExpressionType
case object ListBoolType extends ExpressionType
case object SetIntType extends ExpressionType
case object IteratorType extends ExpressionType
case object UnknownType extends ExpressionType
case object ListTaskType extends ExpressionType
case object MapIntToIntType extends ExpressionType
case object ListAnyType extends ExpressionType
case object ListRectType extends ExpressionType
case object StringType extends ExpressionType
case object ListSetIntType extends ExpressionType

case class Signature(inputs: List[ExpressionType], output: ExpressionType)


def scalaTypeToExprType(cls: Class[?]): ExpressionType = cls match {
  case c if c == classOf[java.lang.Integer] || c == java.lang.Integer.TYPE =>
    IntType

  case c if c == classOf[java.lang.String] =>
    StringType

  case c if c == classOf[java.lang.Boolean] || c == java.lang.Boolean.TYPE =>
    BoolType
  case c if c == classOf[List[Integer]] =>
    ListIntType
  case c if c == classOf[List[Boolean]] =>
    ListBoolType

  case c if c == classOf[Set[Int]] || classOf[Set[?]].isAssignableFrom(c) =>
    SetIntType

  case c if c == classOf[List[Set[Integer]]] =>
    ListSetIntType

  case c if c == classOf[List[Task]] =>
    ListTaskType

  case c if c == classOf[Map[Integer, Integer]] =>
    MapIntToIntType

  case c if c == classOf[List[Any]] =>
    ListAnyType

  case _ =>
    throw new Exception(s"Unsupported type: ${cls.getName}")
}

private def toScala(value: Any): Any = value match {
  case m: java.util.Map[_, _] =>
    m.asScala.view.mapValues(toScala).toMap
  case l: java.util.List[_] =>
    l.asScala.map(toScala).toList
  case other => other
}

object ComponentRegistry extends LogTrait {

  private val binaryOperators: List[BinaryOperator[?]] = List(

    // arithmetic
    new AddOperator[Integer],
    new SubOperator[Integer],
    new MulOperator[Integer],
    //new DivOperator[Integer],
    //new ModOperator[Integer],


    // relational
    new EqualOperator[Integer],
    new EqualOperator[Boolean],
    new EqualOperator[List[Integer]],
    new EqualOperator[Set[Integer]],

    new NotEqualOperator[Integer](),
    new NotEqualOperator[Boolean](),
    new NotEqualOperator[List[Integer]](),
    new NotEqualOperator[Set[Integer]](), // todo: rewrite

    new LessOperator[Integer],
    new GreaterOperator[Integer],
    new LessEqualOperator[Integer],
    new GreaterEqualOperator[Integer],

    new AndOperator[Boolean],
    new OrOperator[Boolean],
    new XorOperator[Boolean],
    new ImpliesOperator[Boolean],

    new ContainsOperator[List[Integer], Integer],
    new ContainsOperator[Set[Int], Int]
  )

  private val unaryOperators: List[UnaryOperator[?]] = List(
    new NotOperator[Boolean](),
    new NegateOperator[Integer](),
    new AbsOperator[Integer](),
    new BoolToIntOperator[Boolean]()
    // todo: add another unary operator
  )

  private val allVariablesFactories: List[Creatable] = DataProvider.getVariableCreatables

  private val allOperators: List[Operator[?]] = binaryOperators ++ unaryOperators

  private val allConstantFactories: List[Creatable] = List(
    Constant.asCreatable[Integer](() => scala.util.Random.nextInt(10)),
    //Constant.asCreatable[Boolean](() => scala.util.Random.nextBoolean())
  )
  private val allArrayElementFactories: List[Creatable] = List(
    ArrayElement.asCreatable[Integer](),
    ArrayElement.asCreatable[List[Integer]]()
  )

  private val expressionFactories: List[Creatable] = List(
    SumExpression.IntListSumFactory,
    ForAllExpression.ForAllIntListFactory,
    CountExpression.IntListCountFactory,
    AllDifferentExpression.ListAllDifferentFactory,
    ExistsExpression.ExistsIntListFactory,
    MinimumExpression.ListMinimumFactory,
    MaximumExpression.ListMaximumFactory,
    GlobalCardinalityExpression.GlobalCardinalityFactory,
    DiffnExpression.DiffnFactory,
    ValuePrecedesChainExpression.ValuePrecedesChainFactory,
    StrEqExpression.StrEqFactory,
    AllDifferentExceptZeroExpression.Factory,
    ArgSortExpression.Factory,
    SymmetryBreakingExpression.Factory,
    SetComprehensionExpression.IntSetComprehensionFactory
    //CumulativeExpression
    //LexicographicalExpression.asCreatable()
  )


  val staticCreatables: List[Creatable] = (
      binaryOperators.map(op => BinaryExpression.asCreatable(op)) ++
      unaryOperators.map(op => UnaryExpression.asCreatable(op)) ++
      allConstantFactories ++
      expressionFactories ++
      allArrayElementFactories
    ).filter(c =>
    val className = c.toString
    AppConfig.getWeight(className) > 0.0
  )

  def creatables: List[Creatable] = {
    val currentVariables = DataProvider.getVariableCreatables
      .filter(v => v.templateSignature.output != UnknownType)
    val all = staticCreatables ++ currentVariables
    all
  }

  creatables.foreach { c =>
    val sig = c.templateSignature
    val inputs = sig.inputs.map(_.toString).mkString(", ")
    debug(s"COMPONENT: ${c.toString} | OUTPUT: ${sig.output} | INPUTS: ($inputs)")
  }




  def findCreatablesReturning(outputType: ExpressionType): List[Creatable] = {
    creatables.filter(_.templateSignature.output == outputType)
  }



  def findOperatorReturning(outputType: ExpressionType): List[Operator[?]] = {
    allOperators.filter(_.signature.output == outputType)
  }

  def findOperatorsForInputs(inputTypes: List[ExpressionType]): List[Operator[?]] = {
    allOperators.filter(_.signature.inputs == inputTypes)
  }

  def findOperatorsWithSignature(sig: Signature): List[Operator[?]] = {
    allOperators.filter(_.signature == sig)
  }

}
