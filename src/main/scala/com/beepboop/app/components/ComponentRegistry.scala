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
import org.yaml.snakeyaml.Yaml

import scala.jdk.CollectionConverters.*
import java.io.FileInputStream
import java.util.Map as JMap

sealed trait ExpressionType
case object IntType extends ExpressionType
case object BoolType extends ExpressionType
case object ListIntType extends ExpressionType
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
    /*
    // arithmetic
    new AddOperator[Integer],
    new SubOperator[Integer],
    new MulOperator[Integer],
    //new DivOperator[Integer],
    new ModOperator[Integer],
    */

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

    // new ContainsOperator[List[Integer], Integer],
    // new ContainsOperator[Set[Int], Int]
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
    Constant.asCreatable[Boolean](() => scala.util.Random.nextBoolean())
  )
  private val allArrayElementFactories: List[Creatable] = List(
    ArrayElement.asCreatable[Integer](),
    ArrayElement.asCreatable[Boolean](),
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
    //CumulativeExpression
    //LexicographicalExpression.asCreatable()
  )

  private def toScala(value: Any): Any = value match {
    case m: java.util.Map[_, _] =>
      m.asScala.view.mapValues(toScala).toMap
    case l: java.util.List[_] =>
      l.asScala.map(toScala).toList
    case other => other
  }

  private val configFile = "src/main/resources/expressions.yml"
  private lazy val creatablesConfig: Map[String, Double] = {
    val yaml = new Yaml()
    val input = new FileInputStream(configFile)
    try {
      val javaData = yaml.load[java.util.Map[String, Object]](input)

      def toDouble(obj: Object): Double = obj match {
        case d: java.lang.Double => d.doubleValue()
        case i: java.lang.Integer => i.doubleValue()
        case b: java.lang.Boolean => if (b) 1.0 else 0.0
        case _ => 0.0
      }

      javaData.asScala.toMap.map { case (k, v) =>
        k -> toDouble(v)
      }
    } finally {
      input.close()
    }
  }


  val creatables: List[Creatable] = (
    binaryOperators.map(op => BinaryExpression.asCreatable(op)) ++
      unaryOperators.map(op => UnaryExpression.asCreatable(op)) ++
      allConstantFactories ++
      allVariablesFactories ++
      expressionFactories ++
      allArrayElementFactories
    ).filter(c =>
    creatablesConfig.getOrElse(c.toString, 0.0) > 0.0
  )

  debug(s"creatables: $creatables")

  def getWeight(c: Creatable): Double = {
    creatablesConfig.getOrElse(c.toString, 1.0)
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