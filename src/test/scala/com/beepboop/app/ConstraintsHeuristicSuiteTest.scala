package com.beepboop.app

import com.beepboop.app.components._
import com.beepboop.app.utils.Implicits._
import org.scalatest.funsuite.AnyFunSuite

class ConstraintsHeuristicSuiteTest extends AnyFunSuite {

  private def i(n: Int): Integer = Integer.valueOf(n)
  private def c(value: Int): Constant[Integer] = Constant(i(value))
  private def cBool(value: Boolean): Constant[Boolean] = Constant(value)
  private def cStr(value: String): Constant[String] = Constant(value)

  private def v(name: String): Variable[Integer] = Variable[Integer](name)
  private def vList(name: String): Variable[List[Integer]] = Variable[List[Integer]](name)
  private def vStr(name: String): Variable[String] = Variable[String](name)

  private def cMapVal(value: Map[Int, Int]): Map[Integer, Integer] = {
    value.map { case (k, v) => (i(k), i(v)) }
  }

  private def binOp(left: Expression[?], op: BinaryOperator[?], right: Expression[?]): Expression[Boolean] =
    BinaryExpression(left, op.asInstanceOf[BinaryOperator[Any]], right).asInstanceOf[Expression[Boolean]]

  private def unaryOp(op: UnaryOperator[?], expr: Expression[?]): Expression[Boolean] =
    UnaryExpression(expr, op.asInstanceOf[UnaryOperator[Any]]).asInstanceOf[Expression[Boolean]]

  private def check(testName: String, expr: Expression[Boolean], context: Map[String, Any], expectedDist: Int): Unit = {
    val actualDist = expr.distance(context)
    val satisfied = expr.eval(context)

    val contextStr = context.map { case (k, v) =>
      val vStr = v match {
        case l: List[_] => l.mkString("[", ",", "]")
        case m: Map[_,_] => m.mkString("{", ",", "}")
        case other => other.toString
      }
      s"$k=$vStr"
    }.mkString(", ")

    println(s"TEST: $testName")
    println(s"  Constraint: $expr")
    println(s"  Context:    $contextStr")
    println(s"  Result:     Satisfied=$satisfied, Dist=$actualDist (Expected: $expectedDist)")
    println("-" * 50)

    assert(actualDist === expectedDist, s"[$testName] Expected distance $expectedDist but got $actualDist")
  }

  test("Greater_Satisfied") {
    check("Greater_Satisfied", binOp(v("x"), new GreaterOperator[Integer], c(5)), Map("x" -> i(8)), 2)
  }

  test("Greater_Fail") {
    check("Greater_Fail", binOp(v("x"), new GreaterOperator[Integer], c(5)), Map("x" -> i(3)), 3)
  }

  test("Less_Satisfied") {
    check("Less_Satisfied", binOp(v("x"), new LessOperator[Integer], c(10)), Map("x" -> i(5)), 4)
  }

  test("Equal_Satisfied") {
    check("Equal_Satisfied", binOp(v("x"), new EqualOperator[Integer], c(5)), Map("x" -> i(5)), 0)
  }

  test("NotEqual_Satisfied") {
    check("NotEqual_Satisfied", binOp(v("x"), new NotEqualOperator[Integer], c(5)), Map("x" -> i(3)), 1)
  }

  test("AND_Satisfied") {
    val expr = binOp(
      binOp(v("x"), new GreaterOperator[Integer], c(0)),
      new AndOperator[Boolean],
      binOp(v("x"), new LessOperator[Integer], c(5))
    )
    check("AND_Satisfied", expr, Map("x" -> i(3)), 3)
  }

  test("OR_Satisfied_Left") {
    val expr = binOp(
      binOp(v("x"), new EqualOperator[Integer], c(1)),
      new OrOperator[Boolean],
      binOp(v("x"), new EqualOperator[Integer], c(2))
    )
    check("OR_Satisfied_Left", expr, Map("x" -> i(1)), 0)
  }

  test("XOR_Satisfied") {
    check("XOR_Satisfied", binOp(cBool(true), new XorOperator[Boolean], cBool(false)), Map.empty, 0)
  }

  test("Implies_Satisfied") {
    check("Implies_Satisfied", binOp(cBool(false), new ImpliesOperator[Boolean], cBool(false)), Map.empty, 0)
  }

  test("Not_Satisfied") {
    check("Not_Satisfied", unaryOp(new NotOperator[Boolean], cBool(false)), Map.empty, 0)
  }

  test("Add_Formula") {
    val expr = binOp(BinaryExpression(v("x"), new AddOperator[Integer], v("y")), new EqualOperator[Integer], c(10))
    check("Add_Formula", expr, Map("x" -> i(4), "y" -> i(6)), 0)
  }

  test("Abs_Formula") {
    val expr = binOp(UnaryExpression(v("x"), new AbsOperator[Integer]), new EqualOperator[Integer], c(5))
    check("Abs_Formula", expr, Map("x" -> i(-5)), 0)
  }

  test("List_Contains_True") {
    check("List_Contains_True", binOp(vList("list"), new ContainsOperator[List[Integer], Integer], c(3)), Map("list" -> List(i(1), i(2), i(3))), 0)
  }

  test("Sum_Check") {
    check("Sum_Check", binOp(SumExpression[Integer](vList("list")), new EqualOperator[Integer], c(10)), Map("list" -> List(i(1), i(2), i(3), i(4))), 0)
  }

  test("Count_Check") {
    check("Count_Check", binOp(CountExpression(vList("list")), new EqualOperator[Integer], c(3)), Map("list" -> List(i(10), i(20), i(30))), 0)
  }

  test("Min_Check") {
    check("Min_Check", binOp(MinimumExpression[Integer](vList("list")), new EqualOperator[Integer], c(1)), Map("list" -> List(i(5), i(1), i(9))), 0)
  }

  test("ForAll_Satisfied") {
    val expr = ForAllExpression(IteratorDef("i", vList("list")), binOp(v("i"), new GreaterOperator[Integer], c(0)))
    check("ForAll_Satisfied", expr, Map("list" -> List(i(1), i(2), i(3))), 3)
  }

  test("Exists_Satisfied") {
    val expr = ExistsExpression(IteratorDef("i", vList("list")), binOp(v("i"), new EqualOperator[Integer], c(5)))
    check("Exists_Satisfied", expr, Map("list" -> List(i(1), i(5), i(3))), 0)
  }

  test("AllDiff_Fail_1Dup") {
    check("AllDiff_Fail_1Dup", AllDifferentExpression(vList("list")), Map("list" -> List(i(1), i(1), i(2))), 1)
  }

  test("GCC_Satisfied") {
    val expr = GlobalCardinalityExpression(vList("list"), Constant(cMapVal(Map(1 -> 2, 2 -> 1))))
    check("GCC_Satisfied", expr, Map("list" -> List(i(1), i(1), i(2))), 0)
  }

  test("ValPrecede_Fail") {
    check("ValPrecede_Fail", ValuePrecedesChainExpression(vList("list"), Constant(List(i(1), i(2)))), Map("list" -> List(i(2), i(1))), 2)
  }

  test("Diffn_Fail_Overlap") {
    val expr = DiffnExpression(
      Variable[List[Integer]]("x"),
      Variable[List[Integer]]("y"),
      Variable[List[Integer]]("dx"),
      Variable[List[Integer]]("dy"),

    )
    val ctx = Map(
      "x" -> List(0,1),
      "y" -> List(0,1),
      "dx" -> List(2,2),
      "dy" -> List(2,2)
    )
    check("Diffn_Fail_Overlap", expr, ctx, 1)
  }

  test("Diffn_Satisfied") {

    val expr = DiffnExpression(
      Variable[List[Integer]]("x"),
      Variable[List[Integer]]("y"),
      Variable[List[Integer]]("dx"),
      Variable[List[Integer]]("dy"),

    )
    val ctx = Map(
      "x" -> List(0,2),
      "y" -> List(0,0),
      "dx" -> List(2,2),
      "dy" -> List(2,2)
    )

    check("Diffn_Satisfied", expr, ctx, 0)
  }

  test("StrEq_Satisfied") {
    check("StrEq_Satisfied", StrEqExpression(vStr("s1"), vStr("s2")), Map("s1" -> "hello", "s2" -> "hello"), 0)
  }

  test("Redundant_Wrapper") {
    check("Redundant_Wrapper", RedundantConstraint(binOp(v("x"), new GreaterOperator[Integer], c(0))), Map("x" -> i(5)), 4)
  }

  private def checkNode(name: String, constraints: List[Expression[Boolean]], context: Map[String, Any], expectedTotal: Int): Unit = {
    val currentTotal = constraints.map(_.distance(context)).sum

    println(s"NODE TEST: $name")
    val contextStr = context.map { case (k, v) =>
      val vStr = v match {
        case l: List[_] => l.mkString("[", ",", "]")
        case other => other.toString
      }
      s"$k=$vStr"
    }.mkString(", ")
    println(s"  Context: $contextStr")

    constraints.zipWithIndex.foreach { case (c, idx) =>
      println(s"    [$idx] Dist: ${c.distance(context).toString.padTo(4, ' ')} | $c")
    }
    println(s"  => Total: $currentTotal (Expected: $expectedTotal)")
    println("=" * 50)

    assert(currentTotal === expectedTotal, s"[$name] Expected total $expectedTotal but got $currentTotal")
  }

  test("Node_Geometric") {
    checkNode("Geometric", List(
      binOp(v("x"), new GreaterOperator[Integer], c(0)),
      binOp(v("x"), new LessOperator[Integer], c(10))
    ), Map("x" -> i(5)), 8)
  }

  test("Node_Mixed_Types") {
    checkNode("Mixed Types", List(
      AllDifferentExpression(vList("list")),
      binOp(SumExpression[Integer](vList("list")), new EqualOperator[Integer], c(5)),
      binOp(v("y"), new EqualOperator[Integer], c(100))
    ), Map("list" -> List(i(1), i(1), i(2)), "y" -> i(100)), 2)
  }

  test("Node_Nested_Logic") {
    checkNode("Nested Logic", List(
      binOp(BinaryExpression(v("x"), new AddOperator[Integer], v("y")), new GreaterOperator[Integer], c(10)),
      RedundantConstraint(binOp(BinaryExpression(v("z"), new SubOperator[Integer], c(1)), new EqualOperator[Integer], c(0)))
    ), Map("x" -> i(2), "y" -> i(3), "z" -> i(2)), 7)
  }

  test("Node_Large_Gap") {
    checkNode("Large Gap", List(
      binOp(v("x"), new EqualOperator[Integer], c(100)),
      binOp(v("y"), new GreaterOperator[Integer], c(50))
    ), Map("x" -> i(0), "y" -> i(0)), 151)
  }

  test("Node_Scheduling_6Constraints") {
    val constraints = List(
      binOp(v("t1"), new GreaterEqualOperator[Integer], c(0)),
      binOp(v("t1"), new LessOperator[Integer], c(10)),
      binOp(v("t2"), new GreaterOperator[Integer], v("t1")),
      binOp(v("t3"), new GreaterOperator[Integer], v("t2")),
      binOp(v("t3"), new EqualOperator[Integer], c(5)),
      binOp(BinaryExpression(v("t2"), new AddOperator[Integer], c(2)), new EqualOperator[Integer], v("t3"))
    )
    checkNode("Scheduling", constraints, Map("t1" -> i(2), "t2" -> i(1), "t3" -> i(5)), 16)
  }

  test("Node_ComplexLogic_7Constraints") {
    val constraints = List(
      AllDifferentExpression(vList("list")),
      binOp(SumExpression[Integer](vList("list")), new EqualOperator[Integer], c(6)),
      binOp(v("a"), new LessOperator[Integer], v("b")),
      binOp(v("b"), new LessOperator[Integer], v("c")),
      binOp(BinaryExpression(v("a"), new AddOperator[Integer], v("b")), new EqualOperator[Integer], v("c")),
      binOp(v("c"), new GreaterOperator[Integer], c(10)),
      binOp(binOp(v("a"), new EqualOperator[Integer], c(1)), new AndOperator[Boolean], binOp(v("a"), new EqualOperator[Integer], c(100)))
    )
    checkNode("Complex Logic", constraints, Map("list" -> List(i(1), i(2), i(3)), "a" -> i(1), "b" -> i(2), "c" -> i(3)), 107)
  }

}