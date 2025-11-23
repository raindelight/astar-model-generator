package com.beepboop.app

import com.beepboop.app.components._
import org.scalatest.funsuite.AnyFunSuite

class ForAllExpressionSuite extends AnyFunSuite {

  test("ForAllExpression should evaluate to true when all elements satisfy the predicate") {
    // forall(i in [1, 2, 3]))(i > 0)
    val collection = Constant(List(1, 2, 3))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(forallExpr.eval(context) === true)
  }

  test("ForAllExpression should evaluate to false when some elements do not satisfy the predicate") {
    // forall(i in [1, -2, 3])(i > 0)
    val collection = Constant(List(1, -2, 3))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(forallExpr.eval(context) === false)
  }

  test("ForAllExpression should evaluate to true for an empty list") {
    // forall(i in List())(i > 0)
    // The `forall` operation on an empty collection should always be true. // todo: check in minizinc
    val collection = Constant(List.empty[Integer])
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(forallExpr.eval(context) === true)
  }

  test("ForAllExpression should work with a more complex predicate") {
    // forall(i in [2, 4, 6])((i % 2) == 0)
    val collection = Constant(List(2, 4, 6))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      BinaryExpression(
        Variable[Integer]("i"),
        ModOperator[Integer],
        Constant(2)
      ),
      EqualOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(forallExpr.eval(context) === true)
  }

  test ("ForAllExpression should work with passing Variable in iterator") {
    // test = [2, 8, 10]
    // forall( i in test ) (i > 0)
    val collection = Variable[List[Integer]]("test")
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val forAllExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any].+("test" -> List(2, 8, 10))
    assert(forAllExpr.eval(context) === true)
  }

  test("ForAllExpression should evaluate to false with a complex predicate that fails") {
    // forall(i in [2, 3, 6])((i % 2) == 0)
    val collection = Constant(List(2, 3, 6))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      BinaryExpression(
        Variable[Integer]("i"),
        ModOperator[Integer](),
        Constant(2)
      ),
      EqualOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(forallExpr.eval(context) === false)
  }

  test("toString should render the expression correctly") {
    val collection = Constant(List(1, 2, 3))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val forallExpr = ForAllExpression(iterator, body)

    val expectedString = "forall(i in [1, 2, 3])(( i > 0 ))"
    assert(forallExpr.toString === expectedString)
  }
}


