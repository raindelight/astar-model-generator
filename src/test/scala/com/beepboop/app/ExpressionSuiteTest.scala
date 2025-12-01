package com.beepboop.app

import com.beepboop.app.components._
import org.scalatest.funsuite.AnyFunSuite
import com.beepboop.app.utils.Implicits.*

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

  test("ForAllExpression toString should render the expression correctly") {
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



class CountExpressionSuite extends AnyFunSuite {

  test("CountExpression should return the correct size of a list") {
    val collection = Constant(List(10, 20, 30))
    val countExpr = CountExpression(collection)

    val context = Map.empty[String, Any]
    assert(countExpr.eval(context) === 3)
  }

  test("CountExpression should return 0 for an empty list") {
    val collection = Constant(List.empty[Integer])
    val countExpr = CountExpression(collection)

    val context = Map.empty[String, Any]
    assert(countExpr.eval(context) === 0)
  }

  test("CountExpression should work with a variable list from context") {
    val collectionVar = Variable[List[Integer]]("test")
    val countExpr = CountExpression(collectionVar)

    val context = Map.empty[String, Any].+("test" -> List(1, 2, 3, 4, 5))
    assert(countExpr.eval(context) === 5)
  }

  test("CountExpression toString should render the expression correctly") {
    val collection = Constant(List(1, 2))
    val countExpr = CountExpression(collection)

    assert(countExpr.toString === "count([1, 2])")
  }
}


class ExistsExpressionSuite extends AnyFunSuite {

  test("ExistsExpression should evaluate to true when at least one element satisfies the predicate") {
    val collection = Constant(List(1, 2, 3))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      EqualOperator[Integer](),
      Constant(2)
    )
    val existsExpr = ExistsExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(existsExpr.eval(context) === true)
  }

  test("ExistsExpression should evaluate to false when no elements satisfy the predicate") {
    val collection = Constant(List(1, 3, 5))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      BinaryExpression(Variable[Integer]("i"), ModOperator[Integer](), Constant(2)),
      EqualOperator[Integer](),
      Constant(0)
    )
    val existsExpr = ExistsExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(existsExpr.eval(context) === false)
  }

  test("ExistsExpression should evaluate to false for an empty list") {
    val collection = Constant(List.empty[Integer])
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(
      Variable[Integer]("i"),
      GreaterOperator[Integer](),
      Constant(0)
    )
    val existsExpr = ExistsExpression(iterator, body)

    val context = Map.empty[String, Any]
    assert(existsExpr.eval(context) === false)
  }

  test("ExistsExpression toString should render the expression correctly") {
    val collection = Constant(List(1, 2))
    val iterator = IteratorDef("i", collection)
    val body = BinaryExpression(Variable[Integer]("i"), GreaterOperator[Integer](), Constant(0))
    val existsExpr = ExistsExpression(iterator, body)

    val expectedString = "exists(i in [1, 2])(( i > 0 ))"
    assert(existsExpr.toString === expectedString)
  }
}


class AllDifferentExpressionSuite extends AnyFunSuite {

  test("AllDifferentExpression should evaluate to true when all elements are unique") {
    val collection = Constant(List(1, 2, 3))
    val allDiffExpr = AllDifferentExpression(collection)

    val context = Map.empty[String, Any]
    assert(allDiffExpr.eval(context) === true)
  }

  test("AllDifferentExpression should evaluate to false when there are duplicates") {
    val collection = Constant(List(1, 2, 1))
    val allDiffExpr = AllDifferentExpression(collection)

    val context = Map.empty[String, Any]
    assert(allDiffExpr.eval(context) === false)
  }

  test("AllDifferentExpression should evaluate to false for empty list (based on implementation requirement list.nonEmpty)") {
    val collection = Constant(List.empty[Integer])
    val allDiffExpr = AllDifferentExpression(collection)

    val context = Map.empty[String, Any]
    assert(allDiffExpr.eval(context) === false)
  }

  test("AllDifferentExpression distance should calculate violation count correctly") {
    val collection = Constant(List(1, 1, 2, 2, 3))
    val allDiffExpr = AllDifferentExpression(collection)

    val context = Map.empty[String, Any]
    assert(allDiffExpr.distance(context) === 2)
  }

  test("AllDifferentExpression toString should render correctly") {
    val collection = Constant(List(1, 2))
    val allDiffExpr = AllDifferentExpression(collection)
    assert(allDiffExpr.toString === "alldifferent([1, 2])")
  }
}



class MinimumExpressionSuite extends AnyFunSuite {

  test("MinimumExpression should return the smallest element") {
    val collection = Constant(List(1, 2, 3).map(Int.box))
    val minExpr = MinimumExpression[Integer](collection)

    val context = Map.empty[String, Any]
    assert(minExpr.eval(context) === 1)
  }

  test("MinimumExpression should handle negative numbers") {
    val collection = Constant(List(1, -5, 0).map(Int.box))
    val minExpr = MinimumExpression[Integer](collection)

    val context = Map.empty[String, Any]
    assert(minExpr.eval(context) === -5)
  }

  test("MinimumExpression should work with variable context") {
    val collectionVar = Variable[List[Integer]]("nums")
    val minExpr = MinimumExpression[Integer](collectionVar)

    val context = Map.empty[String, Any].+("nums" -> List(100, 50, 75))
    assert(minExpr.eval(context) === 50)
  }

  test("MinimumExpression toString should render correctly") {
    val collection = Constant(List(1, 2).map(Int.box))
    val minExpr = MinimumExpression[Integer](collection)
    assert(minExpr.toString === "min([1, 2])")
  }

}


class MaximumExpressionSuite extends AnyFunSuite {

  test("MaximumExpression should return the largest element") {
    val collection = Constant(List(10, 2, 5).map(Int.box))
    val maxExpr = MaximumExpression[Integer](collection)

    val context = Map.empty[String, Any]
    assert(maxExpr.eval(context) === 10)
  }

  test("MaximumExpression should handle negative numbers") {
    val collection = Constant(List(-10, -2, -5).map(Int.box))
    val maxExpr = MaximumExpression[Integer](collection)

    val context = Map.empty[String, Any]
    assert(maxExpr.eval(context) === -2)
  }

  test("MaximumExpression should work with variable context") {
    val collectionVar = Variable[List[Integer]]("nums")
    val maxExpr = MaximumExpression[Integer](collectionVar)

    val context = Map.empty[String, Any].+("nums" -> List(1, 99, 3))
    assert(maxExpr.eval(context) === 99)
  }

  test("MaximumExpression toString should render correctly") {
    val collection = Constant(List(1, 2).map(Int.box))
    val maxExpr = MaximumExpression[Integer](collection)
    assert(maxExpr.toString === "max([1, 2])")
  }

}
