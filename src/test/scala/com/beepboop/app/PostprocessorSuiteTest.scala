import com.beepboop.app.components.{SubOperator, *}
import com.beepboop.app.postprocessor.Postprocessor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inspectors.*
import org.scalatest.Tag




class PostprocessorSuite extends AnyFunSuite {


  test("Unary expression should be simplified on constant abs(-2) | JoinConstants") {
    val rawExpr = UnaryExpression[Integer](
      Constant[Integer](-2),
      AbsOperator[Integer]()
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Integer](2))
  }

  test("(opDur contains ( 2 - 2 )) should be simplified to (opDur contains (0) | JoinConstants") {
    val rawExpr = BinaryExpression[Boolean](
      Variable[List[Integer]]("opDur"),
      ContainsOperator[List[Integer], Integer](),
      BinaryExpression(
        Constant[Integer](2),
        SubOperator[Integer](),
        Constant[Integer](2)
      )
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Boolean] (
      Variable[List[Integer]]("opDur"),
      ContainsOperator[List[Integer], Integer](),
      Constant[Integer](0)
    ))
  }

  test("Sum of two constants should be simplified to constant | JoinConstants"){
    val rawExpr = BinaryExpression[Integer](
      Constant(5),
      AddOperator[Integer](),
      Constant(10)
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant(15))
  }
  test("Sum of nested constants should be simplified to a single constant | JoinConstants") {
    val rawExpr = BinaryExpression[Integer](
      Constant(8),
      AddOperator[Integer](),
      BinaryExpression(
        Constant(3),
        SubOperator[Integer](),
        Constant(2)
      )
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant(9))
  }


  test("Mult of a variable with constant should not be simplified | JoinConstants") {
    val rawExpr = BinaryExpression[Integer](
      Constant[Integer](8),
      MulOperator[Integer](),
      Variable[Integer]("x")
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Integer](
      Constant[Integer](8),
      MulOperator[Integer](),
      Variable[Integer]("x")
    ))
  }

  test("Two levels of constants nesting should be simplified | JoinConstants") {
    val rawExpr = BinaryExpression[Integer](
      BinaryExpression[Integer](
        Constant[Integer](5),
        MulOperator[Integer](),
        Constant[Integer](4)
      ),
      DivOperator[Integer](),
      BinaryExpression[Integer](
        Constant[Integer](4),
        AddOperator[Integer](),
        Constant[Integer](1)
      )
    )

    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Integer](4))
  }

  test("Bools should be simplified | JoinConstants") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](true),
      AndOperator[Boolean](),
      Constant[Boolean](false)
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](false))
  }


  test("Function that calculates based on Constant should be simplified | JoinConstants") {
    val rawExpr = AllDifferentExpression(
      Constant[List[Integer]](List[Integer](1,2,3))
    )

    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](true))
  }

  test("Simplify forall with always true body | SimplifyConstantForall") {
    val rawExpr = ForAllExpression[Integer](
      IteratorDef[Integer]("x", Constant[List[Integer]](List(1,2,3,4,5))),
      BinaryExpression(
        Constant[Integer](1),
        EqualOperator[Integer](),
        Constant[Integer](1)
      )
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](true))

  }



  test("Simplify forall with always false body | SimplifyConstantForall") {
    val rawExpr = ForAllExpression[Integer](
      IteratorDef[Integer]("x", Constant[List[Integer]](List(1,2,3,4,5))),
      BinaryExpression(
        Constant[Integer](1),
        EqualOperator[Integer](),
        Constant[Integer](2)
      )
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](false))

  }


  test("Unnecessary (true or X) should be simplified to true | RemoveRedundantTrueOr") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](true),
      OrOperator[Boolean](),
      BinaryExpression[Boolean](
        Constant[Integer](3),
        EqualOperator[Integer](),
        Variable[Integer]("x")
      )
    )


    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](true))

  }

  test("Unnecessary (false or X) should be simplified to X | RemoveRedundantFalseOr") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](false),
      OrOperator[Boolean](),
      BinaryExpression[Boolean](
        Constant[Integer](3),
        EqualOperator[Integer](),
        Variable[Integer]("x")
      )
    )


    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Boolean](
      Constant[Integer](3),
      EqualOperator[Integer](),
      Variable[Integer]("x")
    ))

  }

  test("Unnecessary (true and X) should be simplified to X | RemoveRedundantTrueAnd") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](true),
      AndOperator[Boolean](),
      BinaryExpression[Boolean](
        Constant[Integer](3),
        EqualOperator[Integer](),
        Variable[Integer]("x")
      )
    )


    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Boolean](
      Constant[Integer](3),
      EqualOperator[Integer](),
      Variable[Integer]("x")
    ))
  }


  test("Unnecessary (false and X) should be simplified to false | RemoveRedundantFalseAnd") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](false),
      AndOperator[Boolean](),
      BinaryExpression[Boolean](
        Constant[Integer](3),
        EqualOperator[Integer](),
        Variable[Integer]("x")
      )
    )


    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](false))
  }


  test("Adding X + 0 should be simplified to X (left) | RemoveRedundantAdd") {
    val rawExpr = BinaryExpression[Integer] (
      Constant[Integer](0),
      AddOperator[Integer](),
      Variable[Integer]("x")
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Variable[Integer]("x"))
  }



  test("Adding X + 1 should not be simplified to X (left) | RemoveRedundantAdd") {
    val rawExpr = BinaryExpression[Integer] (
      Constant[Integer](1),
      AddOperator[Integer](),
      Variable[Integer]("x")
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Integer](
      Constant[Integer](1),
      AddOperator[Integer](),
      Variable[Integer]("x")
    ))
  }


  test("Adding X + 0 should be simplified to X (right) | RemoveRedundantAdd") {
    val rawExpr = BinaryExpression[Integer] (
      Variable[Integer]("x"),
      AddOperator[Integer](),
      Constant[Integer](0)
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Variable[Integer]("x"))
  }




  test("Adding X + 1 should not be simplified to X (right) | RemoveRedundantAdd") {
    val rawExpr = BinaryExpression[Integer] (
      Variable[Integer]("x"),
      AddOperator[Integer](),
      Constant[Integer](1)
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == BinaryExpression[Integer](
      Variable[Integer]("x"),
      AddOperator[Integer](),
      Constant[Integer](1)
    ))
  }






  test("Expression multiplied by 1 should return just expression (right) | RemoveRedundantMult") {
    val rawExpr = BinaryExpression[Integer](
      Variable[Integer]("x"),
      MulOperator[Integer](),
      Constant[Integer](1)
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Variable[Integer]("x"))
  }

  test("Expression multiplied by 0 should return 0 (right) | RemoveRedundantMult") {
    val rawExpr = BinaryExpression[Integer](
      Variable[Integer]("x"),
      MulOperator[Integer](),
      Constant[Integer](0)
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Integer](0))
  }

  test("Expression multiplied by 1 should return just expression (left) | RemoveRedundantMult") {
    val rawExpr = BinaryExpression[Integer](
      Constant[Integer](1),
      MulOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Variable[Integer]("x"))
  }

  test("Expression multiplied by 0 should return 0 (left) | RemoveRedundantMult") {
    val rawExpr = BinaryExpression[Integer](
      Constant[Integer](0),
      MulOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Integer](0))
  }


  test("Subtracting 0 from expression should return expression | RemoveUnnecessarySub") {
    val rawExpr = BinaryExpression[Integer](
      Variable[Integer]("x"),
      SubOperator[Integer](),
      Constant[Integer](0)
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Variable[Integer]("x"))
  }

  test("Subtracting expression from 0 should return unary - | RemoveUnnecessarySub") {
    val rawExpr = BinaryExpression[Integer](
      Constant[Integer](0),
      SubOperator[Integer](),
      Variable[Integer]("x")
    )
    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == UnaryExpression[Integer](
      Variable[Integer]("x"),
      NegateOperator[Integer]()
    ))
  }

  test("Dividing same variables should return 1 | VarDivToOne") {
    val rawExpr = BinaryExpression[Integer](
      Variable[Integer]("x"),
      DivOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Integer](1))
  }



  test("Dividing same n*x / x should return n | VarDivToN") {
    val rawExpr = BinaryExpression[Integer](
      BinaryExpression[Integer](
        Constant[Integer](3),
        MulOperator[Integer](),
        Variable[Integer]("x")
      ),
      DivOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Integer](3))
  }

  test("Subtracting x - x should return 0 | SubVarsToZero") {
    val rawExpr = BinaryExpression[Integer](
      Variable[Integer]("x"),
      SubOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Integer](0))
  }

  test("Var equals Var should return true | EqualityToTrue") {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      EqualOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](true))
  }

  test("Var >= Var should return true | EqualityToTrue") {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      GreaterEqualOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](true))
  }
  test("Var <= Var should return true | EqualityToTrue") {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      LessEqualOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](true))
  }

  test("Var < Var should return false | NonEqualityToFalse")  {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      LessOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](false))

  }

  test("Var > Var should return false | NonEqualityToFalse")  {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      GreaterOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](false))

  }



  test("Var != Var should return false | NonEqualityToFalse")  {
    val rawExpr = BinaryExpression[Boolean](
      Variable[Integer]("x"),
      NotEqualOperator[Integer](),
      Variable[Integer]("x")
    )

    val expr = Postprocessor.simplify(rawExpr)

    assert(expr == Constant[Boolean](false))

  }
}
