import com.beepboop.app.components.{AddOperator, AllDifferentExpression, AndOperator, BinaryExpression, Constant, DivOperator, MulOperator, SubOperator, Variable}
import com.beepboop.app.postprocessor.Postprocessor
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.Inspectors.*

class PostprocessorSuite extends AnyFunSuite {


  test("Sum of two constants should be simplified to constant"){
    val rawExpr = BinaryExpression[Integer](
      Constant(5),
      AddOperator[Integer](),
      Constant(10)
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant(15))
  }
  test("Sum of nested constants should be simplified to a single constant") {
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


  test("Mult of a variable with constant should not be simplified") {
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

  test("Two levels of constants nesting should be simplified") {
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

  test("Bools should bo simplified") {
    val rawExpr = BinaryExpression[Boolean](
      Constant[Boolean](true),
      AndOperator[Boolean](),
      Constant[Boolean](false)
    )
    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](false))
  }


  test("Function that calculates based on Constant should be simplified") {
    val rawExpr = AllDifferentExpression(
      Constant[List[Integer]](List[Integer](1,2,3))
    )

    val expr = Postprocessor.simplify(rawExpr)
    assert(expr == Constant[Boolean](true))
  }

}
