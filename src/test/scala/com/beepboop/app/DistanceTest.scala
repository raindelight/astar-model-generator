package com.beepboop.app.tests

import com.beepboop.app.components.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DistanceSpec extends AnyFlatSpec with Matchers {

  "EqualOperator for Sets" should "use symmetric difference distance" in {
    println("\n--- Testing EqualOperator for Sets ---")
    val op = new EqualOperator[Set[Integer]]()

    val s1: Set[Integer] = Set(1, 2)
    val s2: Set[Integer] = Set(2, 3)
    val d1 = op.distance(s1, s2)
    println(s"Sets: $s1 vs $s2 | Expected: 2 | Actual: $d1")
    op.eval(s1, s2) should be (false)
    d1 should be (2)

    val s3: Set[Integer] = Set(1)
    val s4: Set[Integer] = Set(1, 2, 3)
    val d2 = op.distance(s3, s4)
    println(s"Sets: $s3 vs $s4 | Expected: 2 | Actual: $d2")
    d2 should be (2)

    val s5: Set[Integer] = Set(1)
    val s6: Set[Integer] = Set(2)
    val d3 = op.distance(s5, s6)
    println(s"Sets: $s5 vs $s6 | Expected: 2 | Actual: $d3")
    d3 should be (2)

    val s7: Set[Integer] = Set(1, 2)
    val s8: Set[Integer] = Set(1, 2)
    val d4 = op.distance(s7, s8)
    println(s"Sets: $s7 vs $s8 | Expected: 0 | Actual: $d4")
    d4 should be (0)
  }

  "NotEqualOperator" should "use distance to boundary" in {
    println("\n--- Testing NotEqualOperator ---")
    val op = new NotEqualOperator[Integer]()

    val d1 = op.distance(3, 5)
    println(s"3 != 5 | Expected: 1 | Actual: $d1")
    d1 should be (1)

    val d2 = op.distance(3, 4)
    println(s"3 != 4 | Expected: 0 | Actual: $d2")
    d2 should be (0)

    val d3 = op.distance(3, 3)
    println(s"3 != 3 | Expected: 1 | Actual: $d3")
    d3 should be (1)
  }

  "ContainsOperator" should "calculate correct distance for Lists" in {
    println("\n--- Testing ContainsOperator ---")
    val op = new ContainsOperator[List[Integer], Integer]()

    val list = List[Integer](4, 5, 9)
    val target = 1
    val d1 = op.distance(list, target)
    println(s"List: $list, Target: $target | Expected: 3 | Actual: $d1")
    op.eval(list, target) should be (false)
    d1 should be (3)

    val list2 = List[Integer](1, 5, 9)
    val d2 = op.distance(list2, target)
    println(s"List: $list2, Target: $target | Expected: 0 | Actual: $d2")
    d2 should be (0)
  }

  "LessOperator" should "calculate distance to boundary" in {
    println("\n--- Testing LessOperator ---")
    val op = new LessOperator[Integer]()

    val d1 = op.distance(3, 5)
    println(s"3 < 5 | Expected: 1 | Actual: $d1")
    op.eval(3, 5) should be (true)
    d1 should be (1)

    val d2 = op.distance(3, 4)
    println(s"3 < 4 | Expected: 0 | Actual: $d2")
    op.eval(3, 4) should be (true)
    d2 should be (0)

    val d3 = op.distance(5, 3)
    println(s"5 < 3 | Expected: 3 | Actual: $d3")
    op.eval(5, 3) should be (false)
    d3 should be (3)
  }

  "DiffnExpression" should "calculate overlap area/distance" in {
    println("\n--- Testing DiffnExpression ---")
    val ctx = Map[String, Any]()

    val r1 = RectDescriptor(Constant(0), Constant(0), Constant(2), Constant(2))
    val r2 = RectDescriptor(Constant(1), Constant(1), Constant(2), Constant(2))
    val expr = DiffnExpression(Constant(List(r1, r2)))
    val d1 = expr.distance(ctx)
    println(s"Rects: Overlapping (0,0,2,2) & (1,1,2,2) | Expected: 1 | Actual: $d1")
    expr.eval(ctx) should be (false)
    d1 should be (1)

    val r3 = RectDescriptor(Constant(0), Constant(0), Constant(2), Constant(2))
    val r4 = RectDescriptor(Constant(3), Constant(3), Constant(2), Constant(2))
    val exprSafe = DiffnExpression(Constant(List(r3, r4)))
    val d2 = exprSafe.distance(ctx)
    println(s"Rects: Disjoint (0,0,2,2) & (3,3,2,2) | Expected: 0 | Actual: $d2")
    d2 should be (0)
  }

  "AndOperator" should "sum distances" in {
    println("\n--- Testing AndOperator ---")
    val op = new AndOperator[Boolean]()

    val d1 = op.distance(true, true)
    println(s"true AND true | Expected: 0 | Actual: $d1")
    d1 should be (0)

    val d2 = op.distance(true, false)
    println(s"true AND false | Expected: 1 | Actual: $d2")
    d2 should be (1)
  }
}