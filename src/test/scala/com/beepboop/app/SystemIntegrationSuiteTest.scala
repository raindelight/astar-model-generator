package com.beepboop.app

import com.beepboop.app.components.BinaryExpression
import com.beepboop.parser.{NewMinizincLexer, NewMinizincParser}
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source
/*
class SystemIntegrationSuiteTest extends AnyFunSuite {
  val modelCode: String = Source.fromFile("src/test/resources/test_model.mzn").mkString
  val input = CharStreams.fromString(modelCode)
  val lexer = new NewMinizincLexer(input)
  val tokens = new CommonTokenStream(lexer)
  val parser = new NewMinizincParser(tokens)
  val tree = parser.model()
  val modelListener = new MinizincModelListener(tokens, false)
  new ParseTreeWalker().walk(modelListener, tree)
  val extractedDataItems: List[DataItem] = modelListener.getDataItems
  val distanceAnalyzer = new ConstraintDistanceAnalyzer()

  val sampleSolutionFull: Map[String, Int] = Map(
    // Coloring
    "wa" -> 1, "nt" -> 2, "sa" -> 3, "q" -> 1, "t" -> 1,
    // Scheduling & General
    "v1" -> 5, "v2" -> 2, "v3" -> 8, "v4" -> 2, "v5" -> 3,
    "s1" -> 0, "d1" -> 3, "r1" -> 2, "s2" -> 2, "d2" -> 2, "r2" -> 4, "s3" -> 3, "d3" -> 3, "r3" -> 1,
    // Lexicographical
    "lex_a1" -> 1, "lex_a2" -> 2, "lex_a3" -> 5,
    "lex_b1" -> 1, "lex_b2" -> 3, "lex_b3" -> 1,
    // ValuePrecedeChain
    "precede_chain1" -> 3, "precede_chain2" -> 1, "precede_chain3" -> 4, "precede_chain4" -> 2, "precede_chain5" -> 5,
    // GlobalCardinality
    "gcc_vars1"-> 1, "gcc_vars2"-> 1, "gcc_vars3"-> 2, "gcc_vars4"-> 3, "gcc_vars5"-> 3,
    // Diffn
    "rect1_x" -> 0, "rect1_y" -> 0, "rect1_w" -> 3, "rect1_h" -> 3,
    "rect2_x" -> 2, "rect2_y" -> 2, "rect2_w" -> 3, "rect2_h" -> 3
  )
  
  test("Parser - poprawna liczba wszystkich elementów") {
    assert(extractedDataItems.length == 26)
  }
  test("Listener - poprawna liczba parametrów") {
    assert(extractedDataItems.count(!_.isVar) == 7)
  }
  test("Listener - poprawna liczba zmiennych decyzyjnych") {
    assert(extractedDataItems.count(_.isVar) == 19)
  }
  
  test("EqualityDistanceCalculator - Spełnione") {
    val constraint = BinaryExpression(Variable("wa"), "==", Constant(1))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("EqualityDistanceCalculator - Naruszone") {
    val constraint = BinaryExpression(Variable("sa"), "==", Constant(1))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 2)
  }
  
  test("OrderingDistanceCalculator - Spełnione") {
    val constraint = BinaryExpression(Variable("wa"), "<=", Variable("nt"))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("OrderingDistanceCalculator - Naruszone") {
    val constraint = BinaryExpression(Variable("sa"), "<=", Variable("nt"))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }
  
  test("InequalityDistanceCalculator - Spełnione") {
    val constraint = BinaryExpression(Variable("wa"), "!=", Variable("nt"))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("InequalityDistanceCalculator - Naruszone") {
    val constraint = BinaryExpression(Variable("wa"), "!=", Variable("q"))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }
  
  test("SummationDistanceCalculator - Spełnione") {
    val constraint = Sum(List(Variable("v2"), Variable("v4")), "==", Constant(4))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("SummationDistanceCalculator - Naruszone") {
    val constraint = Sum(List(Variable("v1"), Variable("v3")), ">=", Constant(15))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 2)
  }
  
  test("AllDifferentDistanceCalculator - Spełnione") {
    val constraint = AllDifferent(List(Variable("wa"), Variable("nt"), Variable("sa")))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("AllDifferentDistanceCalculator - Naruszone") {
    val constraint = AllDifferent(List(Variable("wa"), Variable("q"), Variable("t")))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 2)
  }
  
  test("CountDistanceCalculator - Spełnione") {
    val constraint = Count(List(Variable("v1"), Variable("v2"), Variable("v3"), Variable("v4"), Variable("v5")), Constant(2), "==", Constant(2))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("CountDistanceCalculator - Naruszone") {
    val constraint = Count(List(Variable("v1"), Variable("v2"), Variable("v3"), Variable("v4"), Variable("v5")), Constant(5), ">=", Constant(2))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }
  
  test("LexicographicalDistanceCalculator - Spełnione") {
    val left = List(Variable("lex_a1"), Variable("lex_a2"), Variable("lex_a3")) // [1, 2, 5]
    val right = List(Variable("lex_b1"), Variable("lex_b2"), Variable("lex_b3")) // [1, 3, 1]
    val constraint = Lexicographical(left, "<=", right)
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("LexicographicalDistanceCalculator - Naruszone") {
    val left = List(Variable("lex_b1"), Variable("lex_b2"), Variable("lex_b3")) // [1, 3, 1]
    val right = List(Variable("lex_a1"), Variable("lex_a2"), Variable("lex_a3")) // [1, 2, 5]
    val constraint = Lexicographical(left, "<=", right) // 3-2=1
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }

  // ValuePrecedeChain
  test("ValuePrecedeChainDistanceCalculator - Spełnione") {
    val chain = List(Variable("precede_chain1"), Variable("precede_chain2"), Variable("precede_chain3"), Variable("precede_chain4"), Variable("precede_chain5"))
    val constraint = ValuePrecedeChain(List(1, 2, 5), chain) // 1 występuje przed 2, 2 przed 5
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("ValuePrecedeChainDistanceCalculator - Naruszone") {
    val chain = List(Variable("precede_chain1"), Variable("precede_chain2"), Variable("precede_chain3"), Variable("precede_chain4"), Variable("precede_chain5"))
    val constraint = ValuePrecedeChain(List(2, 1, 3), chain)
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 2)
  }

  // GlobalCardinality
  test("GlobalCardinalityDistanceCalculator - Spełnione") {
    val vars = List(Variable("gcc_vars1"), Variable("gcc_vars2"), Variable("gcc_vars3"), Variable("gcc_vars4"), Variable("gcc_vars5"))
    val constraint = GlobalCardinality(vars, Map(1 -> 2, 2 -> 1, 3 -> 2))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("GlobalCardinalityDistanceCalculator - Naruszone") {
    val vars = List(Variable("gcc_vars1"), Variable("gcc_vars2"), Variable("gcc_vars3"), Variable("gcc_vars4"), Variable("gcc_vars5"))
    val constraint = GlobalCardinality(vars, Map(1 -> 3, 2 -> 2)) // oczekiwano 3 jedynki (są 2), 2 dwójki (jest 1). Suma różnic: 1+1=2
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 2)
  }

  // Diffn
  test("DiffnDistanceCalculator - Spełnione") {
    val rects = List(
      RectDescriptor(Variable("rect1_x"), Variable("rect1_y"), Variable("rect1_w"), Constant(0)), // zerowa wysokość = brak overlapu
      RectDescriptor(Variable("rect2_x"), Variable("rect2_y"), Variable("rect2_w"), Variable("rect2_h"))
    )
    assert(distanceAnalyzer.getDistance(Diffn(rects), sampleSolutionFull) == 0)
  }
  test("DiffnDistanceCalculator - Naruszone") {
    val rects = List(
      RectDescriptor(Variable("rect1_x"), Variable("rect1_y"), Variable("rect1_w"), Variable("rect1_h")), // [0,0,3,3]
      RectDescriptor(Variable("rect2_x"), Variable("rect2_y"), Variable("rect2_w"), Variable("rect2_h"))  // [2,2,3,3]
    )
    // Overlap X: min(3,5)-max(0,2) = 1. Overlap Y: min(3,5)-max(0,2) = 1. Pole: 1*1=1
    assert(distanceAnalyzer.getDistance(Diffn(rects), sampleSolutionFull) == 1)
  }

  // Logical
  test("LogicalDistanceCalculator - Spełnione") {
    // (wa==1) AND (nt==2) -> 0 AND 0 -> 0
    val constr = BinaryExpression(BinaryExpression(Variable("wa"), "==", Constant(1)), "AND", BinaryExpression(Variable("nt"), "==", Constant(2)))
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 0)
  }
  test("LogicalDistanceCalculator - Naruszone") {
    // NOT (wa==1) -> NOT (0) -> 1
    val constr = UnaryExpression("NOT", BinaryExpression(Variable("wa"), "==", Constant(1)))
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 1)
  }

  // Minimum
  test("MinimumDistanceCalculator - Spełnione") {
    val constr = Minimum(List(Variable("v1"), Variable("v2"), Variable("v3")), ">=", Constant(2)) // min([5,2,8])=2. 2>=2
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 0)
  }
  test("MinimumDistanceCalculator - Naruszone") {
    val constr = Minimum(List(Variable("v1"), Variable("v2"), Variable("v3")), ">=", Constant(3)) // min([5,2,8])=2. 3-2=1
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 1)
  }

  // Maximum
  test("MaximumDistanceCalculator - Spełnione") {
    val constraint = Maximum(List(Variable("v1"), Variable("v3")), "<=", Constant(8)) // max(5,8)=8. 8<=8
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("MaximumDistanceCalculator - Naruszone") {
    val constraint = Maximum(List(Variable("v1"), Variable("v3")), "<=", Constant(7)) // max(5,8)=8. 8-7=1
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }

  // Cumulative
  test("CumulativeDistanceCalculator - Spełnione") {
    val tasks = List(Task(Variable("s1"), Variable("d1"), Variable("r1")))
    val constraint = Cumulative(tasks, "<=", Constant(5))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 0)
  }
  test("CumulativeDistanceCalculator - Naruszone") {
    val tasks = List(Task(Variable("s1"), Variable("d1"), Variable("r1")), Task(Variable("s2"), Variable("d2"), Variable("r2")))
    // t=2: obciążenie 2+4=6. Przeciążenie: 1
    val constraint = Cumulative(tasks, "<=", Constant(5))
    assert(distanceAnalyzer.getDistance(constraint, sampleSolutionFull) == 1)
  }

  // RedundantConstraint
  test("RedundantConstraintDistanceCalculator - Naruszone") {
    // Kara powinna być taka sama jak dla wewnętrznego ograniczenia (sa==1) -> 2
    val inner = BinaryExpression(Variable("sa"), "==", Constant(1))
    val constr = RedundantConstraint(inner)
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 2)
  }

  // StringEquality (testuje tylko zachowanie stuba)
  test("StringEqualityDistanceCalculator - Naruszone") {
    val constr = StrEq(Constant(123), Constant(456))
    assert(distanceAnalyzer.getDistance(constr, sampleSolutionFull) == 1)
  }
}
*/