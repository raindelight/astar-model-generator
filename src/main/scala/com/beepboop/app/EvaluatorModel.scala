package com.beepboop.app

import com.beepboop.app.components.BinaryExpression

trait ExpressionConfig {
// config similar to LogTrait and LogConfigProvider
  // type of mutation, turned on per Type of Expression
  // max depth ( global, lokalne )
}


// Type of mutations:
/*
change expression
change variable
change parameter
create nonterminal expression
be constant

*/

def stringWithSpaces(strings: String*): String = {
  strings.mkString(" ")
}


// GrammarT - Terminal / NonTerminal
sealed trait Expression[ReturnT] extends ExpressionConfig {
  def toString: String

  def eval: ReturnT
  // figure out how to inform about return type to allow type checking
  def distance: Any // type for heuristic Merge with Constraint Boundary Checker

  // something like this:
  // if ( depth > maxDepth ) {
  // Expression[Terminal, Any], Expression[Terminal, Any]
  //   }
}

/*

case class BinaryExpression[Boolean](left: components.Expression[Any], operator: Operator[Boolean], right: components.Expression[Any]) extends components.Expression {
  override def eval: Boolean = {
    operator.eval(left, right)
  }
  override def toString: String = {
    stringWithSpaces(left.toString, operator.toString, right.toString)
  }
}


case class UnaryExpression(operator: String, expression: components.Expression) extends components.Expression
case class Variable(name: String) extends components.Expression
case class Constant(value: Int) extends components.Expression

case class Forall(variableName: String, start: Int, end: Int, body: components.Expression) extends components.Expression
// TODO: LIST -> (array, range) , var iterate, expression

case class Exists(variableName: String, start: Int, end: Int, body: components.Expression) extends components.Expression
// TODO: LIST -> (array, range) , var iterate, expression


case class Operator(op: String)

case class EvaluableConstraint(left: Variable, op: Operator, right: Variable)


case class Sum(
                variables: List[Variable],
                operator: String,
                limit: components.Expression
              ) extends components.Expression

case class AllDifferent(
                         variables: List[Variable]
                       ) extends components.Expression

case class Count(
                  variables: List[Variable],
                  value: components.Expression,
                  operator: String,
                  count: components.Expression
                ) extends components.Expression


case class Lexicographical(
                            left: List[Variable],
                            operator: String,
                            right: List[Variable]
                          ) extends components.Expression



case class Minimum(
                    variables: List[Variable],
                    operator: String,
                    limit: components.Expression
                  ) extends components.Expression

case class Maximum(
                    variables: List[Variable],
                    operator: String,
                    limit: components.Expression
                  ) extends components.Expression



case class Task(
                 start: Variable,
                 duration: Variable,
                 demand: Variable
               )


case class Cumulative(
                       tasks: List[Task],
                       operator: String,
                       limit: components.Expression
                     ) extends components.Expression

case class ValuePrecedeChain(
                              valuesToPrecede: List[Int],
                              variables: List[Variable]
                            ) extends components.Expression

case class GlobalCardinality(
                              variables: List[Variable],
                              expectedCounts: Map[Int, Int]
                            ) extends components.Expression

case class Diffn(
                  rects: List[RectDescriptor]
                ) extends components.Expression

case class RectDescriptor(
                           x: components.Expression,
                           y: components.Expression,
                           width: components.Expression,
                           height: components.Expression
                         ) extends components.Expression

case class RedundantConstraint(
                                innerConstraint: components.Expression
                              ) extends components.Expression

case class StrEq(
                  s1: components.Expression,
                  s2: components.Expression
                ) extends components.Expression

object EvaluatorModel {
  def expressionToString(expr: components.Expression): String = expr match {
    case BinaryExpression(left, op, right) => s"(${expressionToString(left)} $op ${expressionToString(right)})"
    case UnaryExpression(op, expression) => s"$op(${expressionToString(expression)})"
    case Variable(name) => name
    case Constant(value) => value.toString
    case Forall(variableName, start, end, body) => s"forall ($variableName in $start..$end) (${expressionToString(body)})"
    case Exists(variableName, start, end, body) => s"exists ($variableName in $start..$end) (${expressionToString(body)})"

    case s: Sum =>
      s"sum([${s.variables.map(_.name).mkString(", ")}]) ${s.operator} ${expressionToString(s.limit)}"
    case ad: AllDifferent =>
      s"alldifferent([${ad.variables.map(_.name).mkString(", ")}])"
    case c: Count =>
      s"count([${c.variables.map(_.name).mkString(", ")}], ${expressionToString(c.value)}) ${c.operator} ${expressionToString(c.count)}"
    case l: Lexicographical =>
      s"lex_${l.operator}([${l.left.map(_.name).mkString(", ")}], [${l.right.map(_.name).mkString(", ")}])"
    case m: Minimum =>
      s"min([${m.variables.map(_.name).mkString(", ")}]) ${m.operator} ${expressionToString(m.limit)}"
    case m: Maximum =>
      s"max([${m.variables.map(_.name).mkString(", ")}]) ${m.operator} ${expressionToString(m.limit)}"
    case c: Cumulative =>
      s"cumulative([${c.tasks.map(t => s"task(${expressionToString(t.start)},${expressionToString(t.duration)},${expressionToString(t.demand)})").mkString(", ")}]) ${c.operator} ${expressionToString(c.limit)}"
    case vpc: ValuePrecedeChain =>
      s"value_precede_chain([${vpc.valuesToPrecede.mkString(", ")}], [${vpc.variables.map(_.name).mkString(", ")}])"
    case gc: GlobalCardinality =>
      s"global_cardinality([${gc.variables.map(_.name).mkString(", ")}], {${gc.expectedCounts.map { case (v, count) => s"$v:$count" }.mkString(", ")}})"
    case d: Diffn =>
      s"diffn([${d.rects.map(r => s"rect(${expressionToString(r.x)},${expressionToString(r.y)},${expressionToString(r.width)},${expressionToString(r.height)})").mkString(", ")}])"
    case rc: RedundantConstraint =>
      s"redundant(${expressionToString(rc.innerConstraint)})"
    case se: StrEq =>
      s"str_eq(${expressionToString(se.s1)}, ${expressionToString(se.s2)})"

    case r: RectDescriptor =>
      s"rect_desc(x:${expressionToString(r.x)}, y:${expressionToString(r.y)}, w:${expressionToString(r.width)}, h:${expressionToString(r.height)})"
    case t: Task =>
      s"task_desc(s:${expressionToString(t.start)}, d:${expressionToString(t.duration)}, dem:${expressionToString(t.demand)})"


    case other => s"UNSUPPORTED_EXPR_TYPE[${other.getClass.getSimpleName}]"
  }
  // this should be removed
  def evaluateStringExpression(expr: components.Expression, solution: Map[String, Int]): List[Int] = expr match {
    case Variable(name) => name.map(_.toInt).toList
    case Constant(value) => List(value)
    case _ => List.empty[Int]
  }
}

*/