package com.beepboop.app.policy

import com.beepboop.app.components.{BinaryExpression, ComposableExpression, Constant, DivOperator, Expression, Variable}

sealed trait PolicyResult {
  def isAllowed: Boolean
  def &&(other: PolicyResult): PolicyResult = if (isAllowed) other else this
  def ||(other: PolicyResult): PolicyResult = if (isAllowed) this else other
}
case object Compliant extends PolicyResult {val isAllowed = true }
case class NonCompliant(expr: Expression[?], message: String) extends PolicyResult { val isAllowed = false }

sealed trait Policy {
  def message = "Expression doesn't match policy"
}

trait LocalPolicy extends Policy {
  def validate(node: Expression[?]): PolicyResult
}

trait GlobalPolicy extends Policy {
  def reset(): Unit
  def visit(node: Expression[?]): Unit
  def isSatisfied: Boolean
  def cloneForBranch(): GlobalPolicy = this
}


case class EnsureAnyVarExists() extends GlobalPolicy {
  private var found = false

  override def message: String = "Expression must contain at least one variable"

  override def reset(): Unit = {
    found = false
  }

  override def visit(node: Expression[?]): Unit = {
    if (!found) {
      node match {
        case _: Variable[?] => found = true
        case _ =>
      }
    }
  }

  override def isSatisfied: Boolean = found
}


case class EnsureSpecificVarExists(targetName: String) extends GlobalPolicy {
  private var found = false

  override def message: String = s"Expr doesn't contain '$targetName' in scope"

  override def reset(): Unit = {
    found = false
  }

  override def visit(node: Expression[?]): Unit = {
    if (!found) {
      node match {
        case v: Variable[?] if v.name == targetName =>
          found = true
        case _ =>
      }
    }
  }


  override def isSatisfied: Boolean = found
}

case class DenyDivByZero() extends LocalPolicy {

  override def message: String = s"Division by 0 is not allowed"

  override def validate(node: Expression[_]): PolicyResult = node match {
    case BinaryExpression(_, operator, rightChild) =>
      val isDivision = operator.isInstanceOf[DivOperator[?]]

      if (isDivision) {
        rightChild match {
          case Constant(0) => NonCompliant(node, message)
          case _ => Compliant
        }
      }  else {
        Compliant
      }
    case _ => Compliant
  }
}
