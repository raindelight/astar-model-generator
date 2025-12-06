package com.beepboop.app.policy

import com.beepboop.app.components.{Expression, ComposableExpression, Variable}

sealed trait PolicyResult {
  def isAllowed: Boolean
  def &&(other: PolicyResult): PolicyResult = if (isAllowed) other else this
  def ||(other: PolicyResult): PolicyResult = if (isAllowed) this else other
}
case object Compliant extends PolicyResult {val isAllowed = true }
case class NonCompliant(expr: Expression[?], message: String) extends PolicyResult { val isAllowed = false }

sealed trait Policy {
  def validate(ctx: Expression[?]): PolicyResult
  def message = "Expression doesn't match policy"
}


case class EnsureVarExists() extends Policy {
  override def message = "Expression doesn't contain variable"
  def validate(cts: Expression[?]): PolicyResult = {
    cts match {
      case v: Variable[?] => Compliant
      case _ => NonCompliant(cts, message)
    }
  }
}
