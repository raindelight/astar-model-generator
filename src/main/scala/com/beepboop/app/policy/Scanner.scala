package com.beepboop.app.policy

import com.beepboop.app.components.{Expression, ComposableExpression}
import com.beepboop.app.logger.LogTrait


object Scanner extends LogTrait {
  val DefaultFailure = NonCompliant(null, "No compliant node found in this branch")
  
  
  def visitAll(expr: Expression[?], policy: Policy): PolicyResult = {
    val result = expr match {
      case c: ComposableExpression => {
        c.children
          .map(child => visitAll(child, policy))
          .foldLeft[PolicyResult](DefaultFailure)(_ || _)
      }
      case _ => policy.validate(expr)
    }
    if (result.isAllowed) {
      result
    } else {
      NonCompliant(expr, policy.message)
    }
  }
}