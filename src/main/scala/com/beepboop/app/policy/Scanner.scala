package com.beepboop.app.policy

import com.beepboop.app.components.{ComposableExpression, Expression, ScopeModifier}
import com.beepboop.app.logger.LogTrait



object Scanner extends LogTrait {
  val DefaultSuccess = Compliant

  def visitAll(root: Expression[?], policies: Policy*): PolicyResult = {
    val locals = policies.collect { case p: LocalPolicy => p }.toList
    val globals = policies.collect { case p: GlobalPolicy => p }.toList

    globals.foreach(_.reset())

    val result = visitRecursive(root, locals, globals)

    if (!result.isAllowed) return result

    val globalResult = globals.foldLeft[PolicyResult](DefaultSuccess) { (res, policy) =>
      if (policy.isSatisfied) res else NonCompliant(root, policy.message)
    }

    globalResult
  }

  private def visitRecursive(
                              node: Expression[?],
                              locals: List[LocalPolicy],
                              globals: List[GlobalPolicy]
                            ): PolicyResult = {

    globals.foreach(_.visit(node))

    val localRes = locals.foldLeft[PolicyResult](DefaultSuccess)(_ && _.validate(node))
    if (!localRes.isAllowed) return localRes

    val (currLocals, currGlobals) = node match {
      case m: ScopeModifier =>
        val newPols = m.getAdditionalPolicies
        (locals ++ newPols.collect{case p: LocalPolicy => p},
          globals ++ newPols.collect{case p: GlobalPolicy => p})
      case _ => (locals, globals)
    }

    node match {
      case c: ComposableExpression =>
        val childrenRes = c.children.map(child => visitRecursive(child, currLocals, currGlobals))
          .foldLeft[PolicyResult](DefaultSuccess)(_ && _)

        if (!childrenRes.isAllowed) return childrenRes

        node match {
          case m: ScopeModifier =>
            val myGlobals = m.getAdditionalPolicies.collect{ case p: GlobalPolicy => p }
            myGlobals.foldLeft[PolicyResult](childrenRes) { (res, pol) =>
              if (pol.isSatisfied) res else NonCompliant(node, pol.message)
            }
          case _ => childrenRes
        }

      case _ => Compliant
    }
  }
}