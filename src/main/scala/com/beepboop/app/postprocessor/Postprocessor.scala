package com.beepboop.app.postprocessor

import com.beepboop.app.components.{BinaryExpression, ComposableExpression, Constant, Expression, ForAllExpression, IteratorDef}
import com.beepboop.app.logger.LogTrait

import scala.reflect.{ClassTag, classTag}

sealed trait Rule {
  def condition[T : ClassTag]: (Expression[T] => Boolean)
  def action[T : ClassTag]: (Expression[T] => Expression[T])

}

case class JoinConstants() extends Rule, LogTrait {
  def condition[T : ClassTag]: (Expression[T] => Boolean) = {
    case c: ComposableExpression =>
      val childrenAreConstants = c.children.forall(child => child.isInstanceOf[Constant[?]])

      val isStructuralNode = c.isInstanceOf[IteratorDef[?]]
      childrenAreConstants && !isStructuralNode
    case _ => false
  }

  def action[T : ClassTag]: (Expression[T] => Constant[T]) = {
    case c: ComposableExpression => {
      val resultValue = c.eval(context = Map.empty)
      debug(s"Eval of ${c} is ${resultValue} type [${resultValue.getClass.getSimpleName}] expected [${classTag[T].runtimeClass.getSimpleName}]")
      Constant[T](resultValue)
    }
  }

}

case class SimplifyConstantForall() extends Rule {
  def condition[T: ClassTag]: (Expression[T] => Boolean) = {
    case c: ForAllExpression[T] =>
      val bodyIsConstant = c.body.isInstanceOf[Constant[Boolean]]
      bodyIsConstant
    case _ => false
  }

  def action[T: ClassTag]: (Expression[T] => Constant[T]) = {
    case c: ForAllExpression[T] => {
      val resultValue = c.body.eval(context = Map.empty)
      Constant[T](resultValue)
    }
  }
}

object Postprocessor {
  def availableRules = LazyList (
    JoinConstants(),
    SimplifyConstantForall()
  )
  def applyAllRules[T : ClassTag](expr: Expression[T]): Expression[T] = {
    val distinctResult = availableRules.foldLeft(expr) { (curr, rule) =>
      if (rule.condition(curr)) rule.action(curr).asInstanceOf[Expression[T]] else curr
    }

    if (distinctResult != expr) applyAllRules(distinctResult) else expr
  }

  def simplify[T : ClassTag](expr: Expression[T]): Expression[T] = {
    val nodeWithSimplifiedChildren: Expression[T] = expr match {
      case c: ComposableExpression =>
        val newChildren = c.children.map {
          case childTyped: Expression[childT] =>
            implicit val childTag: ClassTag[childT] = childTyped.ct
            simplify(childTyped)
        }
        c.withNewChildren(newChildren).asInstanceOf[Expression[T]]
      case leaf =>
        leaf
    }
    Postprocessor.applyAllRules(nodeWithSimplifiedChildren)
  }
}