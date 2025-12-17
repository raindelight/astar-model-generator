package com.beepboop.app.postprocessor

import com.beepboop.app.components.*
import com.beepboop.app.logger.LogTrait

import scala.annotation.tailrec
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

case class RemoveRedundantTrueAnd() extends Rule {
  def condition[T: ClassTag]: (Expression[T] => Boolean) = {
    case b: BinaryExpression[T] =>
      val operatorIsAnd = b.operator.isInstanceOf[AndOperator[?]]
      val oneIsTrue = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const.eval(Map.empty) match {
              case true => true
              case _ => false
            }
          case _ => false

        }
      )
      operatorIsAnd && oneIsTrue
    case _ => false
  }

  def action[T: ClassTag]: (Expression[T] => Expression[T]) = {
    case b: BinaryExpression[T] => {
      val nonConstant = b.children.filterNot(c => c.isInstanceOf[Constant[Boolean]]).head
      nonConstant.asInstanceOf[Expression[T]]
    }
  }
}


case class RemoveRedundantFalseAnd() extends Rule {
  def condition[T: ClassTag]: (Expression[T] => Boolean) = {
    case b: BinaryExpression[T] =>
      val operatorIsAnd = b.operator.isInstanceOf[AndOperator[?]]
      val oneIsTrue = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const.eval(Map.empty) match {
              case false => true
              case _ => false
            }
          case _ => false

        }
      )
      operatorIsAnd && oneIsTrue
    case _ => false
  }

  def action[T: ClassTag]: (Expression[T] => Expression[T]) = {
    case b: BinaryExpression[T] => {
      Constant[T](false.asInstanceOf[T])
    }
  }
}


case class RemoveRedundantTrueOr() extends Rule {
  def condition[T: ClassTag]: (Expression[T] => Boolean) = {
    case b: BinaryExpression[T] =>
      val operatorIsOr = b.operator.isInstanceOf[OrOperator[?]]
      val oneIsTrue = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const.eval(Map.empty) match {
              case true => true
              case _ => false
            }
          case _ => false

        }
      )
      operatorIsOr && oneIsTrue
    case _ => false
  }

  def action[T: ClassTag]: (Expression[T] => Constant[T]) = {
    case b: BinaryExpression[T] => {
      Constant[T](true.asInstanceOf[T])
    }
  }
}

case class RemoveRedundantFalseOr() extends Rule {
  def condition[T: ClassTag]: (Expression[T] => Boolean) = {
    case b: BinaryExpression[T] =>
      val operatorIsOr = b.operator.isInstanceOf[OrOperator[?]]
      val oneIsTrue = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const.eval(Map.empty) match {
              case false => true
              case _ => false
            }
          case _ => false

        }
      )
      operatorIsOr && oneIsTrue
    case _ => false
  }

  def action[T: ClassTag]: (Expression[T] => Expression[T]) = {
    case b: BinaryExpression[T] => {
      val nonConstant = b.children.filterNot(c => c.isInstanceOf[Constant[Boolean]]).head
      nonConstant.asInstanceOf[Expression[T]]
    }
  }
}

case class RemoveUnnecessaryAdd() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsAdd = b.operator.isInstanceOf[AddOperator[?]]
      val zeroExists = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const eval(Map.empty) match {
              case 0 => true
              case _ => false
            }
          case _ => false
        }
      )
      operatorIsAdd && zeroExists
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] => {
      val nonConst = b.children.filterNot(c => c.isInstanceOf[Constant[?]]).head
      nonConst.asInstanceOf[Expression[T]]
    }
  }
}


case class RemoveUnnecessarySub() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsSub = b.operator.isInstanceOf[SubOperator[?]]
      val zeroExists = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const eval(Map.empty) match {
              case 0 => true
              case _ => false
            }
          case _ => false
        }
      )
      operatorIsSub && zeroExists
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] => {
      val child1 = b.children.head
      val child2 = b.children.tail.head

      if (child1.isInstanceOf[Constant[?]]) {
        val op = classTag[T].runtimeClass match {
          case c if  c == classOf[Integer] => NegateOperator[Integer]()
          /* v based on the assumptions this shouldn't happen */
          case _ => throw Exception("RemoveUnnecessarySub failed to assume Integer, thus attempted to create NegateOperator with unknown type")
        }
        UnaryExpression[T](
          child2.asInstanceOf[Expression[T]],
          op.asInstanceOf[UnaryOperator[T]]
        )
      } else {
        child1.asInstanceOf[Expression[T]]
      }
    }
  }
}

case class RemoveUnnecessaryMult() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsMul = b.operator.isInstanceOf[MulOperator[?]]
      val OneOrZero = b.children.exists(c =>
        c match {
          case const: Constant[?] =>
            const.eval(Map.empty) match {
              case 0 => true
              case 1 => true
              case _ => false
            }
          case _ => false
        }
      )
      operatorIsMul && OneOrZero
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] => {
      val const = b.children.filter(c => c.isInstanceOf[Constant[?]]).head
      val nonConst = b.children.filterNot(c => c.isInstanceOf[Constant[?]]).head

      const.eval(Map.empty) match {
        case 0 => Constant[T](0.asInstanceOf[T])
        case 1 => nonConst.asInstanceOf[Expression[T]]
      }
    }
  }
}


case class VarDivToOne() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsDiv = b.operator.isInstanceOf[DivOperator[?]]

      val AllAreVarsAndSameName = b.children.headOption match {
        case Some(first: Variable[?]) =>
          b.children.forall {
            case v: Variable[?] => v.name == first.name
            case _              => false
          }
        case None => true
        case _    => false
      }
      operatorIsDiv && AllAreVarsAndSameName
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] => {
      Constant[T](1.asInstanceOf[T])
    }
  }
}


case class VarDivToN() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] if b.operator.isInstanceOf[DivOperator[?]] =>
      b.children match {
        case List(numerator: BinaryExpression[?], denominator: Variable[?])
          if numerator.operator.isInstanceOf[MulOperator[?]] =>

          numerator.children match {
            case List(c: Constant[?], v: Variable[?]) =>
              v.name == denominator.name
            case List(v: Variable[?], c: Constant[?]) =>
              v.name == denominator.name
            case _ => false
          }
        case _ => false
      }
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] =>
      val numerator = b.children.head.asInstanceOf[BinaryExpression[T]]

      numerator.children.find(_.isInstanceOf[Constant[?]]) match {
        case Some(constExpr) => constExpr.asInstanceOf[Expression[T]]
        case None => b
      }
  }
}

case class SubVarsToZero() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsSub = b.operator.isInstanceOf[SubOperator[?]]
      val AllAreVarsAndSameName = b.children.headOption match {
        case Some(first: Variable[?]) =>
          b.children.forall {
            case v: Variable[?] => v.name == first.name
            case _ => false
          }
        case None => true
        case _ => false
      }

      operatorIsSub && AllAreVarsAndSameName
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] =>
      Constant[T](0.asInstanceOf[T])
  }

}


case class EqualityToTrue() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsEquatable = b.operator.isInstanceOf[EqualOperator[?]] ||
          b.operator.isInstanceOf[LessEqualOperator[?]] ||
          b.operator.isInstanceOf[GreaterEqualOperator[?]]

      val AllAreVarsAndSameName = b.children.headOption match {
        case Some(first: Variable[?]) =>
          b.children.forall {
            case v: Variable[?] => v.name == first.name
            case _ => false
          }
        case None => true
        case _ => false
      }

      operatorIsEquatable && AllAreVarsAndSameName
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] =>
      Constant[T](true.asInstanceOf[T])
  }

}


case class NotEqualityToFalse() extends Rule {
  override def condition[T: ClassTag]: Expression[T] => Boolean = {
    case b: BinaryExpression[T] =>
      val operatorIsEquatable = b.operator.isInstanceOf[NotEqualOperator[?]] ||
        b.operator.isInstanceOf[LessOperator[?]] ||
        b.operator.isInstanceOf[GreaterOperator[?]]

      val AllAreVarsAndSameName = b.children.headOption match {
        case Some(first: Variable[?]) =>
          b.children.forall {
            case v: Variable[?] => v.name == first.name
            case _ => false
          }
        case None => true
        case _ => false
      }

      operatorIsEquatable && AllAreVarsAndSameName
    case _ => false
  }

  override def action[T: ClassTag]: Expression[T] => Expression[T] = {
    case b: BinaryExpression[T] =>
      Constant[T](false.asInstanceOf[T])
  }

}


object Postprocessor {
  private def availableRules = LazyList (
    JoinConstants(),
    SimplifyConstantForall(),
    RemoveRedundantTrueOr(),
    RemoveRedundantFalseOr(),
    RemoveRedundantFalseAnd(),
    RemoveRedundantTrueAnd(),
    RemoveUnnecessaryMult(),
    RemoveUnnecessaryAdd(),
    RemoveUnnecessarySub(),
    VarDivToOne(),
    VarDivToN(),
    SubVarsToZero(),
    EqualityToTrue(),
    NotEqualityToFalse()
  )
  @tailrec
  private def applyAllRules[T : ClassTag](expr: Expression[T]): Expression[T] = {
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