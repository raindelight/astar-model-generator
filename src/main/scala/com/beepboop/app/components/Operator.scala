package com.beepboop.app.components

import com.beepboop.app.utils.Implicits.integerNumeric
import scala.reflect.{ClassTag, classTag}
import com.beepboop.app.components.{Addable, Andable, Contains, Divisible, Equatable, GreaterEqual, GreaterThan, Implies, LessEqual, LessThan, Modulable, Multiplicable, NotEquatable, Orable, Signature, Subtractable, Xorable}
import com.beepboop.app.components.{Negatable, Absolutable, BoolToIntConvertible, NotComputable}
import com.beepboop.app.logger.LogTrait

abstract class Operator[ReturnT](implicit val ct: ClassTag[ReturnT]) extends Serializable{
  def toString: String
  def signature: Signature
  def distance(left: Any, right: Any): Int = 0
}

abstract class UnaryOperator[ReturnT](implicit ct: ClassTag[ReturnT]) extends Operator[ReturnT] {
  def eval(expr: Any): ReturnT
}

abstract class BinaryOperator[ReturnT](implicit ct: ClassTag[ReturnT]) extends Operator[ReturnT] {
  def eval(left: Any, right: Any): ReturnT
}

case class AddOperator[T: ClassTag]()(implicit strategy: Addable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.add(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "+"
  override def signature: Signature = {
    val exprType = scalaTypeToExprType(classTag[T].runtimeClass)
    Signature(List(exprType, exprType), exprType)
  }
}

case class SubOperator[T: ClassTag]()(implicit strategy: Subtractable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.sub(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "-"
  override def signature: Signature = {
    val exprType = scalaTypeToExprType(classTag[T].runtimeClass)
    Signature(List(exprType, exprType), exprType)
  }
}

case class MulOperator[T: ClassTag]()(implicit strategy: Multiplicable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.mul(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "*"
  override def signature: Signature = {
    val exprType = scalaTypeToExprType(classTag[T].runtimeClass)
    Signature(List(exprType, exprType), exprType)
  }
}

case class DivOperator[T: ClassTag]()(implicit strategy: Divisible[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.div(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "/"
  override def signature: Signature = {
    val exprType = scalaTypeToExprType(classTag[T].runtimeClass)
    Signature(List(exprType, exprType), exprType)
  }
}

case class ModOperator[T: ClassTag]()(implicit strategy: Modulable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.mod(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "%"
  override def signature: Signature = {
    val exprType = scalaTypeToExprType(classTag[T].runtimeClass)
    Signature(List(exprType, exprType), exprType)
  }
}

case class EqualOperator[T: ClassTag]()(implicit strategy: Equatable[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = {
    strategy.equal(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "="
  override def signature: Signature = {
    Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)
  }

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]

    if (classOf[Number].isAssignableFrom(classTag[T].runtimeClass)) {
      Math.abs(leftT.asInstanceOf[Number].intValue() - rightT.asInstanceOf[Number].intValue())
    } else if (classOf[Set[?]].isAssignableFrom(classTag[T].runtimeClass)) {
      try {
        val s1 = leftT.asInstanceOf[Set[Any]]
        val s2 = rightT.asInstanceOf[Set[Any]]
        (s1 diff s2).size + (s2 diff s1).size
      } catch {
        case _: Exception => if (strategy.equal(leftT, rightT)) 0 else 1
      }
    } else {
      if (strategy.equal(leftT, rightT)) 0 else 1
    }
  }
}

case class NotEqualOperator[T: ClassTag]()(implicit strategy: NotEquatable[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.notEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "!="
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)

  override def distance(left: Any, right: Any): Int = {
    strategy.distance(left.asInstanceOf[T], right.asInstanceOf[T])
  }
}

case class ContainsOperator[L: ClassTag, R: ClassTag]()(implicit strategy: Contains[L, R]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.contains(left.asInstanceOf[L], right.asInstanceOf[R])
  override def toString: String = "contains"
  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[L].runtimeClass), scalaTypeToExprType(classTag[R].runtimeClass)), BoolType
  )
  override def distance(left: Any, right: Any): Int = {
    strategy.distance(left.asInstanceOf[L], right.asInstanceOf[R])
  }
}

case class LessOperator[T: ClassTag]()(implicit strategy: LessThan[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.less(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "<"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      Math.abs(leftT.asInstanceOf[Integer] - (rightT.asInstanceOf[Integer] - 1))
    } else {
      if (strategy.less(leftT, rightT)) 0 else 1
    }
  }
}

case class LessEqualOperator[T: ClassTag]()(implicit strategy: LessEqual[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.lessEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "<="
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      Math.abs(leftT.asInstanceOf[Integer] - rightT.asInstanceOf[Integer])
    } else {
      if (strategy.lessEqual(leftT, rightT)) 0 else 1
    }
  }
}

case class GreaterOperator[T: ClassTag]()(implicit strategy: GreaterThan[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.greater(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = ">"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      Math.abs(leftT.asInstanceOf[Integer] - (rightT.asInstanceOf[Integer] + 1))
    } else {
      if (strategy.greater(leftT, rightT)) 0 else 1
    }
  }
}

case class GreaterEqualOperator[T: ClassTag]()(implicit strategy: GreaterEqual[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.greaterEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = ">="
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass), scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      Math.abs(leftT.asInstanceOf[Integer] - rightT.asInstanceOf[Integer])
    } else {
      if (strategy.greaterEqual(leftT, rightT)) 0 else 1
    }
  }
}

case class AndOperator[T: ClassTag]()(implicit strategy: Andable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.and(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "and"
  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)
  override def distance(left: Any, right: Any): Int = if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
}

class OrOperator[T: ClassTag](implicit strategy: Orable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.or(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "or"
  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)
  override def distance(left: Any, right: Any): Int = if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
}

case class XorOperator[T: ClassTag]()(implicit strategy: Xorable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.xor(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "xor"
  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)
  override def distance(left: Any, right: Any): Int = if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
}

case class ImpliesOperator[T: ClassTag]()(implicit strategy: Implies[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = strategy.implies(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "->"
  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)
  override def distance(left: Any, right: Any): Int = if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
}

case class NotOperator[T: ClassTag]()(implicit strategy: NotComputable[T]) extends UnaryOperator[Boolean] {
  override def eval(expr: Any): Boolean = strategy.compute(expr.asInstanceOf[T])
  override def toString: String = "!"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass)), BoolType)
}

case class NegateOperator[T: ClassTag]()(implicit strategy: Negatable[T]) extends UnaryOperator[T] {
  override def eval(expr: Any): T =  strategy.negate(expr.asInstanceOf[T])
  override def toString: String = "-"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass)), scalaTypeToExprType(classTag[T].runtimeClass))
}

case class AbsOperator[T: ClassTag]()(implicit strategy: Absolutable[T]) extends UnaryOperator[T] {
  override def eval(expr: Any): T =  strategy.abs(expr.asInstanceOf[T])
  override def toString: String = "abs"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass)), scalaTypeToExprType(classTag[T].runtimeClass))
}

case class BoolToIntOperator[T: ClassTag]()(implicit strategy: BoolToIntConvertible[T]) extends UnaryOperator[Integer] {
  override def eval(expr: Any): Integer = strategy.convert(expr.asInstanceOf[T])
  override def toString: String = "bool2int"
  override def signature: Signature = Signature(List(scalaTypeToExprType(classTag[T].runtimeClass)), scalaTypeToExprType(classOf[Integer]))
}