package com.beepboop.app.components

/* third party */
import com.beepboop.app.utils.Implicits.integerNumeric

import scala.reflect.{ClassTag, classTag}

/* own modules */
import com.beepboop.app.components.{Addable, Andable, Contains, Divisible, Equatable, GreaterEqual, GreaterThan, Implies, LessEqual, LessThan, Modulable, Multiplicable, NotEquatable, Orable, Signature, Subtractable, Xorable}
import com.beepboop.app.components.{Addable, Signature, NotEquatable, Equatable, LessThan, GreaterThan, LessEqual, GreaterEqual, Negatable, Absolutable, BoolToIntConvertible}
import com.beepboop.app.components.{Addable, Signature, NotEquatable, Equatable, NotComputable}
import com.beepboop.app.logger.LogTrait


sealed trait Operator[ReturnT] extends Serializable{

  def toString: String

  def signature: Signature

  def distance(left: Any, right: Any): Int = 0
}

trait UnaryOperator[ReturnT] extends Operator[ReturnT] {
  def eval(expr: Any): ReturnT
}

trait BinaryOperator[ReturnT] extends Operator[ReturnT] {
  def eval(left: Any, right: Any): ReturnT
}

class AddOperator[T: ClassTag](implicit strategy: Addable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.add(left.asInstanceOf[T], right.asInstanceOf[T])
  }

  override def toString: String = "+"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), exprType)
  }
}


class SubOperator[T: ClassTag](implicit strategy: Subtractable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.sub(left.asInstanceOf[T], right.asInstanceOf[T])
  }

  override def toString: String = "-"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), exprType)
  }
}


class MulOperator[T: ClassTag](implicit strategy: Multiplicable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.mul(left.asInstanceOf[T], right.asInstanceOf[T])
  }

  override def toString: String = "*"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), exprType)
  }
}

class DivOperator[T: ClassTag](implicit strategy: Divisible[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.div(left.asInstanceOf[T], right.asInstanceOf[T])
  }

  override def toString: String = "/"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), exprType)
  }
}


class ModOperator[T: ClassTag](implicit strategy: Modulable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.mod(left.asInstanceOf[T], right.asInstanceOf[T])
  }

  override def toString: String = "%"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), exprType)
  }
}



class EqualOperator[T: ClassTag](implicit strategy: Equatable[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = {
    strategy.equal(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "="


  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]

    if (classTag[T].runtimeClass.isAssignableFrom(classOf[Number])) {
      Math.abs(leftT.asInstanceOf[Number].intValue() - rightT.asInstanceOf[Number].intValue())
    } else if (leftT.isInstanceOf[Set[?]] && rightT.isInstanceOf[Set[?]]) {
      // Oblicz odległość dla zbiorów (np. rozmiar różnicy symetrycznej)
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

class NotEqualOperator[T: ClassTag](implicit strategy: NotEquatable[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = {
    strategy.notEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "!="


  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }


  override def distance(left: Any, right: Any): Int = {
    strategy.distance(left.asInstanceOf[T], right.asInstanceOf[T])
  }
}

class ContainsOperator[L: ClassTag, R: ClassTag](implicit strategy: Contains[L, R]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.contains(left.asInstanceOf[L], right.asInstanceOf[R])

  override def toString: String = "contains"

  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[L].runtimeClass), scalaTypeToExprType(classTag[R].runtimeClass)), BoolType
  )

  override def distance(left: Any, right: Any): Int = {
    if (strategy.contains(left.asInstanceOf[L], right.asInstanceOf[R])) 0 else 1
  }
}

class LessOperator[T: ClassTag](implicit strategy: LessThan[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.less(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "<"
  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }
  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]

    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      // Distance: min of left side to right side, and right side to left
      ((rightT.asInstanceOf[Integer] - 1) - leftT.asInstanceOf[Integer]).abs
    } else {
      if (strategy.less(leftT, rightT)) 0 else 1
    }
  }
}

class LessEqualOperator[T: ClassTag](implicit strategy: LessEqual[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = strategy.lessEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  override def toString: String = "<="
  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }
  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      (rightT.asInstanceOf[Integer] - leftT.asInstanceOf[Integer]).abs
    } else {
      if (strategy.lessEqual(leftT, rightT)) 0 else 1
    }
  }
}



class GreaterOperator[T: ClassTag](implicit strategy: GreaterThan[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = {
    strategy.greater(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = ">"

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }

  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      ((leftT.asInstanceOf[Integer] - 1) - rightT.asInstanceOf[Integer]).abs
    } else {
      if (strategy.greater(leftT, rightT)) 0 else 1
    }
  }

}

class GreaterEqualOperator[T: ClassTag](implicit strategy: GreaterEqual[T]) extends BinaryOperator[Boolean] {
  override def eval(left: Any, right: Any): Boolean = {
    strategy.greaterEqual(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = ">="

  override def signature: Signature = {
    val cls = classTag[T].runtimeClass
    val exprType = scalaTypeToExprType(cls)
    Signature(List(exprType, exprType), BoolType)
  }


  override def distance(left: Any, right: Any): Int = {
    val leftT = left.asInstanceOf[T]
    val rightT = right.asInstanceOf[T]
    if (leftT.isInstanceOf[Integer] && rightT.isInstanceOf[Integer]) {
      ((leftT.asInstanceOf[Integer]) - rightT.asInstanceOf[Integer]).abs
    } else {
      if (strategy.greaterEqual(leftT, rightT)) 0 else 1 // Fallback
    }
  }
}

class AndOperator[T: ClassTag](implicit strategy: Andable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.and(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "and"

  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)
}

class OrOperator[T: ClassTag](implicit strategy: Orable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.or(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "or"

  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)


  override def distance(left: Any, right: Any): Int = {
    if (eval(left, right).isInstanceOf[Boolean]) 0 else 1
  }
}

class XorOperator[T: ClassTag](implicit strategy: Xorable[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.xor(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "xor"

  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)

  override def distance(left: Any, right: Any): Int = {
    if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
  }
}

class ImpliesOperator[T: ClassTag](implicit strategy: Implies[T]) extends BinaryOperator[T] {
  override def eval(left: Any, right: Any): T = {
    strategy.implies(left.asInstanceOf[T], right.asInstanceOf[T])
  }
  override def toString: String = "->"

  override def signature: Signature = Signature(List(BoolType, BoolType), BoolType)


  override def distance(left: Any, right: Any): Int = {
    if (eval(left, right).asInstanceOf[Boolean]) 0 else 1
  }
}


class NotOperator[T: ClassTag](implicit strategy: NotComputable[T]) extends UnaryOperator[Boolean] {
  override def eval(expr: Any): Boolean = strategy.compute(expr.asInstanceOf[T])

  override def toString: String = "!"

  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[T].runtimeClass)), BoolType
  )
}

class NegateOperator[T: ClassTag](implicit strategy: Negatable[T]) extends UnaryOperator[T] {
  override def eval(expr: Any): T =  strategy.negate(expr.asInstanceOf[T])

  override def toString: String = "-"

  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[T].runtimeClass)), scalaTypeToExprType(classTag[T].runtimeClass)
  )
}

class AbsOperator[T: ClassTag](implicit strategy: Absolutable[T]) extends UnaryOperator[T] {
  override def eval(expr: Any): T =  strategy.abs(expr.asInstanceOf[T])

  override def toString: String = "abs"

  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[T].runtimeClass)), scalaTypeToExprType(classTag[T].runtimeClass)
  )
}

class BoolToIntOperator[T: ClassTag, R: ClassTag](implicit strategy: BoolToIntConvertible[T, R]) extends UnaryOperator[R] {
  override def eval(expr: Any): R = strategy.convert(expr.asInstanceOf[T])

  override def toString: String = "bool2int"

  override def signature: Signature = Signature(
    List(scalaTypeToExprType(classTag[T].runtimeClass)),
    scalaTypeToExprType(classTag[R].runtimeClass)
  )
}