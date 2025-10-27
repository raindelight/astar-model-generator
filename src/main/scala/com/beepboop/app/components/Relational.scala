package com.beepboop.app.components

trait Equatable[T] {
  def equal(a: T, b: T): Boolean
}

object Equatable {
  implicit object IntIsEquatable extends Equatable[Integer] {
    override def equal(a: Integer, b: Integer): Boolean = a == b
  }

  implicit object BoolIsEquatable extends Equatable[Boolean] {
    override def equal(a: Boolean, b: Boolean): Boolean = a == b
  }

  implicit object ListIntIsEquatable extends Equatable[List[Integer]] {
    override def equal(a: List[Integer], b: List[Integer]): Boolean = a == b
  }

  implicit object SetIntIsEquatable extends Equatable[Set[Integer]] {
    override def equal(a: Set[Integer], b: Set[Integer]): Boolean = a == b
  }
}

trait NotEquatable[T] {
  def notEqual(a: T, b: T): Boolean
  def distance(a: T, b: T): Int
}

object NotEquatable {
  implicit object IntIsNotEquatable extends NotEquatable[Integer] {
    override def notEqual(a: Integer, b: Integer): Boolean = a != b
    override def distance(a: Integer, b: Integer): Int = (a - b).abs
  }

  implicit object BoolIsNotEquatable extends NotEquatable[Boolean] {
    override def notEqual(a: Boolean, b: Boolean): Boolean = a != b
    override def distance(a: Boolean, b: Boolean): Int = if (a == b) 1 else 0
  }

  implicit object ListIntIsNotEquatable extends NotEquatable[List[Integer]] {
    override def notEqual(a: List[Integer], b: List[Integer]): Boolean = a != b
    override def distance(a: List[Integer], b: List[Integer]): Int = {
      val maxLength = Math.max(a.length, b.length)
      var diff = 0
      for (i <- 0 until maxLength) {
        val aVal = if (i < a.length) a(i) else null
        val bVal = if (i < b.length) b(i) else null
        if (aVal != bVal) {
          diff += 1
        }
      }
      diff - 1 // In optimal scenario distance on border should be 1, so we need to subtract
    }
  }

  implicit object SetIntIsNotEquatable extends NotEquatable[Set[Integer]] {
    override def notEqual(a: Set[Integer], b: Set[Integer]): Boolean = a != b
    override def distance(a: Set[Integer], b: Set[Integer]): Int = {
      // todo: placeholder
      val intersection = a intersect b
      (a.size - intersection.size) + (b.size - intersection.size)
    }
  }
}

trait Contains[L, R] {
  def contains(left: L, right: R): Boolean
}

implicit object ListIntContainsInt extends Contains[List[Integer], Integer] {
  override def contains(left: List[Integer], right: Integer): Boolean = left.contains(right)
}


implicit object SetIntContainsInt extends Contains[Set[Int], Int] {
  override def contains(left: Set[Int], right: Int): Boolean = left.contains(right)
}

trait LessThan[T] {
  def less(a: T, b: T): Boolean
}

object LessThan {
  implicit object IntIsLessThan extends LessThan[Integer] {
    override def less(a: Integer, b: Integer): Boolean = a < b
  }
}

trait GreaterThan[T] {
  def greater(a: T, b: T): Boolean
}

object GreaterThan {
  implicit object IntIsGreaterThan extends GreaterThan[Integer] {
    override def greater(a: Integer, b: Integer): Boolean = a > b
  }
}

trait LessEqual[T] {
  def lessEqual(a: T, b: T): Boolean
}

object LessEqual {
  implicit object IntIsLessEqual extends LessEqual[Integer] {
    override def lessEqual(a: Integer, b: Integer): Boolean = a <= b
  }
}

trait GreaterEqual[T] {
  def greaterEqual(a: T, b: T): Boolean
}

object GreaterEqual {
  implicit object IntIsGreaterEqual extends GreaterEqual[Integer] {
    override def greaterEqual(a: Integer, b: Integer): Boolean = a >= b
  }
}


trait NotComputable[T] {
  def compute(a: T): Boolean
}

object NotComputable {
  implicit object BoolIsNotComputable extends NotComputable[Boolean] {
    override def compute(a: Boolean): Boolean = !a
  }
}