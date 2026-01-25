package com.beepboop.app.components


trait Equatable[T] extends Serializable {
  def equal(a: T, b: T): Boolean
  def distance(a: T, b: T): Int
}

object Equatable {
  implicit object IntIsEquatable extends Equatable[Integer] {
    override def equal(a: Integer, b: Integer): Boolean = a == b

    override def distance(a: Integer, b: Integer): Int = Math.abs(a - b)
  }

  implicit object BoolIsEquatable extends Equatable[Boolean] {
    override def equal(a: Boolean, b: Boolean): Boolean = a == b

    override def distance(a: Boolean, b: Boolean): Int = if (a == b) 0 else 1
  }

  implicit object ListIntIsEquatable extends Equatable[List[Integer]] {
    override def equal(a: List[Integer], b: List[Integer]): Boolean = {
      require(a.size == b.size, "arrays must be equal length")
      a == b
    }
    override def distance(a: List[Integer], b: List[Integer]): Int = {
      if (a.size != b.size) {
        1000 + Math.abs(a.size - b.size)
      } else {
        a.zip(b).map { case (v1, v2) => Math.abs(v1 - v2) }.sum
      }
    }
  }

  implicit object SetIntIsEquatable extends Equatable[Set[Integer]] {
    override def equal(a: Set[Integer], b: Set[Integer]): Boolean = a == b

    override def distance(a: Set[Integer], b: Set[Integer]): Int = {
      (a diff b).size + (b diff a).size
    }
  }
}
trait NotEquatable[T] extends Serializable{
  def notEqual(a: T, b: T): Boolean
  def distance(a: T, b: T): Int
}

object NotEquatable {
  implicit object IntIsNotEquatable extends NotEquatable[Integer] {
    override def notEqual(a: Integer, b: Integer): Boolean = a != b
    override def distance(a: Integer, b: Integer): Int = {
      Math.abs(Math.abs(a - b) - 1)
    }
  }

  implicit object BoolIsNotEquatable extends NotEquatable[Boolean] {
    override def notEqual(a: Boolean, b: Boolean): Boolean = a != b
    override def distance(a: Boolean, b: Boolean): Int = if (a != b) 0 else 1
  }

  implicit object ListIntIsNotEquatable extends NotEquatable[List[Integer]] {
    override def notEqual(a: List[Integer], b: List[Integer]): Boolean = {
      require(a.size == b.size, "arrays must be equal length")
      a != b
    }
    override def distance(a: List[Integer], b: List[Integer]): Int = {
      require(a.size == b.size, "arrays must be equal length")
      if (a != b) {
        val diffCount = a.zip(b).count { case (i, j) => i != j }
        diffCount - 1
      } else {
        1
      }
    }
  }
  implicit object SetIntIsNotEquatable extends NotEquatable[Set[Integer]] {
    override def notEqual(a: Set[Integer], b: Set[Integer]): Boolean = a != b
    override def distance(a: Set[Integer], b: Set[Integer]): Int = {
      val diffSize = (a diff b).size + (b diff a).size
      Math.abs(diffSize - 1)
    }
  }
}

trait Contains[L, R] extends Serializable{
  def contains(left: L, right: R): Boolean
  def distance(left: L, right: R): Int
}

implicit object ListIntContainsInt extends Contains[List[Integer], Integer] {
  override def contains(left: List[Integer], right: Integer): Boolean = left.contains(right)

  override def distance(left: List[Integer], right: Integer): Int = {
    if (left.isEmpty) {
      return 1000
    }
    val occurrences = left.count(_ == right)
    if (occurrences > 0) {
      occurrences - 1
    } else {
      left.map(x => (x - right).abs).min
    }
  }
}

implicit object SetIntContainsInt extends Contains[Set[Int], Int] {
  override def contains(left: Set[Int], right: Int): Boolean = left.contains(right)

  override def distance(left: Set[Int], right: Int): Int = {
    if (left.isEmpty) return Int.MaxValue
    if (left.contains(right)) 0
    else left.map(x => (x - right).abs).min
  }
}

trait LessThan[T] extends Serializable{
  def less(a: T, b: T): Boolean
  def distance(a: T, b: T): Integer
}

object LessThan {
  implicit object IntIsLessThan extends LessThan[Integer] {
    override def less(a: Integer, b: Integer): Boolean = a < b

    override def distance(a: Integer, b: Integer): Integer = {
      if (a < b) {
        (b - 1) - a
      } else {
        (a - b) + 1
      }
    }

  }
}

trait GreaterThan[T] extends Serializable {
  def greater(a: T, b: T): Boolean

  def distance(a: T, b: T): Integer
}

object GreaterThan {
  implicit object IntIsGreaterThan extends GreaterThan[Integer] {
    override def greater(a: Integer, b: Integer): Boolean = a > b

    override def distance(a: Integer, b: Integer): Integer = {
      if (a > b) {
        a - (b + 1)
      } else {
        (b + 1) - a
      }
    }
  }
}


trait LessEqual[T] extends Serializable{
  def lessEqual(a: T, b: T): Boolean
  def distance(a: T, b: T): Integer
}

object LessEqual {
  implicit object IntIsLessEqual extends LessEqual[Integer] {
    override def lessEqual(a: Integer, b: Integer): Boolean = a <= b
    override def distance(a: Integer, b: Integer): Integer  = {
      Math.abs(a - b)
    }
  }
}

trait GreaterEqual[T] extends Serializable{
  def greaterEqual(a: T, b: T): Boolean
  def distance(a: T, b: T): Integer
}

object GreaterEqual {
  implicit object IntIsGreaterEqual extends GreaterEqual[Integer] {
    override def greaterEqual(a: Integer, b: Integer): Boolean = a >= b
    override def distance(a: Integer, b: Integer): Integer  = {
      Math.abs(a - b)
    }
  }
}

trait NotComputable[T] extends Serializable{
  def compute(a: T): Boolean
}

object NotComputable {
  implicit object BoolIsNotComputable extends NotComputable[Boolean] {
    override def compute(a: Boolean): Boolean = !a
  }
}