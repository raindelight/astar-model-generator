package com.beepboop.app.components

/* third party modules */

/* own modules */


trait Addable[T] {
  def add(a: T, b: T): T
}

object Addable {
  implicit object IntIsAddable extends Addable[Integer] {
    override def add(a: Integer, b: Integer): Integer = a + b
  }
}


trait Subtractable[T] {
  def sub(a: T, b: T): T
}

object Subtractable {
  implicit object IntIsSubtractable extends Subtractable[Integer] {
    override def sub(a: Integer, b: Integer): Integer = a - b
  }
}

trait Multiplicable[T] {
  def mul(a: T, b: T): T
}

object Multiplicable {
  implicit object IntIsMultiplicable extends Multiplicable[Integer] {
    override def mul(a: Integer, b: Integer): Integer = a * b
  }
}

trait Divisible[T] {
  def div(a: T, b: T): T
}

object Divisible {
  implicit object IntIsDivisible extends Divisible[Integer] {
    override def div(a: Integer, b: Integer): Integer = {
      if(b == 0) return 0
      a / b
    }
  }
}

trait Modulable[T] {
  def mod(a: T, b: T): T
}

object Modulable {
  implicit object IntIsModulable extends Modulable[Integer] {
    override def mod(a: Integer, b: Integer): Integer = {
      if (b == 0) return 0
      a % b
    }
  }
}

trait Andable[T] {
  def and(a: T, b: T): T
}

object Andable {
  implicit object BoolIsAndable extends Andable[Boolean] {
    override def and(a: Boolean, b: Boolean): Boolean = a && b
  }
}

trait Orable[T] {
  def or(a: T, b: T): T
}

object Orable {
  implicit object BoolIsOrable extends Orable[Boolean] {
    override def or(a: Boolean, b: Boolean): Boolean = a || b
  }
}

trait Xorable[T] {
  def xor(a: T, b: T): T
}

object Xorable {
  implicit object BoolIsXorable extends Xorable[Boolean] {
    override def xor(a: Boolean, b: Boolean): Boolean = a ^ b
  }
}

trait Implies[T] {
  def implies(a: T, b: T): T
}

object Implies {
  implicit object BoolImplies extends Implies[Boolean] {
    override def implies(a: Boolean, b: Boolean): Boolean = !a || b
  }
}

trait Negatable[T] {
  def negate(a: T): T
}

object Negatable {
  implicit object IntIsNegable extends Negatable[Int] {
    override def negate(a: Int): Int = 0 - a
    
  }
}



