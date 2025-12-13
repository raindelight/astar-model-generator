package com.beepboop.app.components

/* third party modules */

/* own modules */


trait Addable[T] extends Serializable{
  def add(a: T, b: T): T
}

object Addable {
  implicit object IntIsAddable extends Addable[Integer] {
    override def add(a: Integer, b: Integer): Integer = a + b
  }
}


trait Subtractable[T] extends Serializable{
  def sub(a: T, b: T): T
}

object Subtractable {
  implicit object IntIsSubtractable extends Subtractable[Integer] {
    override def sub(a: Integer, b: Integer): Integer = a - b
  }
}

trait Multiplicable[T] extends Serializable{
  def mul(a: T, b: T): T
}

object Multiplicable {
  implicit object IntIsMultiplicable extends Multiplicable[Integer] {
    override def mul(a: Integer, b: Integer): Integer = a * b
  }
}

trait Divisible[T] extends Serializable{
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

trait Modulable[T] extends Serializable{
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

trait Andable[T] extends Serializable{
  def and(a: T, b: T): T
}

object Andable {
  implicit object BoolIsAndable extends Andable[Boolean] {
    override def and(a: Boolean, b: Boolean): Boolean = a && b
  }
}

trait Orable[T] extends Serializable{
  def or(a: T, b: T): T
}

object Orable {
  implicit object BoolIsOrable extends Orable[Boolean] {
    override def or(a: Boolean, b: Boolean): Boolean = a || b
  }
}

trait Xorable[T] extends Serializable{
  def xor(a: T, b: T): T
}

object Xorable {
  implicit object BoolIsXorable extends Xorable[Boolean] {
    override def xor(a: Boolean, b: Boolean): Boolean = a ^ b
  }
}

trait Implies[T] extends Serializable{
  def implies(a: T, b: T): T
}

object Implies {
  implicit object BoolImplies extends Implies[Boolean] {
    override def implies(a: Boolean, b: Boolean): Boolean = !a || b
  }
}

trait Negatable[T] extends Serializable{
  def negate(a: T): T
}

object Negatable {
  implicit object IntIsNegatable extends Negatable[Integer] {
    override def negate(a: Integer): Integer = 0 - a
  }
}

trait Absolutable[T] extends Serializable{
  def abs(a: T): T
}

object Absolutable {
  implicit object IntIsAbsolutable extends Absolutable[Integer] {
    override def abs(a: Integer): Integer = java.lang.Math.abs(a)
  }
}

trait BoolToIntConvertible[T] extends Serializable{
  def convert(a: T): Integer
}

object BoolToIntConvertible {
  implicit object BoolIsConvertible extends BoolToIntConvertible[Boolean] {
    override def convert(a: Boolean): Integer = if (a) 1 else 0
  }
}



