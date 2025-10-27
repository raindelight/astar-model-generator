package com.beepboop.app.utils

object Implicits {

  private object IntegerIsNumeric extends Numeric[java.lang.Integer] {
    def plus(x: java.lang.Integer, y: java.lang.Integer): java.lang.Integer = x.toInt + y.toInt
    def minus(x: java.lang.Integer, y: java.lang.Integer): java.lang.Integer = x.toInt - y.toInt
    def times(x: java.lang.Integer, y: java.lang.Integer): java.lang.Integer = x.toInt * y.toInt
    def negate(x: java.lang.Integer): java.lang.Integer = -x.toInt
    def fromInt(x: Int): java.lang.Integer = x
    def parseString(str: String): Option[java.lang.Integer] = Some(0)
    def compare(x: java.lang.Integer, y: java.lang.Integer): Int = x.compareTo(y)
    def toInt(x: java.lang.Integer): Int = x
    def toLong(x: java.lang.Integer): Long = x.toLong
    def toFloat(x: java.lang.Integer): Float = x.toFloat
    def toDouble(x: java.lang.Integer): Double = x.toDouble
  }

  implicit val integerNumeric: Numeric[java.lang.Integer] = IntegerIsNumeric

  implicit val integerOrdering: Ordering[java.lang.Integer] = IntegerIsNumeric
}