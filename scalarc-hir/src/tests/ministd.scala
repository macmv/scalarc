package scala

final abstract class Int private extends AnyVal {
  def toInt: Int
  def toDouble: Double

  def unary_~ : Int
  def unary_+ : Int
  def unary_- : Int

  def ==(x: Int): Boolean
  def ==(x: Double): Boolean

  def !=(x: Int): Boolean
  def !=(x: Double): Boolean

  def <(x: Int): Boolean
  def <(x: Double): Boolean

  def <=(x: Int): Boolean
  def <=(x: Double): Boolean

  def >(x: Int): Boolean
  def >(x: Double): Boolean

  def >=(x: Int): Boolean
  def >=(x: Double): Boolean

  def +(x: Int): Int
  def +(x: Double): Double

  def -(x: Int): Int
  def -(x: Double): Double
}

object Int extends AnyValCompanion {
  final val MinValue = java.lang.Integer.MIN_VALUE
  final val MaxValue = java.lang.Integer.MAX_VALUE

  override def toString = "object scala.Int"
}
