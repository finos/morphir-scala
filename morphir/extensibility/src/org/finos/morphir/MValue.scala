package org.finos.morphir
import spire._
import spire.math._
import spire.implicits._

import MValue.{MorphirDecimal, MorphirFloat, MorphirInt, PrimitiveFlags}

sealed trait MValue[+A] extends Product with Serializable {
  type Type <: A
  def value: A
}

sealed trait MData[+A] extends MValue[A]
sealed trait MPrimitive[+A] extends MData[A] {
  def flags: PrimitiveFlags
}

final case class MInt(value: MorphirInt, flags: PrimitiveFlags) extends MPrimitive[MorphirInt] {
  def +(that: MInt): MInt = MInt(value + that.value, flags)
  def -(that: MInt): MInt = MInt(value - that.value, flags)
  def *(that: MInt): MInt = MInt(value * that.value, flags)
  def /(that: MInt): MInt = MInt(value / that.value, flags)
  def %(that: MInt): MInt = MInt(value % that.value, flags)
  def unary_- : MInt      = MInt(-value, flags)

  def toShort: Short           = value.toShort
  def toInt: Int               = value.toInt
  def toLong: Long             = value.toLong
  def toFloat: Float           = value.toFloat
  def toDouble: Double         = value.toDouble
  def toBigDecimal: BigDecimal = value.toBigDecimal
  def isValidInt: Boolean      = value.isValidInt
  def isValidLong: Boolean     = value.isValidLong

  def compare(other: MInt): Int = value.compare(other.value)
}

final case class MInt8(value: Byte, flags: PrimitiveFlags)          extends MPrimitive[Byte]
final case class MInt16(value: Short, flags: PrimitiveFlags)        extends MPrimitive[Short]
final case class MInt32(value: Int, flags: PrimitiveFlags)          extends MPrimitive[Int]
final case class MInt64(value: Long, flags: PrimitiveFlags)         extends MPrimitive[Long]
final case class MBool(value: Boolean, flags: PrimitiveFlags)       extends MPrimitive[Boolean]
final case class MFloat(value: MorphirFloat, flags: PrimitiveFlags) extends MPrimitive[MorphirFloat]
final case class MString(value: String, flags: PrimitiveFlags)      extends MPrimitive[String]
case object MUnit extends MData[scala.Unit] {
  val value = ()
}

object MValue {
  type MorphirInt     = SafeLong
  type MorphirFloat   = Double
  type MorphirDecimal = BigDecimal

  val unit: MValue[scala.Unit]                   = MUnit
  def int(value: MorphirInt): MValue[MorphirInt] = MInt(value, PrimitiveFlags(isLiteral = false))
  def int(value: MorphirInt, isLiteral: Boolean): MValue[MorphirInt] =
    MInt(value, PrimitiveFlags(isLiteral = isLiteral))
  def bool(value: Boolean): MValue[Boolean]                     = MBool(value, PrimitiveFlags(isLiteral = false))
  def bool(value: Boolean, isLiteral: Boolean): MValue[Boolean] = MBool(value, PrimitiveFlags(isLiteral = isLiteral))
  def float(value: MorphirFloat): MValue[MorphirFloat]          = MFloat(value, PrimitiveFlags(isLiteral = false))
  def float(value: MorphirFloat, isLiteral: Boolean): MValue[MorphirFloat] =
    MFloat(value, PrimitiveFlags(isLiteral = isLiteral))
  def string(value: String): MValue[String]                     = MString(value, PrimitiveFlags(isLiteral = false))
  def string(value: String, isLiteral: Boolean): MValue[String] = MString(value, PrimitiveFlags(isLiteral = isLiteral))

  final case class PrimitiveFlags(isLiteral: Boolean)
  object PrimitiveFlags {}

  object Value {
    def unapply[A](value: MValue[A]): Option[A] = Some(value.value)
  }
}

object MInt {
  def fromInt(value: Int): MInt            = MInt(SafeLong(value), PrimitiveFlags(isLiteral = false))
  def fromIntLiteral(value: Int): MInt     = MInt(SafeLong(value), PrimitiveFlags(isLiteral = true))
  implicit def fromLong(value: Long): MInt = MInt(SafeLong(value), PrimitiveFlags(isLiteral = false))
  def fromLongLiteral(value: Long): MInt   = MInt(SafeLong(value), PrimitiveFlags(isLiteral = true))

  @throws[NumberFormatException]
  def fromStringUnsafe(str: String): MInt = MInt(BigInt(str), PrimitiveFlags(isLiteral = false))

  def fromString(str: String): Option[MInt] =
    try
      Some(fromStringUnsafe(str))
    catch {
      case _: NumberFormatException => None
    }
}
object MBool   {}
object MFloat  {}
object MString {}
