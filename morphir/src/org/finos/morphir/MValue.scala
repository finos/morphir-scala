package org.finos.morphir
import spire.*
import spire.math.*
import spire.implicits.*

import MValue.PrimitiveFlags

sealed trait MValue[+A] extends Product with Serializable {
  type Type <: A
}

sealed trait MData[+A] extends MValue[A]
sealed trait MPrimitive[+A] extends MData[A] {
  def flags: PrimitiveFlags
}

final case class MInt(value: SafeLong, flags: PrimitiveFlags) extends MPrimitive[MorphirInt] {
  def +(that: MInt): MInt = MInt(value + that.value, flags)
  def -(that: MInt): MInt = MInt(value - that.value, flags)
  def *(that: MInt): MInt = MInt(value * that.value, flags)
  def /(that: MInt): MInt = MInt(value / that.value, flags)
  def %(that: MInt): MInt = MInt(value % that.value, flags)
  def unary_- : MInt      = MInt(-value, flags)

  def toInt: Int   = value.toInt
  def toLong: Long = value.toLong
}
final case class MInt8(value: Byte, flags: PrimitiveFlags)          extends MPrimitive[Byte]
final case class MInt16(value: Short, flags: PrimitiveFlags)        extends MPrimitive[Short]
final case class MInt32(value: Int, flags: PrimitiveFlags)          extends MPrimitive[Int]
final case class MInt64(value: Long, flags: PrimitiveFlags)         extends MPrimitive[Long]
final case class MBool(value: Boolean, flags: PrimitiveFlags)       extends MPrimitive[Boolean]
final case class MFloat(value: MorphirFloat, flags: PrimitiveFlags) extends MPrimitive[MorphirFloat]
final case class MString(value: String, flags: PrimitiveFlags)      extends MPrimitive[String]
case object MUnit                                                   extends MData[scala.Unit]

object MValue {
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

}

object MInt {
  implicit def fromLong(value: Long): MInt = MInt(SafeLong(value), PrimitiveFlags(isLiteral = false))
}
object MBool   {}
object MFloat  {}
object MString {}
