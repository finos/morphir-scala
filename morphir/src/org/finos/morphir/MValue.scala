package org.finos.morphir
import MValue.PrimitiveFlags

sealed trait MValue extends Product with Serializable {}

sealed trait MData extends MValue
sealed trait MPrimitive extends MData {
  def flags: PrimitiveFlags
}

final case class MInt(value: MorphirInt, flags: PrimitiveFlags)     extends MPrimitive
final case class MBool(value: Boolean, flags: PrimitiveFlags)       extends MPrimitive
final case class MFloat(value: MorphirFloat, flags: PrimitiveFlags) extends MPrimitive
final case class MString(value: String, flags: PrimitiveFlags)      extends MPrimitive
case object MUnit                                                   extends MData

object MValue {
  val unit: MValue                                       = MUnit
  def int(value: MorphirInt): MValue                     = MInt(value, PrimitiveFlags(isLiteral = false))
  def int(value: MorphirInt, isLiteral: Boolean): MValue = MInt(value, PrimitiveFlags(isLiteral = isLiteral))
  def bool(value: Boolean): MBool                        = MBool(value, PrimitiveFlags(isLiteral = false))
  def bool(value: Boolean, isLiteral: Boolean): MValue   = MBool(value, PrimitiveFlags(isLiteral = isLiteral))
  def float(value: MorphirFloat): MFloat                 = MFloat(value, PrimitiveFlags(isLiteral = false))
  def float(value: MorphirFloat, isLiteral: Boolean): MValue =
    MFloat(value, PrimitiveFlags(isLiteral = isLiteral))
  def string(value: String): MValue = MString(value, PrimitiveFlags(isLiteral = false))
  def string(value: String, isLiteral: Boolean): MValue =
    MString(value, PrimitiveFlags(isLiteral = isLiteral))

  final case class PrimitiveFlags(isLiteral: Boolean)
  object PrimitiveFlags {}

}

object MInt    {}
object MBool   {}
object MFloat  {}
object MString {}
