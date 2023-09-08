package org.finos

import zio.prelude._
import spire.math.SafeLong
import zio.prelude._
import org.finos.morphir.naming._
import org.finos.morphir.universe.ir._

package object morphir {

  /// Morphir's canonical repreresentation of an integer.
  /// In the Morphir universe, an `Int` (`MorphirInt`) is an arbitrarily large integer.
  type MorphirInt = MorphirInt.Type

  object MorphirInt extends Subtype[SafeLong] with HasFQNameInfo {
    val fqName: FQName         = FQName.fqn("Morphir.SDK", "Basics", "Int")
    val fqNameInfo: FQNameInfo = FQNameInfo.fromFQName(fqName)

    implicit def fromInt(value: Int): MorphirInt          = wrap(SafeLong(value))
    implicit def fromLong(value: Long): MorphirInt        = wrap(SafeLong(value))
    def fromMorphirInt8(value: MorphirInt8): MorphirInt   = wrap(SafeLong(value.toInt))
    def fromMorphirInt16(value: MorphirInt16): MorphirInt = wrap(SafeLong(value.toInt))
    def fromMorphirInt32(value: MorphirInt32): MorphirInt = wrap(SafeLong(value))
    def fromMorphirInt64(value: MorphirInt64): MorphirInt = wrap(SafeLong(value))

    // implicit val morphirTypeTag: MorphirTypeTag[Integer] = {
    //   val morphirType = ???
    //   MorphirTypeTag.succeed(Type.Reference((), fqName, List.empty))
    // }

    implicit final class MorphirIntSyntax(private val self: MorphirInt) extends AnyVal {
      def value: SafeLong = unwrap(self)

      def +(that: MorphirInt): MorphirInt = {
        val lhs = self.value
        val rhs = that.value
        wrap(lhs + rhs)
      }

      @inline def add(that: MorphirInt): MorphirInt = this + that
    }
  }

  type MorphirInt8 = Int8.Type
  object Int8 extends Subtype[Byte] {
    def fromByte(value: Byte): MorphirInt8 = wrap(value)
    final implicit class Int8Ops(val self: MorphirInt8) extends AnyVal {
      def value: Byte = unwrap(self)
      def toInt: Int  = value.toInt
    }
  }

  type MorphirInt16 = Int16.Type
  object Int16 extends Subtype[Short] {
    def fromShort(value: Short): MorphirInt16 = wrap(value)
    implicit final class Int16Ops(val self: MorphirInt16) extends AnyVal {
      def value: Short = unwrap(self)
      def toInt: Int   = value.toInt
    }
  }

  type MorphirInt32 = MorphirInt32.Type
  object MorphirInt32 extends Subtype[Int] {
    def fromInt(value: Int): MorphirInt32 = wrap(value)
  }

  type MorphirInt64 = MorphirInt64.Type
  object MorphirInt64 extends Subtype[Long] {
    def fromLong(value: Long): MorphirInt64 = wrap(value)
  }

  type MorphirFloat = MorphirFloat.Type
  object MorphirFloat extends Subtype[scala.Double] {
    def fromDouble(value: Double): MorphirFloat      = wrap(value)
    def fromMorphirFloat(value: Float): MorphirFloat = wrap(value.toDouble)

    implicit final class MorphirFloatSyntax(val self: MorphirFloat) extends AnyVal {
      def value: Double = unwrap(self)

      def +(that: MorphirFloat): MorphirFloat = {
        val lhs = self.value
        val rhs = that.value
        wrap(lhs + rhs)
      }

      def add(that: MorphirFloat): MorphirFloat = this + that
    }
  }

  final implicit class ZValidationOps[+W, +E, +A](private val self: ZValidation[W, E, A]) extends AnyVal {
    def isFailure: Boolean = self.fold(_ => true, _ => false)
    def errors: List[E]    = self.fold(_.toList, _ => Nil)
  }

  implicit val mIntIsIntegral: scala.Integral[MInt] = new scala.math.Integral[MInt] {
    override def plus(x: MInt, y: MInt): MInt           = x + y
    override def minus(x: MInt, y: MInt): MInt          = x - y
    override def times(x: MInt, y: MInt): MInt          = x * y
    override def negate(x: MInt): MInt                  = x.unary_-
    override def fromInt(x: Int): MInt                  = MInt.fromInt(x)
    override def parseString(str: String): Option[MInt] = MInt.fromString(str)
    override def toInt(x: MInt): Int                    = x.toInt
    override def toLong(x: MInt): Long                  = x.toLong
    override def toFloat(x: MInt): Float                = x.toFloat
    override def toDouble(x: MInt): Double              = x.toDouble
    override def compare(x: MInt, y: MInt): Int         = x.compare(y)

    override def quot(x: MInt, y: MInt): MInt = x / y
    override def rem(x: MInt, y: MInt): MInt  = x % y
  }

  // Has to be After mIntIsIntegral or it will assign a null reference
  implicit val mIntIsNumeric: scala.Numeric[MInt] = mIntIsIntegral
}
