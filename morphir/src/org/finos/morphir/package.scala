package org.finos
import zio.prelude.*
import spire.math.SafeLong
import zio.prelude.*
import org.finos.morphir.universe.ir.*

package object morphir {

  /// Morphir's canonical repreresentation of an integer.
  /// In the Morphir universe, an `Int` (`MorphirInt`) is an arbitrarily large integer.
  type MorphirInt = MorphirInt.Type

  object MorphirInt extends Subtype[SafeLong] with HasFQNameInfo {
    val fqName: FQName         = FQName.fqn("Morphir.SDK", "Basics", "Int")
    val fqNameInfo: FQNameInfo = FQNameInfo.fromFQName(fqName)

    implicit def fromInt(value: Int): MorphirInt          = wrap(SafeLong(value))
    implicit def fromLong(value: Long): MorphirInt        = wrap(SafeLong(value))
    def fromMorphirInt8(value: MorphirInt8): MorphirInt   = wrap(SafeLong(value))
    def fromMorphirInt16(value: MorphirInt16): MorphirInt = wrap(SafeLong(value))
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
  }

  type MorphirInt16 = Int16.Type
  object Int16 extends Subtype[Short] {
    def fromShort(value: Short): MorphirInt16 = wrap(value)
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
}
