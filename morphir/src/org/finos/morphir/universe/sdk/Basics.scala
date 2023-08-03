package org.finos.morphir.universe.sdk
import org.finos.morphir.universe.MorphirTypeTag
import org.finos.morphir.universe.ir.*

import spire.math.SafeLong
import zio.prelude.*
import PackageNamingContext.morphir.sdk.*

object Basics {
  private implicit val moduleNamingContext: ModuleNamingContext = ModuleNamingContext(ModuleName.fromString("Basics"))

  type Integer = Integer.Type

  object Integer extends Subtype[SafeLong] with HasFQNameInfo {
    val fqName: FQName         = FQName.fqn("Int")
    val fqNameInfo: FQNameInfo = FQNameInfo.fromFQName(fqName)

    def fromInt(value: Int): Integer   = wrap(SafeLong(value))
    def fromLong(value: Long): Integer = wrap(SafeLong(value))

    // implicit val morphirTypeTag: MorphirTypeTag[Integer] = {
    //   val morphirType = ???
    //   MorphirTypeTag.succeed(Type.Reference((), fqName, List.empty))
    // }

    implicit final class IntegerOps(val self: Integer) extends AnyVal {
      def value: SafeLong = unwrap(self)

      def +(that: Integer): Integer = {
        val lhs = self.value
        val rhs = that.value
        wrap(lhs + rhs)
      }

      def add(that: Integer): Integer = this + that
    }
  }

  type Float = Float.Type
  object Float extends Subtype[scala.Double] {
    def fromDouble(value: scala.Double): Float = wrap(value)
    def fromFloat(value: scala.Float): Float   = wrap(value.toDouble)

    implicit final class FloatOps(val self: Float) extends AnyVal {
      def value: Double = unwrap(self)

      def +(that: Float): Float = {
        val lhs = self.value
        val rhs = that.value
        wrap(lhs + rhs)
      }

      def add(that: Float): Float = this + that
    }
  }
}
