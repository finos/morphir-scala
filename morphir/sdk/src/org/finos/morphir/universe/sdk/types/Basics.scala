package org.finos.morphir.universe.sdk.types
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

    // implicit val morphirTypeTag: MorphirTypeTag[Integer] = {
    //   val morphirType = ???
    //   MorphirTypeTag.succeed(Type.Reference((), fqName, List.empty))
    // }

    implicit final class IntegerOps(val self: Integer) extends AnyVal {
      def value: SafeLong = unwrap(self)
    }
  }

  type Float = Float.Type
  object Float extends Subtype[scala.Double] {
    implicit final class FloatOps(val self: Float) extends AnyVal {
      def value: Double = unwrap(self)
    }
  }
}
