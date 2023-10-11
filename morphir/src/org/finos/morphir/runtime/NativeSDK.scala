package org.finos.morphir.runtime

import org.finos.morphir.extensibility.SdkModuleDescriptor
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.internal.NativeFunctionAdapter
import org.finos.morphir.runtime.sdk._
import org.finos.morphir.runtime.sdk.ListSDK
import org.finos.morphir.{Hints, ModuleDescriptor, MorphirTag, naming}

object NativeSDK {
  import Coercer.*

  object Morphir {
    object SDK {

      implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")

      case object String extends SdkModuleDescriptor("String") {
        // TODO Add methods
        val functions = scala.List()
      }

      case object Basics extends SdkModuleDescriptor("Basics") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(sdk.BasicsSDK.modBy.asNative2),
          NativeFunctionAdapter.Fun2(sdk.BasicsSDK.greaterThan.asNative2),
          NativeFunctionAdapter.Fun2(sdk.BasicsSDK.greaterThanOrEqual.asNative2),
          NativeFunctionAdapter.Fun2(sdk.BasicsSDK.lessThan.asNative2),
          NativeFunctionAdapter.Fun2(sdk.BasicsSDK.lessThanOrEqual.asNative2)
        )
      }

      case object List extends SdkModuleDescriptor("List") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(ListSDK.concat),
          NativeFunctionAdapter.Fun1(ListSDK.singleton),
          NativeFunctionAdapter.Fun1(ListSDK.isEmpty),
          NativeFunctionAdapter.Fun1(ListSDK.length),
          NativeFunctionAdapter.Fun2(ListSDK.filter),
          NativeFunctionAdapter.Fun2(ListSDK.map),
          NativeFunctionAdapter.Fun2(ListSDK.any),
          NativeFunctionAdapter.Fun2(ListSDK.partition),
          NativeFunctionAdapter.Fun3(ListSDK.foldl),
          NativeFunctionAdapter.Fun2(ListSDK.append),
          NativeFunctionAdapter.Fun2(ListSDK.cons)
        )
      }
      case object Maybe extends SdkModuleDescriptor(moduleName = "Maybe") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(MaybeSDK.map),
          NativeFunctionAdapter.Fun2(MaybeSDK.withDefault)
        )
      }
      case object Result extends SdkModuleDescriptor(moduleName = "Result") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(ResultSDK.map),
          NativeFunctionAdapter.Fun2(ResultSDK.mapError),
          NativeFunctionAdapter.Fun2(ResultSDK.withDefault),
          NativeFunctionAdapter.Fun1(ResultSDK.toMaybe),
          NativeFunctionAdapter.Fun2(ResultSDK.fromMaybe)
        )

      }
    }
  }

  val resolvedFunctions: Map[FQName, SDKValue] = {
    import Morphir.SDK.*
    Basics.resolvedFunctions ++ List.resolvedFunctions ++ Maybe.resolvedFunctions ++ Result.resolvedFunctions
  }
}
