package org.finos.morphir.runtime

import org.finos.morphir.extensibility.SdkModuleDescriptor
import org.finos.morphir.naming._
import org.finos.morphir.runtime.internal.NativeFunctionAdapter
import org.finos.morphir.runtime.sdk._
import org.finos.morphir.runtime.sdk.ListSDK
import org.finos.morphir.{Hints, ModuleDescriptor, MorphirTag, naming}

object NativeSDK {

  import Coercer._

  object Morphir {
    object SDK {

      implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")

      case object String extends SdkModuleDescriptor("String") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(StringSDK.append),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.concat),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.contains),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.dropLeft),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.dropRight),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.endsWith),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.join),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.length),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.padLeft),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.padRight),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.right),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.slice),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.split),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.startsWith),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.toFloat),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.toLower),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.toUpper),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.trim),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.trimLeft),
          NativeFunctionAdapter.Fun1(StringSDK.trimRight)
        )
      }

      case object Basics extends SdkModuleDescriptor("Basics") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(BasicsSDK.ceiling),
          NativeFunctionAdapter.Fun1(BasicsSDK.floor),
          NativeFunctionAdapter.Fun1(BasicsSDK.truncate),
          NativeFunctionAdapter.Fun2(BasicsSDK.integerDivide),
          NativeFunctionAdapter.Fun2(BasicsSDK.always),
          NativeFunctionAdapter.Fun2(BasicsSDK.xor),
          NativeFunctionAdapter.Fun1(BasicsSDK.identity),
          NativeFunctionAdapter.Fun3(BasicsSDK.clamp),
          NativeFunctionAdapter.Fun2(BasicsSDK.power),
          NativeFunctionAdapter.Fun1(BasicsSDK.abs),
          NativeFunctionAdapter.Fun2(BasicsSDK.modBy),
          NativeFunctionAdapter.Fun2(BasicsSDK.remainderBy),
          NativeFunctionAdapter.Fun1(BasicsSDK.sqrt),
          NativeFunctionAdapter.Fun2(BasicsSDK.greaterThan),
          NativeFunctionAdapter.Fun2(BasicsSDK.greaterThanOrEqual),
          NativeFunctionAdapter.Fun2(BasicsSDK.lessThan),
          NativeFunctionAdapter.Fun2(BasicsSDK.lessThanOrEqual),
          NativeFunctionAdapter.Fun3(BasicsSDK.composeRight)
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
          NativeFunctionAdapter.Fun2(ListSDK.cons),
          NativeFunctionAdapter.Fun2(ListSDK.all),
          NativeFunctionAdapter.Fun2(ListSDK.concatMap)
        )
      }

      case object Maybe extends SdkModuleDescriptor(moduleName = "Maybe") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(MaybeSDK.map),
          NativeFunctionAdapter.Fun2(MaybeSDK.withDefault),
          NativeFunctionAdapter.Fun2(MaybeSDK.andThen)
        )
      }

      case object Result extends SdkModuleDescriptor(moduleName = "Result") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(ResultSDK.map),
          NativeFunctionAdapter.Fun2(ResultSDK.mapError),
          NativeFunctionAdapter.Fun2(ResultSDK.withDefault),
          NativeFunctionAdapter.Fun1(ResultSDK.toMaybe),
          NativeFunctionAdapter.Fun2(ResultSDK.fromMaybe),
          NativeFunctionAdapter.Fun2(ResultSDK.andThen)
        )
      }

      case object LocalDate extends SdkModuleDescriptor(moduleName = "LocalDate") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun3(LocalDateSDK.fromCalendarDate),
          NativeFunctionAdapter.Fun2(LocalDateSDK.fromOrdinalDate),
          NativeFunctionAdapter.Fun3(LocalDateSDK.fromParts),
          NativeFunctionAdapter.Fun2(LocalDateSDK.addWeeks),
          NativeFunctionAdapter.Fun2(LocalDateSDK.diffInDays),
          NativeFunctionAdapter.Fun1(LocalDateSDK.fromISO),
          NativeFunctionAdapter.Fun1(LocalDateSDK.year),
          NativeFunctionAdapter.Fun1(LocalDateSDK.month),
          NativeFunctionAdapter.Fun1(LocalDateSDK.monthNumber),
          NativeFunctionAdapter.Fun1(LocalDateSDK.day),
          NativeFunctionAdapter.Fun1(LocalDateSDK.dayOfWeek)
        )

        private val enumSDKConstructor = SDKConstructor(scala.List())

        // Morphir.SDK:LocalDate:Month
        private val monthCtors = RTValue.Month.allFqns.map(fqn => fqn -> enumSDKConstructor).toMap

        // Morphir.SDK:LocalDate:DayOfWeek
        private val dayOfWeekCtors = RTValue.DayOfWeek.allFqns.map(fqn => fqn -> enumSDKConstructor).toMap

        override val ctors: Map[FQName, SDKConstructor] = monthCtors ++ dayOfWeekCtors
      }

      case object LocalTime extends SdkModuleDescriptor(moduleName = "LocalTime") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(LocalTimeSDK.addHours),
          NativeFunctionAdapter.Fun2(LocalTimeSDK.addMinutes),
          NativeFunctionAdapter.Fun2(LocalTimeSDK.addSeconds),
          NativeFunctionAdapter.Fun2(LocalTimeSDK.diffInSeconds),
          NativeFunctionAdapter.Fun1(LocalTimeSDK.fromISO)
        )
      }

      case object Dict extends SdkModuleDescriptor(moduleName = "Dict") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(DictSDK.partition),
          NativeFunctionAdapter.Fun2(DictSDK.remove)
        )
      }
    }
  }

  val modules: Seq[SdkModuleDescriptor] = {
    import Morphir.SDK._
    Seq(
      Basics,
      Dict,
      List,
      LocalDate,
      LocalTime,
      Maybe,
      Result,
      String
    )
  }

  val resolvedFunctions: Map[FQName, SDKValue] = modules.map(_.resolvedFunctions).reduce(_ ++ _)

  val ctors: Map[FQName, SDKConstructor] = modules.map(_.ctors).reduce(_ ++ _)
}
