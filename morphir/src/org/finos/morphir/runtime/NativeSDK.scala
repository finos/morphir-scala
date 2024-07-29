package org.finos.morphir.runtime

import org.finos.morphir.extensibility.SdkModuleDescriptor
import org.finos.morphir.naming._
import org.finos.morphir.runtime.internal.NativeFunctionAdapter
import org.finos.morphir.runtime.sdk._
import org.finos.morphir.{Hints, ModuleDescriptor, MorphirTag, naming}

object NativeSDK {

  import Coercer._

  object Morphir {
    object SDK {

      implicit val packageName: PackageName = PackageName.fromString("Morphir.SDK")

      case object Aggregate extends SdkModuleDescriptor(moduleName = "Aggregate") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(AggregateSDK.groupBy),
          NativeFunctionAdapter.Fun3(AggregateSDK.aggregateMap),
          NativeFunctionAdapter.Fun4(AggregateSDK.aggregateMap2),
          NativeFunctionAdapter.Fun5(AggregateSDK.aggregateMap3),
          NativeFunctionAdapter.Fun6(AggregateSDK.aggregateMap4),
          NativeFunctionAdapter.Fun1(AggregateSDK.sumOf),
          NativeFunctionAdapter.Fun1(AggregateSDK.minimumOf),
          NativeFunctionAdapter.Fun1(AggregateSDK.maximumOf),
          NativeFunctionAdapter.Fun1(AggregateSDK.averageOf),
          NativeFunctionAdapter.Fun2(AggregateSDK.weightedAverageOf),
          NativeFunctionAdapter.Fun2(AggregateSDK.byKey),
          NativeFunctionAdapter.Fun2(AggregateSDK.withFilter)
        )
      }

      case object Char extends SdkModuleDescriptor("Char") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isUpper),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isLower),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isAlpha),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isAlphaNum),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isDigit),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isOctDigit),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.isHexDigit),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.toUpper),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.toLower),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.toLocaleUpper),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.toLocaleLower),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.toCode),
          NativeFunctionAdapter.Fun1(sdk.CharSDK.fromCode)
        )
      }

      case object String extends SdkModuleDescriptor("String") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(StringSDK.append),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.repeat),
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
          NativeFunctionAdapter.Fun1(sdk.StringSDK.trimRight),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.fromChar),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.cons),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.uncons),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.toList),
          NativeFunctionAdapter.Fun1(sdk.StringSDK.fromList),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.pad),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.map),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.filter),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.foldl),
          NativeFunctionAdapter.Fun3(sdk.StringSDK.foldr),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.any),
          NativeFunctionAdapter.Fun2(sdk.StringSDK.all)
        )
      }

      case object Basics extends SdkModuleDescriptor("Basics") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(BasicsSDK.ceiling),
          NativeFunctionAdapter.Fun1(BasicsSDK.floor),
          NativeFunctionAdapter.Fun1(BasicsSDK.truncate),
          NativeFunctionAdapter.Fun2(BasicsSDK.integerDivide),
          NativeFunctionAdapter.Fun1(BasicsSDK.isInfinite),
          NativeFunctionAdapter.Fun1(BasicsSDK.isNaN),
          NativeFunctionAdapter.Fun2(BasicsSDK.always),
          NativeFunctionAdapter.Fun2(BasicsSDK.equal),
          NativeFunctionAdapter.Fun2(BasicsSDK.notEqual),
          NativeFunctionAdapter.Fun2(BasicsSDK.lessThan),
          NativeFunctionAdapter.Fun2(BasicsSDK.greaterThan),
          NativeFunctionAdapter.Fun2(BasicsSDK.greaterThanOrEqual),
          NativeFunctionAdapter.Fun2(BasicsSDK.lessThanOrEqual),
          NativeFunctionAdapter.Fun2(BasicsSDK.min),
          NativeFunctionAdapter.Fun2(BasicsSDK.max),
          NativeFunctionAdapter.Fun2(BasicsSDK.compare),
          NativeFunctionAdapter.Fun2(BasicsSDK.xor),
          NativeFunctionAdapter.Fun1(BasicsSDK.identity),
          NativeFunctionAdapter.Fun3(BasicsSDK.clamp),
          NativeFunctionAdapter.Fun2(BasicsSDK.power),
          NativeFunctionAdapter.Fun1(BasicsSDK.abs),
          NativeFunctionAdapter.Fun2(BasicsSDK.modBy),
          NativeFunctionAdapter.Fun2(BasicsSDK.remainderBy),
          NativeFunctionAdapter.Fun1(BasicsSDK.sqrt),
          NativeFunctionAdapter.Fun3(BasicsSDK.composeRight),
          NativeFunctionAdapter.Fun3(BasicsSDK.composeLeft),
          NativeFunctionAdapter.Fun1(BasicsSDK.cos),
          NativeFunctionAdapter.Fun1(BasicsSDK.sin),
          NativeFunctionAdapter.Fun1(BasicsSDK.tan),
          NativeFunctionAdapter.Fun1(BasicsSDK.acos),
          NativeFunctionAdapter.Fun1(BasicsSDK.asin),
          NativeFunctionAdapter.Fun1(BasicsSDK.atan),
          NativeFunctionAdapter.Fun2(BasicsSDK.atan2),
          NativeFunctionAdapter.Fun1(BasicsSDK.degrees),
          NativeFunctionAdapter.Fun1(BasicsSDK.radians),
          NativeFunctionAdapter.Fun1(BasicsSDK.turns),
          NativeFunctionAdapter.Fun1(BasicsSDK.round),
          NativeFunctionAdapter.Fun1(BasicsSDK.fromPolar),
          NativeFunctionAdapter.Fun1(BasicsSDK.toPolar)
        )

        private val enumSDKConstructor = SDKConstructor.Explicit(scala.List())

        override val ctors: Map[FQName, SDKConstructor] =
          RTValue.Order.allFqns.map(fqn => fqn -> enumSDKConstructor).toMap
      }

      case object Decimal extends SdkModuleDescriptor("Decimal") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(DecimalSDK.abs),
          NativeFunctionAdapter.Fun2(DecimalSDK.add),
          NativeFunctionAdapter.Fun1(DecimalSDK.bps),
          NativeFunctionAdapter.Fun2(DecimalSDK.compare),
          NativeFunctionAdapter.Fun2(DecimalSDK.div),
          NativeFunctionAdapter.Fun3(DecimalSDK.divWithDefault),
          NativeFunctionAdapter.Fun2(DecimalSDK.eq),
          NativeFunctionAdapter.Fun1(DecimalSDK.fromFloat),
          NativeFunctionAdapter.Fun1(DecimalSDK.fromInt),
          NativeFunctionAdapter.Fun1(DecimalSDK.fromString),
          NativeFunctionAdapter.Fun2(DecimalSDK.gt),
          NativeFunctionAdapter.Fun2(DecimalSDK.gte),
          NativeFunctionAdapter.Fun1(DecimalSDK.hundred),
          NativeFunctionAdapter.Fun1(DecimalSDK.hundredth),
          NativeFunctionAdapter.Fun2(DecimalSDK.lt),
          NativeFunctionAdapter.Fun2(DecimalSDK.lte),
          NativeFunctionAdapter.Fun1(DecimalSDK.million),
          NativeFunctionAdapter.Fun1(DecimalSDK.millionth),
          NativeFunctionAdapter.Fun2(DecimalSDK.mul),
          NativeFunctionAdapter.Fun1(DecimalSDK.negate),
          NativeFunctionAdapter.Fun2(DecimalSDK.neq),
          NativeFunctionAdapter.Fun1(DecimalSDK.round),
          NativeFunctionAdapter.Fun2(DecimalSDK.sub),
          NativeFunctionAdapter.Fun1(DecimalSDK.tenth),
          NativeFunctionAdapter.Fun1(DecimalSDK.thousand),
          NativeFunctionAdapter.Fun1(DecimalSDK.thousandth),
          NativeFunctionAdapter.Fun1(DecimalSDK.toFloat),
          NativeFunctionAdapter.Fun1(DecimalSDK._toString),
          NativeFunctionAdapter.Fun1(DecimalSDK.truncate),
          NativeFunctionAdapter.Fun2(DecimalSDK.shiftDecimalLeft),
          NativeFunctionAdapter.Fun2(DecimalSDK.shiftDecimalRight)
        )
      }

      case object Key extends SdkModuleDescriptor(moduleName = "Key") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun1(KeySDK.key0)
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
          NativeFunctionAdapter.Fun3(ListSDK.map2),
          NativeFunctionAdapter.Fun4(ListSDK.map3),
          NativeFunctionAdapter.Fun5(ListSDK.map4),
          NativeFunctionAdapter.Fun6(ListSDK.map5),
          NativeFunctionAdapter.Fun2(ListSDK.any),
          NativeFunctionAdapter.Fun1(ListSDK.maximum),
          NativeFunctionAdapter.Fun1(ListSDK.minimum),
          NativeFunctionAdapter.Fun2(ListSDK.partition),
          NativeFunctionAdapter.Fun3(ListSDK.foldl),
          NativeFunctionAdapter.Fun2(ListSDK.append),
          NativeFunctionAdapter.Fun2(ListSDK.cons),
          NativeFunctionAdapter.Fun2(ListSDK.all),
          NativeFunctionAdapter.Fun2(ListSDK.concatMap),
          NativeFunctionAdapter.Fun2(ListSDK.drop),
          NativeFunctionAdapter.Fun2(ListSDK.filterMap),
          NativeFunctionAdapter.Fun3(ListSDK.foldr),
          NativeFunctionAdapter.Fun1(ListSDK.sort),
          NativeFunctionAdapter.Fun2(ListSDK.sortBy),
          NativeFunctionAdapter.Fun2(ListSDK.sortWith),
          NativeFunctionAdapter.Fun1(ListSDK.head),
          NativeFunctionAdapter.Fun2(ListSDK.indexedMap),
          NativeFunctionAdapter.Fun2(ListSDK.member),
          NativeFunctionAdapter.Fun2(ListSDK.range),
          NativeFunctionAdapter.Fun2(ListSDK.repeat),
          NativeFunctionAdapter.Fun1(ListSDK.reverse),
          NativeFunctionAdapter.Fun1(ListSDK.tail),
          NativeFunctionAdapter.Fun2(ListSDK.take),
          NativeFunctionAdapter.Fun1(ListSDK.sum),
          NativeFunctionAdapter.Fun1(ListSDK.product),
          NativeFunctionAdapter.Fun2(ListSDK.intersperse),
          NativeFunctionAdapter.Fun1(ListSDK.unzip),
          NativeFunctionAdapter.Fun3(ListSDK.innerJoin),
          NativeFunctionAdapter.Fun3(ListSDK.leftJoin)
        )
      }

      case object Maybe extends SdkModuleDescriptor(moduleName = "Maybe") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(MaybeSDK.map),
          NativeFunctionAdapter.Fun2(MaybeSDK.withDefault),
          NativeFunctionAdapter.Fun2(MaybeSDK.andThen),
          NativeFunctionAdapter.Fun3(MaybeSDK.map2),
          NativeFunctionAdapter.Fun4(MaybeSDK.map3),
          NativeFunctionAdapter.Fun5(MaybeSDK.map4),
          NativeFunctionAdapter.Fun1(MaybeSDK.hasValue)
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
          NativeFunctionAdapter.Fun1(LocalDateSDK.day),
          NativeFunctionAdapter.Fun1(LocalDateSDK.dayOfWeek),
          NativeFunctionAdapter.Fun1(LocalDateSDK.fromISO),
          NativeFunctionAdapter.Fun1(LocalDateSDK.month),
          NativeFunctionAdapter.Fun1(LocalDateSDK.monthNumber),
          NativeFunctionAdapter.Fun1(LocalDateSDK.year),
          NativeFunctionAdapter.Fun2(LocalDateSDK.addDays),
          NativeFunctionAdapter.Fun2(LocalDateSDK.addWeeks),
          NativeFunctionAdapter.Fun2(LocalDateSDK.addYears),
          NativeFunctionAdapter.Fun2(LocalDateSDK.diffInDays),
          NativeFunctionAdapter.Fun2(LocalDateSDK.diffInWeeks),
          NativeFunctionAdapter.Fun2(LocalDateSDK.diffInMonths),
          NativeFunctionAdapter.Fun2(LocalDateSDK.diffInYears),
          NativeFunctionAdapter.Fun1(LocalDateSDK.monthToInt),
          NativeFunctionAdapter.Fun1(LocalDateSDK.isWeekend),
          NativeFunctionAdapter.Fun1(LocalDateSDK.isWeekday),
          NativeFunctionAdapter.Fun2(LocalDateSDK.addMonths),
          NativeFunctionAdapter.Fun2(LocalDateSDK.fromOrdinalDate),
          NativeFunctionAdapter.Fun3(LocalDateSDK.fromCalendarDate),
          NativeFunctionAdapter.Fun3(LocalDateSDK.fromParts),
          NativeFunctionAdapter.Fun1(LocalDateSDK.toISOString)
        )

        private val enumSDKConstructor = SDKConstructor.Explicit(scala.List())

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
          NativeFunctionAdapter.Fun2(LocalTimeSDK.diffInHours),
          NativeFunctionAdapter.Fun2(LocalTimeSDK.diffInMinutes),
          NativeFunctionAdapter.Fun1(LocalTimeSDK.fromISO)
        )
      }

      case object Dict extends SdkModuleDescriptor(moduleName = "Dict") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun2(DictSDK.partition),
          NativeFunctionAdapter.Fun2(DictSDK.remove),
          NativeFunctionAdapter.Fun2(DictSDK.diff),
          NativeFunctionAdapter.Fun2(DictSDK.intersect),
          NativeFunctionAdapter.Fun2(DictSDK.union),
          NativeFunctionAdapter.Fun3(DictSDK.foldl),
          NativeFunctionAdapter.Fun3(DictSDK.foldr),
          NativeFunctionAdapter.Fun2(DictSDK.map),
          NativeFunctionAdapter.Fun6(DictSDK.merge)
        )
      }

      case object Set extends SdkModuleDescriptor(moduleName = "Set") {
        val functions: List[NativeFunctionAdapter] = scala.List(
          NativeFunctionAdapter.Fun3(SetSDK.foldr),
          NativeFunctionAdapter.Fun3(SetSDK.foldl),
          NativeFunctionAdapter.Fun2(SetSDK.filter),
          NativeFunctionAdapter.Fun2(SetSDK.insert),
          NativeFunctionAdapter.Fun1(SetSDK.singleton),
          NativeFunctionAdapter.Fun2(SetSDK.union),
          NativeFunctionAdapter.Fun2(SetSDK.intersect),
          NativeFunctionAdapter.Fun1(SetSDK.isEmpty),
          NativeFunctionAdapter.Fun2(SetSDK.map),
          NativeFunctionAdapter.Fun2(SetSDK.partition),
          NativeFunctionAdapter.Fun2(SetSDK.remove)
        )
      }
    }
  }

  val modules: Seq[SdkModuleDescriptor] = {
    import Morphir.SDK._
    Seq(
      Aggregate,
      Basics,
      Char,
      Decimal,
      Dict,
      Key,
      List,
      LocalDate,
      LocalTime,
      Maybe,
      Result,
      Set,
      String
    )
  }

  val resolvedFunctions: Map[FQName, SDKValue] = modules.map(_.resolvedFunctions).reduce(_ ++ _)

  val ctors: Map[FQName, SDKConstructor] = modules.map(_.ctors).reduce(_ ++ _)
}
