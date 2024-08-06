package org.finos.morphir.runtime.sdk

import org.finos.morphir.{MInt, MValue}
import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.MorphirRuntimeError.{ExternalError, FailedCoercion, IllegalValue}
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.RTValue.Comparable.orderToInt
import org.finos.morphir.runtime.RTValue.{
  Primitive,
  coerceBoolean,
  coerceComparable,
  coerceDecimal,
  coerceFloat,
  coerceInt,
  coerceKey,
  coerceList,
  coerceNumeric,
  coerceTuple,
  unwrapNumericWithHelper
}

import scala.collection.mutable

object AggregateSDK {

  val groupBy = DynamicNativeFunction2("groupBy") {
    (context: NativeContext) => (f: RT.Function, list: RT.List) =>
      val result = list.value groupBy { a =>
        context.evaluator.handleApplyResult(Type.UType.Variable("a"), f, a)
      }
      val r = result.view.mapValues(l => RT.List(l))
      RT.Map(mutable.LinkedHashMap.from(r))
  }

  private object AggregateMapHelper {
    def apply(list: RT.List): List[AggregateMapHelper] =
      list.value.map(AggregateMapHelper(_, Nil))
  }

  private case class AggregateMapHelper(originalValue: RTValue, opResults: List[RT.Primitive.Float])

  private def mapAndFilter(
      agg: RT.Aggregation,
      list: List[AggregateMapHelper]
  )(ctx: NativeContext): List[AggregateMapHelper] = {
    val filtered: List[AggregateMapHelper] = list filter { case AggregateMapHelper(a, _) =>
      agg.filter(a).value
    }
    val grouped = filtered groupBy { case AggregateMapHelper(a, _) =>
      val aggResult = ctx.evaluator.handleApplyResult(Type.UType.Unit(()), agg.key, a)
      coerceKey(aggResult)
    }
    val result = grouped flatMap { case (_, aggHelpers) =>
      val input = agg.operation(RT.List(aggHelpers.map(_.originalValue)))
      aggHelpers map { a =>
        a.copy(opResults = a.opResults :+ input)
      }
    }
    result.toList
  }

  val aggregateMap = DynamicNativeFunction3("aggregateMap") {
    (context: NativeContext) => (agg: RT.Aggregation, f: RT.Function, list: RT.List) =>
      val aggResult = mapAndFilter(agg, AggregateMapHelper(list))(context)
      val result = aggResult map {
        case AggregateMapHelper(a, List(aggValue)) =>
          context.evaluator.handleApplyResult2(Type.UType.Variable("a"), f, aggValue, a)
        case helper =>
          throw MorphirRuntimeError.OtherError("aggregateMap had an unexpected number of aggregated values", helper)
      }
      RT.List(result)
  }

  val aggregateMap2 = DynamicNativeFunction4("aggregateMap2") {
    (context: NativeContext) => (agg1: RT.Aggregation, agg2: RT.Aggregation, f: RT.Function, list: RT.List) =>
      val agg1result = mapAndFilter(agg1, AggregateMapHelper(list))(context)
      val agg2result = mapAndFilter(agg2, agg1result)(context)
      val result = agg2result map {
        case AggregateMapHelper(a, List(agg1Value, agg2Value)) =>
          context.evaluator.handleApplyResult3(Type.UType.Variable("a"), f, agg1Value, agg2Value, a)
        case helper =>
          throw MorphirRuntimeError.OtherError("aggregateMap2 had an unexpected number of aggregated values", helper)
      }
      RT.List(result)
  }

  val aggregateMap3 = DynamicNativeFunction5("aggregateMap3") {
    (context: NativeContext) =>
      (agg1: RT.Aggregation, agg2: RT.Aggregation, agg3: RT.Aggregation, f: RT.Function, list: RT.List) =>
        val agg1result = mapAndFilter(agg1, AggregateMapHelper(list))(context)
        val agg2result = mapAndFilter(agg2, agg1result)(context)
        val agg3result = mapAndFilter(agg3, agg2result)(context)
        val result = agg3result map {
          case AggregateMapHelper(a, List(agg1Value, agg2Value, agg3Value)) =>
            context.evaluator.handleApplyResult4(Type.UType.Variable("a"), f, agg1Value, agg2Value, agg3Value, a)
          case helper =>
            throw MorphirRuntimeError.OtherError("aggregateMap3 had an unexpected number of aggregated values", helper)
        }
        RT.List(result)
  }

  val aggregateMap4 = DynamicNativeFunction6("aggregateMap4") {
    (context: NativeContext) => (
        agg1: RT.Aggregation,
        agg2: RT.Aggregation,
        agg3: RT.Aggregation,
        agg4: RT.Aggregation,
        f: RT.Function,
        list: RT.List
    ) =>
      val agg1result = mapAndFilter(agg1, AggregateMapHelper(list))(context)
      val agg2result = mapAndFilter(agg2, agg1result)(context)
      val agg3result = mapAndFilter(agg3, agg2result)(context)
      val agg4result = mapAndFilter(agg4, agg3result)(context)
      val result = agg4result map {
        case AggregateMapHelper(a, List(agg1Value, agg2Value, agg3Value, agg4Value)) =>
          context.evaluator.handleApplyResult5(
            Type.UType.Variable("a"),
            f,
            agg1Value,
            agg2Value,
            agg3Value,
            agg4Value,
            a
          )
        case helper =>
          throw MorphirRuntimeError.OtherError("aggregateMap4 had an unexpected number of aggregated values", helper)
      }
      RT.List(result)
  }

  // Operations

  val count: SDKValue = SDKValue.SDKNativeValue(RT.Aggregation(l => RT.Primitive.Float(l.value.size.toDouble)))

  val sumOf = DynamicNativeFunction1("sumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation {
        list =>
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.sum)
      }
  }

  val minimumOf = DynamicNativeFunction1("minimumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation {
        list =>
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.min)
      }
  }

  val maximumOf = DynamicNativeFunction1("maximumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation {
        list =>
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.max)
      }
  }

  val averageOf = DynamicNativeFunction1("averageOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation {
        list =>
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.sum / results.size)
      }
  }

  val weightedAverageOf = DynamicNativeFunction2("weightedAverageOf") {
    (context: NativeContext) => (aToFloat: RT.Function, aToWeight: RT.Function) =>
      RT.Aggregation {
        list =>
          val results: List[Double] = list.value map { a =>
            val aggResult      = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            val weightedResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToWeight, a)
            coerceFloat(aggResult).value * coerceFloat(weightedResult).value
          }
          RT.Primitive.Float(results.sum / results.size)
      }
  }

  val byKey = DynamicNativeFunction2[RT.Function, RT.Aggregation, RT.Aggregation]("byKey") {
    (context: NativeContext) => (key: RT.Function, aggregation: RT.Aggregation) =>
      aggregation.copy(key = key)
  }

  val withFilter = DynamicNativeFunction2("withFilter") {
    (context: NativeContext) => (filterFunc: RT.Function, aggregation: RT.Aggregation) =>
      aggregation.copy(filter = a => {
        val result = context.evaluator.handleApplyResult(Type.UType.Unit(()), filterFunc, a)
        coerceBoolean(result)
      })
  }

}
