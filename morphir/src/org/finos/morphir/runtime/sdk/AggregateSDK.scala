package org.finos.morphir.runtime.sdk

import org.finos.morphir.{MInt, MValue}
import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.MorphirRuntimeError.{FailedCoercion, IllegalValue}
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.runtime.{RTValue => RT}
import org.finos.morphir.runtime.RTValue.Comparable.orderToInt
import org.finos.morphir.runtime.RTValue.{Primitive, coerceComparable, coerceDecimal, coerceFloat, coerceInt, coerceNumeric, coerceTuple, unwrapNumericWithHelper}

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

  val aggregateMap = DynamicNativeFunction3("aggregateMap") {
    (context: NativeContext) => (agg: RT.Aggregation, f: RT.Function, list: RT.List) =>
      val result = list.value map { a =>
        val aggResult = context.evaluator.handleApplyResult(Type.UType.Variable("a"), agg.key, a)
        val input = coerceFloat(aggResult)
        val output = context.evaluator.handleApplyResult2(Type.UType.Variable("a"), f, input, a)
        RT.Tuple(a, output)
      }
      RT.List(result)
  }
  
  //Operations

  val count: SDKValue = SDKValue.SDKNativeValue(RT.Aggregation(l => RT.Primitive.Float(l.value.size.toDouble)))

  val sumOf = DynamicNativeFunction1("sumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation(
        list => {
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.sum)
        }
      )
  }

  val minimumOf = DynamicNativeFunction1("minimumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation(
        list => {
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.min)
        }
      )
  }

  val maximumOf = DynamicNativeFunction1("maximumOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation(
        list => {
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.max)
        }
      )
  }

  val averageOf = DynamicNativeFunction1("averageOf") {
    (context: NativeContext) => (aToFloat: RT.Function) =>
      RT.Aggregation(
        list => {
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            coerceFloat(aggResult).value
          }
          RT.Primitive.Float(results.sum / results.size)
        }
      )
  }

  val weightedAverageOf = DynamicNativeFunction2("weightedAverageOf") {
    (context: NativeContext) => (aToFloat: RT.Function, aToWeight: RT.Function) =>
      RT.Aggregation(
        list => {
          val results: List[Double] = list.value map { a =>
            val aggResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToFloat, a)
            val weightedResult = context.evaluator.handleApplyResult(Type.UType.Unit(()), aToWeight, a)
            coerceFloat(aggResult).value * coerceFloat(weightedResult).value
          }
          RT.Primitive.Float(results.sum / results.size)
        }
      )
  }

  val byKey = DynamicNativeFunction2("byKey") {
    (context: NativeContext) => (key: RT.Key, aggregation: RT.Aggregation) =>
      aggregation.copy(key = key)
  }

  val withFilter = DynamicNativeFunction2("withFilter") {
    (context: NativeContext) => (filterFunc: RT.Function, aggregation: RT.Aggregation) =>
      aggregation.copy(filter = a: RTValue => {
        context.evaluator.handleApplyResult(Type.UType.Unit(()), filterFunc, a)
      })
  }

}
