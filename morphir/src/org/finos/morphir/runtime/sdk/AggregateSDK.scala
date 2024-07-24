package org.finos.morphir.runtime.sdk

import org.finos.morphir.{MInt, MValue}
import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.MorphirRuntimeError.{FailedCoercion, IllegalValue}
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

}
