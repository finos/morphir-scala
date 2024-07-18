package org.finos.morphir.runtime.sdk

import org.finos.morphir.{MInt, MValue}
import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.MorphirRuntimeError.{FailedCoercion, IllegalValue}
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.RTValue.Comparable.orderToInt
import org.finos.morphir.runtime.RTValue.{
  Primitive,
  coerceComparable,
  coerceDecimal,
  coerceFloat,
  coerceInt,
  coerceNumeric,
  coerceTuple,
  unwrapNumericWithHelper
}

object AggregateSDK {

  val concat = DynamicNativeFunction1("concat") {
    (context: NativeContext) => (list: RTValue.List) =>
      val flattened = list.elements.flatMap(inner => RTValue.coerceList(inner).elements)
      RTValue.List(flattened)
  }

}
