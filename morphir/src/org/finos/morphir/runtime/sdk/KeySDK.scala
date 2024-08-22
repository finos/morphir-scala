package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.runtime.MorphirRuntimeError.{FailedCoercion, IllegalValue}
import org.finos.morphir.runtime.{RTValue as RT, *}
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
import org.finos.morphir.runtime.internal.*
import org.finos.morphir.{MInt, MValue}

import scala.collection.mutable

object KeySDK {

  val key0 = DynamicNativeFunction1("key0") {
    (context: NativeContext) => (a: RT) =>
      RT.Key0
  }

}
