package org.finos.morphir.runtime.sdk

import org.finos.morphir.datamodel.Schema.Primitive
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  NativeContext
}
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.runtime.RTValue.Primitive.BigDecimal as RTDecimal

object DecimalSDK {

  val abs = DynamicNativeFunction("abs") {
    (_: NativeContext) => (dec: RTDecimal) =>
      val result = dec.value.abs
      RTDecimal(result)
  }

  val add = DynamicNativeFunction("add") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val bps = DynamicNativeFunction("bps") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val compare = DynamicNativeFunction("compare") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val div = DynamicNativeFunction("div") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val divWithDefault = DynamicNativeFunction("divWithDefault") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val eq = DynamicNativeFunction("eq") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val fromInt = DynamicNativeFunction("fromInt") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val fromString = DynamicNativeFunction("fromString") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val gt = DynamicNativeFunction("gt") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val gte = DynamicNativeFunction("gte") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val lt = DynamicNativeFunction("lt") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val lte = DynamicNativeFunction("lte") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val minusOne = DynamicNativeFunction("minusOne") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val mul = DynamicNativeFunction("mul") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val negate = DynamicNativeFunction("negate") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val neq = DynamicNativeFunction("neq") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val one = DynamicNativeFunction("one") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val round = DynamicNativeFunction("round") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val sub = DynamicNativeFunction("sub") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val truncate = DynamicNativeFunction("truncate") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  val zero = DynamicNativeFunction("zero") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

}
