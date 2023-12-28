package org.finos.morphir.runtime.sdk

import org.finos.morphir.datamodel.Schema.Primitive
import org.finos.morphir.runtime.ErrorUtils.tryOption
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

  val abs = DynamicNativeFunction1("abs") {
    (_: NativeContext) => (dec: RTDecimal) =>
      val result = dec.value.abs
      RTDecimal(result)
  }

  val add = DynamicNativeFunction2("add") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value + dec2.value
      RTDecimal(result)
  }

  val bps = DynamicNativeFunction1("bps") {
    (_: NativeContext) => () =>
      RTDecimal(result)
  }

  // todo
  val compare = DynamicNativeFunction2("compare") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value.compare(dec2.value)
      RTDecimal(result)
  }

  val div = DynamicNativeFunction2("div") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = tryOption(dec1.value / dec2.value).map(RTDecimal(_))
      MaybeSDK.resultToMaybe(result)
  }

  val divWithDefault = DynamicNativeFunction3("divWithDefault") {
    (_: NativeContext) => (default: RTDecimal, dec1: RTDecimal, dec2: RTDecimal) =>
      tryOption(dec1.value / dec2.value) match {
        case Some(value) => RTDecimal(value)
        case None        => default
      }
  }

  val eq = DynamicNativeFunction2("eq") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value.eq(dec2.value)
      RT.Primitive.Boolean(result)
  }

  val fromInt = DynamicNativeFunction1("fromInt") {
    (_: NativeContext) => (int: RT.Primitive.Int) =>
      val result = int.value.toBigDecimal
      RTDecimal(result)
  }

  val fromString = DynamicNativeFunction1("fromString") {
    (_: NativeContext) => (str: RT.Primitive.String) =>
      val result = BigDecimal(str.value)
      RTDecimal(result)
  }

  val gt = DynamicNativeFunction("gt") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value > dec2.value
      RT.Primitive.Boolean(result)
  }

  val gte = DynamicNativeFunction("gte") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value >= dec2.value
      RT.Primitive.Boolean(result)
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
