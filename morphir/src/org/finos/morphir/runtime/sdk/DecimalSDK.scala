package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.ErrorUtils.tryOption
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  NativeContext
}
import org.finos.morphir.runtime.{SDKValue, RTValue as RT}
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

  // Converts an int to a Decimal that represents n basis points (i.e. 1/10 of % or a ten-thousandth)
  val bps = DynamicNativeFunction1("bps") {
    (_: NativeContext) => (int: RT.Primitive.Int) =>
      val result = int.value.toBigDecimal * 0.0001
      RTDecimal(result)
  }

  val compare = DynamicNativeFunction2("compare") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value.compare(dec2.value)
      RT.Comparable.intToOrder(result)
  }

  val div = DynamicNativeFunction2("div") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = tryOption(dec1.value / dec2.value).map(RTDecimal(_))
      MaybeSDK.optionToMaybe(result)
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
      val result = dec1.value.equals(dec2.value)
      RT.Primitive.Boolean(result)
  }

  val fromInt = DynamicNativeFunction1("fromInt") {
    (_: NativeContext) => (int: RT.Primitive.Int) =>
      val result = int.value.toBigDecimal
      RTDecimal(result)
  }
    val fromFloat = DynamicNativeFunction1("fromFloat") {
    (_: NativeContext) =>
      (float: RT.Primitive.Float) =>
        val result = BigDecimal(float.value)
        RTDecimal(result)
  }


  val fromString = DynamicNativeFunction1("fromString") {
    (_: NativeContext) => (str: RT.Primitive.String) =>
      val result = tryOption(BigDecimal(str.value)).map(RTDecimal(_))
      MaybeSDK.optionToMaybe(result)
  }

  val gt = DynamicNativeFunction2("gt") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value > dec2.value
      RT.Primitive.Boolean(result)
  }

  val gte = DynamicNativeFunction2("gte") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value >= dec2.value
      RT.Primitive.Boolean(result)
  }

  val lt = DynamicNativeFunction2("lt") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value < dec2.value
      RT.Primitive.Boolean(result)
  }

  val lte = DynamicNativeFunction2("lte") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value <= dec2.value
      RT.Primitive.Boolean(result)
  }

  val minusOne: SDKValue = SDKValue.SDKNativeValue(RTDecimal(-1))

  val mul = DynamicNativeFunction2("mul") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value * dec2.value
      RTDecimal(result)
  }

  val negate = DynamicNativeFunction1("negate") {
    (_: NativeContext) => (value: RTDecimal) =>
      RTDecimal(value.value.unary_-)
  }

  val neq = DynamicNativeFunction2("neq") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value != dec2.value
      RT.Primitive.Boolean(result)
  }

  val one: SDKValue = SDKValue.SDKNativeValue(RTDecimal(1))

  val round = DynamicNativeFunction1("round") {
    (_: NativeContext) => (value: RTDecimal) =>
      val dec    = value.value
      val result = dec.setScale(0, BigDecimal.RoundingMode.HALF_UP)
      RTDecimal(result)
  }

  val sub = DynamicNativeFunction2("sub") {
    (_: NativeContext) => (dec1: RTDecimal, dec2: RTDecimal) =>
      val result = dec1.value - dec2.value
      RTDecimal(result)
  }

  val truncate = DynamicNativeFunction1("truncate") {
    (_: NativeContext) => (value: RTDecimal) =>
      val d      = value.value
      val result = d.setScale(0, BigDecimal.RoundingMode.DOWN)
      RTDecimal(result)
  }

  val zero: SDKValue = SDKValue.SDKNativeValue(RTDecimal(0))

  val shiftDecimalLeft = DynamicNativeFunction2("shiftDecimalLeft") {
    (_: NativeContext) => (shift: RT.Primitive.Int, dec: RTDecimal) =>
      val result = dec.value / BigDecimal(10).pow(shift.valueAsInt)
      RTDecimal(result)
  }

  val shiftDecimalRight = DynamicNativeFunction2("shiftDecimalRight") {
    (_: NativeContext) => (shift: RT.Primitive.Int, dec: RTDecimal) =>
      val result = dec.value * BigDecimal(10).pow(shift.valueAsInt)
      RTDecimal(result)
  }
}
