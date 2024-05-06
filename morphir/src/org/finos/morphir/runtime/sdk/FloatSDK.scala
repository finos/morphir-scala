package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.ErrorUtils.tryOption
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.internal.{DynamicNativeFunction1, DynamicNativeFunction2, NativeContext}
import org.finos.morphir.runtime.{SDKValue, RTValue as RT}
import org.finos.morphir.runtime.RTValue.Primitive.Float as RTFloat
import org.finos.morphir.runtime.RTValue.Primitive.Int

object FloatSDK {

  val fromInt = DynamicNativeFunction1("fromInt") {
    (_: NativeContext) => (a: RT.Primitive.Int) =>
      RTFloat(a.value.toDouble)
  } 

  val round = DynamicNativeFunction1("round") {
    (_: NativeContext) => (a: RTFloat) =>
      Primitive.Int(a.value.round)
  }

  val floor = DynamicNativeFunction1("floor") {
    (_: NativeContext) => (a: RTFloat) =>
      Primitive.Int(a.value.floor.toInt)
  }

  val ceiling = DynamicNativeFunction1("ceiling") {
    (_: NativeContext) => (a: RTFloat) =>
      Primitive.Int(a.value.ceil.toInt)
  }

  val truncate = DynamicNativeFunction1("truncate") {
    (_: NativeContext) => (a: RTFloat) =>
      if (a.value < 0) Primitive.Int(a.value.ceil.toInt)
      else Primitive.Int(a.value.floor.toInt)
  }
}
