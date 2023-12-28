package org.finos.morphir.runtime.sdk

object DecimalSDK {

  val abs = DynamicNativeFunction("abs") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val add = DynamicNativeFunction("add") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val bps = DynamicNativeFunction("bps") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val compare = DynamicNativeFunction("compare") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val div = DynamicNativeFunction("div") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val divWithDefault = DynamicNativeFunction("divWithDefault") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val eq = DynamicNativeFunction("eq") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val fromInt = DynamicNativeFunction("fromInt") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

  val fromString = DynamicNativeFunction("fromString") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

}
