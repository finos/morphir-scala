package org.finos.morphir.runtime.sdk

object DecimalSDK {

  val abs = DynamicNativeFunction("abs") {
    (_: NativeContext) => () =>
      Decimal(result)
  }

}
