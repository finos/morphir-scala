package org.finos.morphir.runtime.sdk

import org.finos.morphir.runtime.RTValue.Primitive.Char as RTChar
import org.finos.morphir.runtime.internal.{DynamicNativeFunction1, DynamicNativeFunction2, NativeContext}
import org.finos.morphir.runtime.{SDKValue, RTValue as RT}

object CharSDK {

  val isUpper = DynamicNativeFunction1("isUpper") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.isUpper
      RT.Primitive.Boolean(result)
  }

}
