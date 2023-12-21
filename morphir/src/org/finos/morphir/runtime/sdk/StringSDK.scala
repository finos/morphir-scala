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
import org.finos.morphir.runtime.RTValue.Primitive.String as RTString
import org.finos.morphir.runtime.RTValue.{coerceList, coerceString}

import scala.collection.StringOps

object StringSDK {

  val append = DynamicNativeFunction2("append") {
    (context: NativeContext) =>
      (str1: RTString, str2: RTString) =>
        val appended = str1.value :++ str2.value
        RTString(appended)
  }

  val concat = DynamicNativeFunction1("concat") {
    (context: NativeContext) =>
      (list: RT.List) =>
        val str = list.value.foldLeft("")((x, y) => x + coerceString(y).value)
        RTString(str)
  }
}
