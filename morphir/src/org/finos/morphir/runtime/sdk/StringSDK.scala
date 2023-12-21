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

  val contains = DynamicNativeFunction2("contains") {
    (context: NativeContext) =>
      (substring: RTString, str: RTString) =>
        val result = str.value.contains(substring.value)
        RT.Primitive.Boolean(result)
  }

  val dropLeft = DynamicNativeFunction2("dropLeft") {
    (context: NativeContext) =>
      (int: RT.Primitive.Int, str: RTString) =>
        val result = str.value.drop(int.valueAsInt)
        RTString(result)
  }

  val dropRight = DynamicNativeFunction2("dropRight") {
    (context: NativeContext) =>
      (int: RT.Primitive.Int, str: RTString) =>
        val result = str.value.dropRight(int.valueAsInt)
        RTString(result)
  }

  val endsWith = DynamicNativeFunction2("endsWith") {
    (context: NativeContext) =>
      (ref: RTString, str: RTString) =>
        val result = str.value.endsWith(ref.value)
        RT.Primitive.Boolean(result)
  }

  val join = DynamicNativeFunction2("join") {
    (context: NativeContext) =>
      (sep: RTString, list: RT.List) =>
        val result = list.elements.zipWithIndex.foldLeft("") { (x, y) =>
          val withSep =
            if (y._2 < list.elements.length - 1) coerceString(y._1).value ++ sep.value else coerceString(y._1).value
          x ++ withSep
        }
        RTString(result)
  }

  val length = DynamicNativeFunction1("length") {
    (context: NativeContext) =>
      (str: RTString) =>
        val length = str.value.length
        RT.Primitive.Int(length)
  }

  val padLeft = DynamicNativeFunction3("padLeft") {
    (context: NativeContext) =>
      (n: RT.Primitive.Int, ch: RT.Primitive.Char, str: RTString) =>
        val result = (ch.value.toString * n.valueAsInt) ++ str.value
        RTString(result)
  }

  val padRight = DynamicNativeFunction3("padRight") {
    (context: NativeContext) =>
      (n: RT.Primitive.Int, ch: RT.Primitive.Char, str: RTString) =>
        val result = str.value ++ (ch.value.toString * n.valueAsInt)
        RTString(result)
  }
}
