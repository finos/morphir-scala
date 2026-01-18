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
import org.finos.morphir.ir.Type
import scala.collection.StringOps

object StringSDK {

  val append = DynamicNativeFunction2("append") {
    (context: NativeContext) => (str1: RTString, str2: RTString) =>
      val appended = str1.value :++ str2.value
      RTString(appended)
  }

  val concat = DynamicNativeFunction1("concat") {
    (context: NativeContext) => (list: RT.List) =>
      val str = list.value.foldLeft("")((x, y) => x + coerceString(y).value)
      RTString(str)
  }

  val contains = DynamicNativeFunction2("contains") {
    (context: NativeContext) => (substring: RTString, str: RTString) =>
      val result = str.value.contains(substring.value)
      RT.Primitive.Boolean(result)
  }

  val dropLeft = DynamicNativeFunction2("dropLeft") {
    (context: NativeContext) => (int: RT.Primitive.Int, str: RTString) =>
      val result = str.value.drop(int.valueAsInt)
      RTString(result)
  }

  val repeat = DynamicNativeFunction2("repeat") {
    (context: NativeContext) => (int: RT.Primitive.Int, str: RTString) =>
      val result = str.value.repeat(int.valueAsInt)
      RTString(result)
  }

  val dropRight = DynamicNativeFunction2("dropRight") {
    (context: NativeContext) => (int: RT.Primitive.Int, str: RTString) =>
      val result = str.value.dropRight(int.valueAsInt)
      RTString(result)
  }

  val endsWith = DynamicNativeFunction2("endsWith") {
    (context: NativeContext) => (ref: RTString, str: RTString) =>
      val result = str.value.endsWith(ref.value)
      RT.Primitive.Boolean(result)
  }

  val join = DynamicNativeFunction2("join") {
    (context: NativeContext) => (sep: RTString, list: RT.List) =>
      val result = list.elements.map(s => coerceString(s).value).mkString(sep.value)
      RTString(result)
  }

  val length = DynamicNativeFunction1("length") {
    (context: NativeContext) => (str: RTString) =>
      val length = str.value.length
      RT.Primitive.Int(length)
  }

  val padLeft = DynamicNativeFunction3("padLeft") {
    (context: NativeContext) => (n: RT.Primitive.Int, ch: RT.Primitive.Char, str: RTString) =>
      val result = (ch.value.toString * n.valueAsInt) ++ str.value
      RTString(result)
  }

  val padRight = DynamicNativeFunction3("padRight") {
    (context: NativeContext) => (n: RT.Primitive.Int, ch: RT.Primitive.Char, str: RTString) =>
      val result = str.value ++ (ch.value.toString * n.valueAsInt)
      RTString(result)
  }

  val right = DynamicNativeFunction2("right") {
    (context: NativeContext) => (n: RT.Primitive.Int, str: RTString) =>
      val result = str.value.substring(str.value.length - n.valueAsInt)
      RTString(result)
  }

  /**
   * This doesn't support negative indexes that would be supported in Morphir elm.
   */
  val slice = DynamicNativeFunction3("slice") {
    (context: NativeContext) => (start: RT.Primitive.Int, end: RT.Primitive.Int, str: RTString) =>
      val result = str.value.slice(start.valueAsInt, end.valueAsInt)
      RTString(result)
  }

  /**
   * This implementation follows the morphir-jvm implementation and is different from the Morphir elm implementation.
   * The following are examples of what to expect: split("o", "foo")
   *   - java: ["f"]
   *   - elm: ["f", "o", "o"]
   *
   * split("o{", foo")
   *   - java: throws Exception
   *   - elm: ["foo"]
   */
  val split = DynamicNativeFunction2("split") {
    (context: NativeContext) => (sep: RTString, str: RTString) =>
      val result = str.value.split(java.util.regex.Pattern.quote(sep.value)).toList.map(RTString(_))
      RT.List(result)
  }

  val startsWith = DynamicNativeFunction2("startsWith") {
    (context: NativeContext) => (ref: RTString, str: RTString) =>
      val result = str.value.startsWith(ref.value)
      RT.Primitive.Boolean(result)
  }

  val toFloat = DynamicNativeFunction1("toFloat") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.toFloatOption.flatMap(RT.Primitive.make(_))
      MaybeSDK.optionToMaybe(result)
  }

  val toLower = DynamicNativeFunction1("toLower") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.toLowerCase
      RTString(result)
  }

  val toUpper = DynamicNativeFunction1("toUpper") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.toUpperCase
      RTString(result)
  }

  val trim = DynamicNativeFunction1("trim") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.strip()
      RTString(result)
  }

  val trimLeft = DynamicNativeFunction1("trimLeft") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.stripLeading()
      RTString(result)
  }

  val trimRight = DynamicNativeFunction1("trimRight") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.stripTrailing()
      RTString(result)
  }

  val fromChar = DynamicNativeFunction1("fromChar") {
    (context: NativeContext) => (char: RT.Primitive.Char) =>
      RTString(char.value.toString)
  }

  val cons = DynamicNativeFunction2("cons") {
    (context: NativeContext) => (char: RT.Primitive.Char, str: RTString) =>
      RTString(char.value.toString + str.value)
  }

  val uncons = DynamicNativeFunction1("uncons") {
    (context: NativeContext) => (str: RTString) =>
      str.value match {
        case "" => MaybeSDK.optionToMaybe(None)
        case _  =>
          MaybeSDK.optionToMaybe(Some(RT.Tuple(RT.Primitive.Char(str.value.head), RTString(str.value.tail))))
      }
  }

  val toList = DynamicNativeFunction1("toList") {
    (context: NativeContext) => (str: RTString) =>
      val result = str.value.toList.map(c => RT.Primitive.Char(c))
      RT.List(result)
  }

  val fromList = DynamicNativeFunction1("fromList") {
    (context: NativeContext) => (list: RT.List) =>
      val result = list.elements.map(c => RTValue.coerceChar(c).value).mkString
      RTString(result)
  }

  val pad = DynamicNativeFunction3("pad") {
    (context: NativeContext) => (n: RT.Primitive.Int, ch: RT.Primitive.Char, str: RTString) =>
      val totalExtra                  = n.valueAsInt - str.value.length
      val half                        = totalExtra / 2.0
      val (leftPadding, rightPadding) = (
        ch.value.toString * Math.ceil(half).toInt,
        ch.value.toString * Math.floor(half).toInt
      )
      val result = leftPadding ++ str.value ++ rightPadding
      RTString(result)
  }

  val map = DynamicNativeFunction2("map") {
    (context: NativeContext) => (f: RTValue.Function, str: RTString) =>
      val result = str.value.map { c =>
        val res = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, RT.Primitive.Char(c))
        RTValue.coerceChar(res).value
      }
      RTString(result)
  }

  val filter = DynamicNativeFunction2("filter") {
    (context: NativeContext) => (f: RTValue.Function, str: RTString) =>
      val result = str.value.filter { c =>
        val res = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, RT.Primitive.Char(c))
        RTValue.coerceBoolean(res).value
      }
      RTString(result)
  }

  val foldl = DynamicNativeFunction3("foldl") {
    (context: NativeContext) => (f: RTValue.Function, acc: RTValue, str: RTString) =>
      val result = str.value.foldLeft(acc) { (acc, c) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, RT.Primitive.Char(c), acc)
      }
      result
  }

  val foldr = DynamicNativeFunction3("foldr") {
    (context: NativeContext) => (f: RTValue.Function, acc: RTValue, str: RTString) =>
      val result = str.value.foldRight(acc) { (c, acc) =>
        context.evaluator.handleApplyResult2(Type.UType.Unit(()), f, RT.Primitive.Char(c), acc)
      }
      result
  }

  val any = DynamicNativeFunction2("any") {
    (context: NativeContext) => (f: RTValue.Function, str: RTString) =>
      val result = str.value.exists { c =>
        val res = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, RT.Primitive.Char(c))
        RTValue.coerceBoolean(res).value
      }
      RT.Primitive.Boolean(result)
  }

  val all = DynamicNativeFunction2("all") {
    (context: NativeContext) => (f: RTValue.Function, str: RTString) =>
      val result = str.value.forall { c =>
        val res = context.evaluator.handleApplyResult(Type.UType.Unit(()), f, RT.Primitive.Char(c))
        RTValue.coerceBoolean(res).value
      }
      RT.Primitive.Boolean(result)
  }
}
