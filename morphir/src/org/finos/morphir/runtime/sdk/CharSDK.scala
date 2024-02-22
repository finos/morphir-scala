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

  val isLower = DynamicNativeFunction1("isLower") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.isLower
      RT.Primitive.Boolean(result)
  }

  val isAlpha = DynamicNativeFunction1("isAlpha") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.isLetter
      RT.Primitive.Boolean(result)
  }

  val isAlphaNum = DynamicNativeFunction1("isAlphaNum") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.isLetterOrDigit
      RT.Primitive.Boolean(result)
  }

  val isDigit = DynamicNativeFunction1("isDigit") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.isDigit
      RT.Primitive.Boolean(result)
  }

  val isOctDigit = DynamicNativeFunction1("isOctDigit") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = if (Character.digit(ch.value, 8) == -1)
        false
      else
        true
      RT.Primitive.Boolean(result)
  }

  val isHexDigit = DynamicNativeFunction1("isHexDigit") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = if (Character.digit(ch.value, 16) == -1)
        false
      else
        true
      RT.Primitive.Boolean(result)
  }

  val toUpper = DynamicNativeFunction1("toUpper") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.toUpper
      RTChar(result)
  }

  val toLower = DynamicNativeFunction1("toLower") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.toLower
      RTChar(result)
  }

  val toLocaleUpper = DynamicNativeFunction1("toLocaleUpper") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.toString.toUpperCase
      RTChar(result.charAt(0))
  }

  val toLocaleLower = DynamicNativeFunction1("toLocaleLower") {
    (_: NativeContext) => (ch: RTChar) =>
      val result = ch.value.toString.toLowerCase
      RTChar(result.charAt(0))
  }

}
