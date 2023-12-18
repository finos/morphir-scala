package org.finos.morphir.runtime.sdk

import org.finos.morphir.Hints
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.SDKValue
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.MorphirRuntimeError.IllegalValue

object BasicsSDK {
  type AnyNum = Any

  val ceiling = DynamicNativeFunction1("ceiling") {
    (_: NativeContext) => (a: Primitive.Float) =>
      Primitive.Int(a.value.ceil.toInt)
  }

  val floor = DynamicNativeFunction1("floor") {
    (_: NativeContext) => (a: Primitive.Float) =>
      Primitive.Int(a.value.floor.toInt)
  }

  val truncate = DynamicNativeFunction1("truncate") {
    (_: NativeContext) => (a: Primitive.Float) =>
      // truncate rounds negative numbers *towards zero*
      if (a.value < 0) Primitive.Int(a.value.ceil.toInt)
      else Primitive.Int(a.value.floor.toInt)
  }

  val sqrt = DynamicNativeFunction1("sqrt") {
    (_: NativeContext) => (a: Primitive.Float) =>
      Primitive.Float(scala.math.sqrt(a.value))
  }

  val integerDivide = DynamicNativeFunction2("integerDivide") {
    (_: NativeContext) => (a: Primitive.Int, b: Primitive.Int) =>
      Primitive.Int(a.value / b.value)
  }

  val xor = DynamicNativeFunction2("xor") {
    (_: NativeContext) => (a: Primitive.Boolean, b: Primitive.Boolean) =>
      Primitive.Boolean(a.value ^ b.value)
  }

  val always = DynamicNativeFunction2("always") {
    (_: NativeContext) => (a: RTValue, _: RTValue) =>
      a
  }

  val identity = DynamicNativeFunction1("identity") {
    (_: NativeContext) => (a: RTValue) =>
      a
  }

  val abs = NumericFunction1("abs") {
    (numericHelpers: NumericHelpers[AnyNum], _) => (arg1: AnyNum) =>
      {
        val result = numericHelpers.numericHelper.abs(arg1)
        numericHelpers.numericType.makeOrFail(result)
      }
  }

  val clamp = NumericFunction3("clamp") {
    (numericHelpers: NumericHelpers[AnyNum], _) => (min: AnyNum, max: AnyNum, x: AnyNum) =>
      {
        val helper = numericHelpers.numericHelper
        val result =
          if (helper.lt(x, min)) min
          else if (helper.gt(x, min) && helper.lt(x, max)) x
          else max
        numericHelpers.numericType.makeOrFail(result)
      }
  }

  val power = DynamicNativeFunction2("power") {
    (_: NativeContext) => (a: Primitive.Numeric[_], b: Primitive.Numeric[_]) =>
      (a, b) match {
        case (intA: Primitive.Int, intB: Primitive.Int) =>
          Primitive.Int(scala.math.pow(intA.value.toDouble, intB.value.toDouble).toInt)
        case (floatA: Primitive.Float, floatB: Primitive.Float) =>
          Primitive.Float(scala.math.pow(floatA.value, floatB.value))
        case _ => throw IllegalValue(s"The values must be either Int or Float and of the same type.")
      }
  }

  val modBy = NumericFunction2("modBy") {
    (numricHelpers: NumericHelpers[AnyNum], _) =>
      // Elm modBy is reversed: 1st arg is the divisor
      (arg1: AnyNum, arg2: AnyNum) =>
        {
          val output = numricHelpers.integralHelperOrThrow.rem(arg2, arg1)
          numricHelpers.numericType.makeOrFail(output)
        }
  }

  /**
   * Modulo and remainder operators differ with respect to negative values. With a remainder operator, the sign of the
   * result is the same as the sign of the numerator while with a modulo operator the sign of the result is the same as
   * the divisor.
   */
  val remainderBy = NumericFunction2("remainderBy") {
    (numricHelpers: NumericHelpers[AnyNum], _) => (divisor: AnyNum, numerator: AnyNum) =>
      {
        val helper                = numricHelpers.integralHelperOrThrow
        def isNegative(a: AnyNum) = helper.lt(a, helper.zero)
        val result                = helper.rem(numerator, divisor)
        val output =
          if (isNegative(numerator)) {
            if (!isNegative(result)) helper.negate(result) else result
          } else {
            if (isNegative(result)) helper.negate(result) else result
          }
        numricHelpers.numericType.makeOrFail(output)
      }
  }

  val greaterThan = NumericFunction2("greaterThan") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.gt(a, b))
  }

  val greaterThanOrEqual = NumericFunction2("greaterThanOrEqual") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.gteq(a, b))
  }

  val lessThan = NumericFunction2("lessThan") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.lt(a, b))
  }

  val lessThanOrEqual = NumericFunction2("lessThanOrEqual") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.lteq(a, b))
  }
}
