package org.finos.morphir.runtime.sdk

import org.finos.morphir.Hints
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.MorphirRuntimeError.IllegalValue

object BasicsSDK {
  type AnyNum = Any

  val ceiling = DynamicNativeFunction1("ceiling") {
    (_: NativeContext) =>
      (a: Primitive.Float) =>
        Primitive.Int(a.value.ceil.toInt)
  }

  val modBy = NumericFunction2("modBy") {
    (numricHelpers: NumericHelpers[AnyNum], _) =>
      // ELM modBy is reversed, 1st arg is the divisor
      (arg1: AnyNum, arg2: AnyNum) =>
        {
          val output = numricHelpers.integralHelperOrThrow.rem(arg2, arg1)
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
