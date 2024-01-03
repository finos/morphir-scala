package org.finos.morphir.runtime.sdk

import org.finos.morphir.ir.Type
import org.finos.morphir.Hints
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.SDKValue
import org.finos.morphir.runtime.internal._
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.MorphirRuntimeError.IllegalValue


object BasicsSDK {
  type AnyNum = Any

  def comparableHelper(a : RTValue, b : RTValue) : Int = {
    (a, b) match {
      case (Primitive.Int(a), Primitive.Int(b)) => a.value.compareTo(b.value)
      case (Primitive.Float(a), Primitive.Float(b)) => a.compareTo(b)
      case (Primitive.String(a), Primitive.String(b)) => a.compareTo(b)
      case (Primitive.Char(a), Primitive.Char(b)) => a.compareTo(b)
      case (RTValue.List(l1), RTValue.List(l2)) => l1.zip(l2).map{case (elem_1, elem_2) => comparableHelper (elem_1, elem_2)}.find(_ != 0).getOrElse(l1.length.compareTo(l2.length))
      case (RTValue.Tuple(a), RTValue.Tuple(b)) => 
        if (a.length == b.length) a.zip(b).map{case (elem_1, elem_2) => comparableHelper(elem_1, elem_2)}.find(_ != 0).getOrElse(0)
        else throw IllegalValue(s"Cannot compare tuples of different lengths: $a and $b")
      case _ => throw IllegalValue(s"Cannot compare values of different types: $a and $b")
    }
  }
  def intToToOrder(i : Int) : RTValue = {
    i match {
      case 0 => RTValue.Order.EQ
      case 1 => RTValue.Order.GT
      case -1 => RTValue.Order.LT
      case _ => throw IllegalValue(s"Cannot convert $i to Order")
    }
  }
  
  val compare = DynamicNativeFunction2("compare") {
    (_: NativeContext) => (a: RTValue, b: RTValue) =>
      intToToOrder(comparableHelper(a, b))
  }


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
      if (b.valueAsInt == 0) Primitive.Int(0)
      else Primitive.Int(a.value / b.value)
  }

  val xor = DynamicNativeFunction2("xor") {
    (_: NativeContext) => (a: Primitive.Boolean, b: Primitive.Boolean) =>
      Primitive.Boolean(a.value ^ b.value)
  }

  /**
   * Note: While the Elm docs specify this fn as a -> b -> a, with partial application giving b -> a, implementing it as
   * a simple identity, ignoring the second param, works.
   */
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
  }.asNative1

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
  }.asNative3

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
  }.asNative2

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
  }.asNative2

  val greaterThan = NumericFunction2("greaterThan") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.gt(a, b))
  }.asNative2

  val greaterThanOrEqual = NumericFunction2("greaterThanOrEqual") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.gteq(a, b))
  }.asNative2

  val lessThan = NumericFunction2("lessThan") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.lt(a, b))
  }.asNative2

  val lessThanOrEqual = NumericFunction2("lessThanOrEqual") {
    (h: NumericHelpers[Any], _) => (a: AnyNum, b: AnyNum) =>
      Primitive.Boolean(h.numericHelper.lteq(a, b))
  }.asNative2

  val composeRight = DynamicNativeFunction3("composeRight") {
    (ctx: NativeContext) => (f1: RTValue.Function, f2: RTValue.Function, arg: RTValue) =>
      {
        val res1 = ctx.evaluator.handleApplyResult(Type.UType.Unit(()), f1, arg)
        ctx.evaluator.handleApplyResult(Type.UType.Unit(()), f2, res1)
      }
  }
}
