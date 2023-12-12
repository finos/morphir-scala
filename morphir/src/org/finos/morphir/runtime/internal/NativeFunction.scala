package org.finos.morphir.runtime.internal

import org.finos.morphir.Hints
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.MorphirRuntimeError.IllegalValue

case class NativeContext(evaluator: InvokeableEvaluator, hints: Hints = Hints.empty)

sealed trait DynamicNativeFunction {
  def name: String
}

// format: off
class DynamicNativeFunction1[T1 <: RTValue, R <: RTValue](val name: String)(
    val f: (NativeContext) => (T1) => R
) extends DynamicNativeFunction

object DynamicNativeFunction1 {
  def apply[T1 <: RTValue, R <: RTValue](name: String)(
    f: (NativeContext) => (T1) => R
  ) = new DynamicNativeFunction1[T1, R](name)(f)
}

class DynamicNativeFunction2[T1 <: RTValue, T2 <: RTValue, R <: RTValue](val name: String)(
  val f: (NativeContext) => (T1, T2) => R
) extends DynamicNativeFunction

object DynamicNativeFunction2 {
  def apply[T1 <: RTValue, T2 <: RTValue, R <: RTValue](name: String)(
    f: (NativeContext) => (T1, T2) => R
  ) = new DynamicNativeFunction2[T1, T2, R](name)(f)
}

class DynamicNativeFunction3[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, R <: RTValue](val name: String)(
    val f: (NativeContext) => (T1, T2, T3) => R
) extends DynamicNativeFunction

object DynamicNativeFunction3 {
  def apply[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, R <: RTValue](name: String)(
      f: (NativeContext) => (T1, T2, T3) => R
  ) = new DynamicNativeFunction3[T1, T2, T3, R](name)(f)
}

class DynamicNativeFunction4[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, R <: RTValue](val name: String)(
    val f: (NativeContext) => (T1, T2, T3, T4) => R
) extends DynamicNativeFunction

object DynamicNativeFunction4 {
  def apply[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, R <: RTValue](name: String)(
      f: (NativeContext) => (T1, T2, T3, T4) => R
  ) = new DynamicNativeFunction4[T1, T2, T3, T4, R](name)(f)
}

class DynamicNativeFunction5[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, T5 <: RTValue, R <: RTValue](val name: String)(
  val f: (NativeContext) => (T1, T2, T3, T4, T5) => R
) extends DynamicNativeFunction

object DynamicNativeFunction5 {
  def apply[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, T5 <: RTValue, R <: RTValue](name: String)(
    f: (NativeContext) => (T1, T2, T3, T4, T5) => R
  ) = new DynamicNativeFunction5[T1, T2, T3, T4, T5, R](name)(f)
}
// format: on

case class NumericHelpers[T](
    numericType: RTValue.Primitive.Numeric.Type[T],
    numericHelper: scala.Numeric[T],
    integralHelper: Option[scala.Integral[T]],
    fractionalHelper: Option[scala.Fractional[T]]
) {
  def integralHelperOrThrow: scala.Integral[T] =
    integralHelper.getOrElse(
      throw IllegalValue(s"The value is not whole-number numeric (has no scala.math.Integral helper.")
    )

  def fractionalHelperOrThrow(args: List[RTValue]): scala.Fractional[T] =
    fractionalHelper.getOrElse(
      throw IllegalValue(s"The value(s) ${args} is not fractional numeric (has no scala.math.Fractional helper.")
    )
}

class NumericFunction1[
    T <: RTValue.Primitive.Numeric[Any],
    R <: RTValue
](val name: String)(val f: (NumericHelpers[Any], NativeContext) => (T) => R) {
  def asNative1 =
    DynamicNativeFunction1[T, R](name) { (ctx: NativeContext) => (arg: T) =>
      {
        val helper =
          NumericHelpers[Any](arg.numericType, arg.numericHelper, arg.integralHelper, arg.fractionalHelper)
        f(helper, ctx)(arg)
      }
    }
}

object NumericFunction1 {
  def apply[
      T <: RTValue.Primitive.Numeric[Any],
      R <: RTValue
  ](name: String)(f: (NumericHelpers[Any], NativeContext) => (T) => R) =
    new NumericFunction1(name)(f)
}

class NumericFunction2[
    T <: RTValue.Primitive.Numeric[N],
    R <: RTValue,
    N
](val name: String)(val f: (NumericHelpers[N], NativeContext) => (T, T) => R) {
  def asNative2 =
    DynamicNativeFunction2[T, T, R](name) { (ctx: NativeContext) => (arg1: T, arg2: T) =>
      {
        if (arg1.numericType != arg2.numericType) {
          throw IllegalValue(
            s"The values $arg1 and $arg2 are not of the same numeric type (${arg1.numericType} versus ${arg2.numericType})."
          )
        }
        val helper =
          NumericHelpers[N](arg1.numericType, arg1.numericHelper, arg1.integralHelper, arg1.fractionalHelper)
        f(helper, ctx)(arg1, arg2)
      }
    }
}

object NumericFunction2 {
//  def apply[
//      T <: RTValue.Primitive.Numeric[Any],
//      R <: RTValue
//  ](name: String)(f: (NumericHelpers[Any], NativeContext) => (T, T) => R) =
//    new NumericFunction2(name)(f)

  def apply[T](name: String)(f: (NumericHelpers[T], NativeContext) => (T, T) => RTValue) =
    new NumericFunction2(name)((h: NumericHelpers[T], ctx: NativeContext) =>
      (a: RTValue.Primitive.Numeric[T], b: RTValue.Primitive.Numeric[T]) =>
        f(h, ctx)(a.value, b.value)
    )
}
