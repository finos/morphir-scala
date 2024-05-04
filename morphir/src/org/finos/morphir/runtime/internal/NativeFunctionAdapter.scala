package org.finos.morphir.runtime.internal

import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.internal.{
  DynamicNativeFunction,
  DynamicNativeFunction1,
  DynamicNativeFunction2,
  DynamicNativeFunction3,
  DynamicNativeFunction4,
  DynamicNativeFunction5,
  NativeContext
}
import org.finos.morphir.runtime.{Coercer, RTValue, SDKValue, internal}
import org.finos.morphir.{Hints, MorphirTag, naming}

sealed trait NativeFunctionAdapter {
  def dnf: DynamicNativeFunction
  def realize: SDKValue.SDKNativeInnerFunction
}

// format: off
object NativeFunctionAdapter {
  case class Fun1[T1 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction1[T1, R])(implicit u1: Coercer[T1]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun1(loop => r1 => dnf.f(internal.NativeContext(loop))(u1.coerce(r1))) }
  }
  case class Fun2[T1 <: RTValue, T2 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction2[T1, T2, R])(implicit u1: Coercer[T1], u2: Coercer[T2]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun2(loop => (r1, r2) => dnf.f(NativeContext(loop))(u1.coerce(r1), u2.coerce(r2))) }
  }
  case class Fun3[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction3[T1, T2, T3, R])(implicit u1: Coercer[T1], u2: Coercer[T2], u3: Coercer[T3]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun3(loop => (r1, r2, r3) => dnf.f(internal.NativeContext(loop))(u1.coerce(r1), u2.coerce(r2), u3.coerce(r3))) }
  }
  case class Fun4[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction4[T1, T2, T3, T4, R])(implicit u1: Coercer[T1], u2: Coercer[T2], u3: Coercer[T3], u4: Coercer[T4]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun4(loop => (r1, r2, r3, r4) => dnf.f(internal.NativeContext(loop))(u1.coerce(r1), u2.coerce(r2), u3.coerce(r3), u4.coerce(r4))) }
  }
  case class Fun5[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, T5 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction5[T1, T2, T3, T4, T5, R])(implicit u1: Coercer[T1], u2: Coercer[T2], u3: Coercer[T3], u4: Coercer[T4], u5: Coercer[T5]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun5(loop => (r1, r2, r3, r4, r5) => dnf.f(internal.NativeContext(loop))(u1.coerce(r1), u2.coerce(r2), u3.coerce(r3), u4.coerce(r4), u5.coerce(r5))) }
  }
  case class Fun6[T1 <: RTValue, T2 <: RTValue, T3 <: RTValue, T4 <: RTValue, T5 <: RTValue, T6 <: RTValue, R <: RTValue](val dnf: DynamicNativeFunction6[T1, T2, T3, T4, T5, T6, R])(implicit u1: Coercer[T1], u2: Coercer[T2], u3: Coercer[T3], u4: Coercer[T4], u5: Coercer[T5], u6: Coercer[T6]) extends NativeFunctionAdapter {
    def realize: SDKValue.SDKNativeInnerFunction =
      SDKValue.SDKNativeInnerFunction { new NativeFunctionSignatureAdv.Fun6(loop => (r1, r2, r3, r4, r5, r6) => dnf.f(internal.NativeContext(loop))(u1.coerce(r1), u2.coerce(r2), u3.coerce(r3), u4.coerce(r4), u5.coerce(r5), u6.coerce(r6))) }
  }
}
// format: on
