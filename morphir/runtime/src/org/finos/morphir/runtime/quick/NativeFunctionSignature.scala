package org.finos.morphir.runtime.quick

sealed trait NativeFunctionSignature {
  def numArgs: Int
  def f: Any
}
object NativeFunctionSignature {
  case class Fun1(f: Result => Result)
      extends NativeFunctionSignature { val numArgs = 1 }
  case class Fun2(f: (Result, Result) => Result)
      extends NativeFunctionSignature { val numArgs = 2 }
  case class Fun3(f: (Result, Result, Result) => Result)
      extends NativeFunctionSignature { val numArgs = 3 }
  case class Fun4(f: (Result, Result, Result, Result) => Result)
      extends NativeFunctionSignature { val numArgs = 4 }
  case class Fun5(f: (
      Result,
      Result,
      Result,
      Result,
      Result
  ) => Result)
      extends NativeFunctionSignature { val numArgs = 5 }
}

// Advanced native function that takes a store
sealed trait NativeFunctionSignatureAdv {
  def numArgs: Int
  def f: Loop => Any
  // Apply the store an convert back to a regular function
  def injectEvaluator(loop: Loop) =
    this match {
      case NativeFunctionSignatureAdv.Fun1(f) => NativeFunctionSignature.Fun1(f(loop))
      case NativeFunctionSignatureAdv.Fun2(f) => NativeFunctionSignature.Fun2(f(loop))
      case NativeFunctionSignatureAdv.Fun3(f) => NativeFunctionSignature.Fun3(f(loop))
      case NativeFunctionSignatureAdv.Fun4(f) => NativeFunctionSignature.Fun4(f(loop))
      case NativeFunctionSignatureAdv.Fun5(f) => NativeFunctionSignature.Fun5(f(loop))
    }
}
object NativeFunctionSignatureAdv {
  case class Fun1(f: Loop => Result => Result)
      extends NativeFunctionSignatureAdv { val numArgs = 1 }
  case class Fun2(f: Loop => (Result, Result) => Result)
      extends NativeFunctionSignatureAdv { val numArgs = 2 }
  case class Fun3(f: Loop => (Result, Result, Result) => Result)
      extends NativeFunctionSignatureAdv { val numArgs = 3 }
  case class Fun4(f: Loop => (
      Result,
      Result,
      Result,
      Result
  ) => Result)
      extends NativeFunctionSignatureAdv { val numArgs = 4 }
  case class Fun5(f: Loop => (
      Result,
      Result,
      Result,
      Result,
      Result
  ) => Result)
      extends NativeFunctionSignatureAdv { val numArgs = 5 }
}
