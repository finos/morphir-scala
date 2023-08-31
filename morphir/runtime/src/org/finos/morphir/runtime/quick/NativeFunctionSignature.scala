package org.finos.morphir.runtime.quick

sealed trait NativeFunctionSignature[TA, VA] {
  def numArgs: Int
  def f: Any
}
object NativeFunctionSignature {
  case class Fun1[TA, VA](f: Result[TA, VA] => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 1 }
  case class Fun2[TA, VA](f: (Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 2 }
  case class Fun3[TA, VA](f: (Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 3 }
  case class Fun4[TA, VA](f: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 4 }
  case class Fun5[TA, VA](f: (
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA]
  ) => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 5 }
}

// Advanced native function that takes a store
sealed trait NativeFunctionSignatureAdv[TA, VA] {
  def numArgs: Int
  def f: Loop[TA, VA] => Any
  // Apply the store an convert back to a regular function
  def injectEvaluator(loop: Loop[TA, VA]) =
    this match {
      case NativeFunctionSignatureAdv.Fun1(f) => NativeFunctionSignature.Fun1(f(loop))
      case NativeFunctionSignatureAdv.Fun2(f) => NativeFunctionSignature.Fun2(f(loop))
      case NativeFunctionSignatureAdv.Fun3(f) => NativeFunctionSignature.Fun3(f(loop))
      case NativeFunctionSignatureAdv.Fun4(f) => NativeFunctionSignature.Fun4(f(loop))
      case NativeFunctionSignatureAdv.Fun5(f) => NativeFunctionSignature.Fun5(f(loop))
    }
}
object NativeFunctionSignatureAdv {
  case class Fun1[TA, VA](f: Loop[TA, VA] => Result[TA, VA] => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 1 }
  case class Fun2[TA, VA](f: Loop[TA, VA] => (Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 2 }
  case class Fun3[TA, VA](f: Loop[TA, VA] => (Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 3 }
  case class Fun4[TA, VA](f: Loop[TA, VA] => (
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA]
  ) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 4 }
  case class Fun5[TA, VA](f: Loop[TA, VA] => (
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA]
  ) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 5 }
}
