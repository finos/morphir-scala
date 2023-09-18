package org.finos.morphir.runtime.internal

import org.finos.morphir.runtime.RTValue

sealed trait NativeFunctionSignature {
  def numArgs: Int
  def f: Any
}
object NativeFunctionSignature {
  case class Fun1(f: RTValue => RTValue)
      extends NativeFunctionSignature { val numArgs = 1 }
  case class Fun2(f: (RTValue, RTValue) => RTValue)
      extends NativeFunctionSignature { val numArgs = 2 }
  case class Fun3(f: (RTValue, RTValue, RTValue) => RTValue)
      extends NativeFunctionSignature { val numArgs = 3 }
  case class Fun4(f: (RTValue, RTValue, RTValue, RTValue) => RTValue)
      extends NativeFunctionSignature { val numArgs = 4 }
  case class Fun5(f: (
      RTValue,
      RTValue,
      RTValue,
      RTValue,
      RTValue
  ) => RTValue)
      extends NativeFunctionSignature { val numArgs = 5 }
}

// Advanced native function that takes a store
sealed trait NativeFunctionSignatureAdv {
  def numArgs: Int
  def f: InvokeableEvaluator => Any
  // Apply the store an convert back to a regular function
  def injectEvaluator(evaluator: InvokeableEvaluator) =
    this match {
      case NativeFunctionSignatureAdv.Fun1(f) => NativeFunctionSignature.Fun1(f(evaluator))
      case NativeFunctionSignatureAdv.Fun2(f) => NativeFunctionSignature.Fun2(f(evaluator))
      case NativeFunctionSignatureAdv.Fun3(f) => NativeFunctionSignature.Fun3(f(evaluator))
      case NativeFunctionSignatureAdv.Fun4(f) => NativeFunctionSignature.Fun4(f(evaluator))
      case NativeFunctionSignatureAdv.Fun5(f) => NativeFunctionSignature.Fun5(f(evaluator))
    }
}
object NativeFunctionSignatureAdv {
  case class Fun1(f: InvokeableEvaluator => RTValue => RTValue)
      extends NativeFunctionSignatureAdv { val numArgs = 1 }
  case class Fun2(f: InvokeableEvaluator => (RTValue, RTValue) => RTValue)
      extends NativeFunctionSignatureAdv { val numArgs = 2 }
  case class Fun3(f: InvokeableEvaluator => (RTValue, RTValue, RTValue) => RTValue)
      extends NativeFunctionSignatureAdv { val numArgs = 3 }
  case class Fun4(f: InvokeableEvaluator => (
      RTValue,
      RTValue,
      RTValue,
      RTValue
  ) => RTValue)
      extends NativeFunctionSignatureAdv { val numArgs = 4 }
  case class Fun5(f: InvokeableEvaluator => (
      RTValue,
      RTValue,
      RTValue,
      RTValue,
      RTValue
  ) => RTValue)
      extends NativeFunctionSignatureAdv { val numArgs = 5 }
}
