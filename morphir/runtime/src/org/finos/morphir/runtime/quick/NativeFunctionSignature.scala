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
  case class Fun5[TA, VA](f: (Result[TA, VA], Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignature[TA, VA] { val numArgs = 5 }
}

// Advanced native function that takes a store
sealed trait NativeFunctionSignatureAdv[TA, VA] {
  def numArgs: Int
  def f: Store[TA, VA] => Any
}
object NativeFunctionSignatureAdv {
  case class Fun1[TA, VA](f: Store[TA, VA] => Result[TA, VA] => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 1 }
  case class Fun2[TA, VA](f: Store[TA, VA] => (Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 2 }
  case class Fun3[TA, VA](f: Store[TA, VA] => (Result[TA, VA], Result[TA, VA], Result[TA, VA]) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 3 }
  case class Fun4[TA, VA](f: Store[TA, VA] => (
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA]
  ) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 4 }
  case class Fun5[TA, VA](f: Store[TA, VA] => (
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA],
      Result[TA, VA]
  ) => Result[TA, VA])
      extends NativeFunctionSignatureAdv[TA, VA] { val numArgs = 5 }
}
