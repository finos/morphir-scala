package morphir.sdk

object Basics {

  type Integer = org.finos.morphir.universe.sdk.Basics.Integer
  private val Integer: org.finos.morphir.universe.sdk.Basics.Integer.type =
    org.finos.morphir.universe.sdk.Basics.Integer

  type Float = org.finos.morphir.universe.sdk.types.MFloat
  private val Float: org.finos.morphir.universe.sdk.types.MFloat.type =
    org.finos.morphir.universe.sdk.types.MFloat

  def add(a: Integer): Integer => Integer          = b => a add b
  def add(a: Integer, b: Integer): Integer         = Integer(a.value + b.value)
  def modBy(modulus: Integer): Integer => Integer  = x => Integer(x % modulus)
  def modBy(modulus: Integer, a: Integer): Integer = Integer(a % modulus)
}
