package morphir.sdk

object Basics {

  type Integer = org.finos.morphir.universe.sdk.types.MInteger
  private val Integer: org.finos.morphir.universe.sdk.types.MInteger.type =
    org.finos.morphir.universe.sdk.types.MInteger

  type Float = org.finos.morphir.universe.sdk.types.MFloat
  private val Float: org.finos.morphir.universe.sdk.types.MFloat.type =
    org.finos.morphir.universe.sdk.types.MFloat

  def add(x: Integer, y: Integer): Integer = ??? // Integer.add(x, y)
}
