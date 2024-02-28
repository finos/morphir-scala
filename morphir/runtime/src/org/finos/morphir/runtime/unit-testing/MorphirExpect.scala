package org.finos.morphir.runtime
import org.finos.morphir.naming.*

sealed trait MorphirExpect {
  def arity: Int;
  def localName: Local
}

object MorphirExpect {
  def expectation(result: RTValue) =
    RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
  val passed =
    val result = RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
    expectation(result)
  def failed(msg: String) =
    val result =
      RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(Primitive.String(msg)))
    expectation(result)

}
