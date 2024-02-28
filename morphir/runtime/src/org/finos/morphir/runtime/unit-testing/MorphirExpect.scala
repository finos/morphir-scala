package org.finos.morphir.runtime
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
import org.finos.morphir.ir.Value.Value.{List as ListValue, *}

sealed trait MorphirExpect {
  def arity: Int
  def funcName: String
  def thunkify: TypedValue => Option[TypedValue] = {
      value match {
        case app @ Apply(_, Apply(_, Reference(_, funcName), arg1IR), arg2IR)}
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
