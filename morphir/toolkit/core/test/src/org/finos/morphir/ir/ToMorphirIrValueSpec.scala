package org.finos.morphir
package ir

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V, _}
import org.finos.morphir.ir.Literal.Lit

import zio.test._
import org.finos.morphir.testing.MorphirBaseSpec

object ValueConversionSpec extends MorphirBaseSpec {
  def spec = suite("ValueConversion Spec")(
    booleanSuite,
    listSuite,
    unitSuite
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to convert a Scala Unit to a Morphir Unit") {
      val toValue     = ToMorphirTypedValue[scala.Unit]
      val inputValue  = ()
      val morphirUnit = toValue(inputValue)
      assertTrue(morphirUnit == (Value.unit(Type.unit)))
    }
  )

  def booleanSuite = suite("Boolean")(
    test("Should be possible to convert a Scala Boolean to a Morphir Boolean") {
      val toValue           = ToMorphirTypedValue[Boolean]
      val trueValue         = true
      val falseValue        = false
      val morphirTrueValue  = toValue(trueValue)
      val morphirFalseValue = toValue(falseValue)
      assertTrue(morphirTrueValue == Lit.True, morphirFalseValue == Lit.False)
    }
  )

  def listSuite = suite("List")(
    test("Should be possible to convert a Scala List[Boolean] to a Morphir List[Boolean]") {
      val toValue    = ToMorphirTypedValue[scala.List[Boolean]]
      val inputValue = List(true, false, true)
      val actual     = toValue(inputValue)
      assertTrue(
        actual == V.Value.List(sdk.List.listType(sdk.Basics.boolType), zio.Chunk(Lit.True, Lit.False, Lit.True))
      )
    }
  )

}
