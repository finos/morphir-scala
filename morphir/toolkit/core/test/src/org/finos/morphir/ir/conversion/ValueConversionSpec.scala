package org.finos.morphir
package ir
package conversion

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V}

import org.finos.morphir.ir.Literal.Lit

import zio.Chunk
import zio.test._
import org.finos.morphir.testing.MorphirBaseSpec
import V._

object ValueConversionSpec extends MorphirBaseSpec {
  def spec = suite("ValueConversion Spec")(
    booleanSuite,
    listSuite,
    unitSuite
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to convert a Scala Unit to a Morphir Unit") {
      val toValue     = ToMorphirValue.summon[scala.Unit].typed
      val inputValue  = ()
      val morphirUnit = toValue(inputValue)
      assertTrue(morphirUnit == (V.unit(Type.unit)))
    }
  )

  def booleanSuite = suite("Boolean")(
    test("Should be possible to convert a Scala Boolean to a Morphir Boolean") {
      val toValue           = ToMorphirValue.summon[Boolean].typed
      val trueValue         = true
      val falseValue        = false
      val morphirTrueValue  = toValue(trueValue)
      val morphirFalseValue = toValue(falseValue)
      assertTrue(morphirTrueValue == Lit.True, morphirFalseValue == Lit.False)
    }
  )

  def listSuite = suite("List")(
    test("Should be possible to convert a Scala List[Boolean] to a Morphir List[Boolean]") {
      val sut        = ToMorphirValue.summon[scala.List[Boolean]].typed
      val inputValue = List(true, false, true)
      val actual     = sut.toMorphirValue(inputValue)
      assertTrue(
        actual == V.Value.List(sdk.List.listType(sdk.Basics.boolType), zio.Chunk(Lit.True, Lit.False, Lit.True))
      )
    },
    test("Should be possible to convert a Scala List[Int] to a Morphir List[Int]") {
      val sut        = ToMorphirValue.summon[scala.List[Int]].typed
      val inputValue = List(10, 20, 30)
      val actual     = sut.toMorphirValue(inputValue)
      assertTrue(
        actual == V.Value.List(sdk.List.listType(sdk.Basics.intType), Chunk(intTyped(10)))
      )
    }
  )

}
