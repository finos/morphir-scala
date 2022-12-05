package org.finos.morphir
package toolkit

import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V}

import zio.{test => _, _}
import zio.test._
import zio.test.TestAspect.{ignore, tag}
import org.finos.morphir.testing.MorphirBaseSpec
import V._

object EvaluatorSpec extends MorphirBaseSpec with EvaluationWithTypedValueVisitorSpecs {
  def spec = suite("Evaluator Spec")(
    typedValueVisitorSuite
  )

}

trait EvaluationWithTypedValueVisitorSpecs { self: MorphirBaseSpec =>
  def typedValueVisitorSuite = suite("With TypedValueVisitor")(
    listSuite,
    literalSuite,
    unitSuite
  )

  def literalSuite = suite("Literal")(
    test("Should evaluate True to true") {
      val value: TypedValue = V.boolean(true) :> ir.sdk.Basics.boolType
      for {
        actual <- eval(value)
      } yield assertTrue(actual == true)
    },
    test("Should evaluate False to false") {
      val value: TypedValue = V.boolean(false) :> ir.sdk.Basics.boolType
      for {
        actual <- eval(value)
      } yield assertTrue(actual == false)
    },
    test("Should evaluate a String Literal to its value") {
      check(Gen.string) { strValue =>
        val value: TypedValue = V.string(strValue) :> ir.sdk.String.stringType
        for {
          actual <- eval(value)
        } yield assertTrue(actual == strValue)
      }
    }
  )

  def listSuite = suite("List")(
    suite("Of Literals")(
      test("Should evaluate a List of Booleans") {
        val value: TypedValue = V.listOf(
          ir.sdk.Basics.boolType,
          V.boolean(true) :> ir.sdk.Basics.boolType,
          V.boolean(false) :> ir.sdk.Basics.boolType
        )
        for {
          actual <- eval(value)
        } yield assertTrue(actual == List(true, false))
      },
      test("Should evaluate a List of Ints") {
        val value: TypedValue = V.listOf(
          ir.sdk.Basics.intType,
          V.int(10) :> ir.sdk.Basics.intType,
          V.int(20) :> ir.sdk.Basics.intType,
          V.int(30) :> ir.sdk.Basics.intType,
          V.int(40) :> ir.sdk.Basics.intType
        )
        for {
          actual <- eval(value)
        } yield assertTrue(actual == List(10, 20, 30, 40))
      }
    )
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to evaluate a Unit value") {
      val value = V.unit(T.unit)
      for {
        actual <- eval(value)
      } yield assertTrue(actual == ())
    }
  )

  def eval(
      value: TypedValue
  )(implicit ctx: Evaluator.TypedEvaluationContext = new Evaluator.TypedEvaluationContext {}) = {
    val visitor = new TypedValueVisitor.Default
    visitor.evaluate(value).provide(ZLayer.succeed(ctx))
  }
}
