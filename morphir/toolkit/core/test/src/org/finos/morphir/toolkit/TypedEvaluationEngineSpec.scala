package org.finos.morphir
package toolkit

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.ir.{Type => T}
import org.finos.morphir.ir.{Value => V}
import zio.{test => _, _}
import zio.prelude.fx._
import zio.test._
import zio.test.TestAspect.{ignore, tag}
import EvaluationEngine._
import V._

trait TypedEvaluationEngineSpec { self: MorphirBaseSpec =>
  def typedEvaluationEngineSuite =
    suite("For TypedValue")(
      letDefinitionSuite,
      listSuite,
      literalSuite,
      unitSuite,
      ifThenElseSuite,
      variableSuite,
      tupleSuite
    ).provide(
      ZLayer.succeed(EvaluationEngine.typed)
    )

  def tupleSuite = suite("Tuple")(
    suite("Of Literals")(
      test("Should be possible to evaluate a Tuple of literal values") {
        val value: TypedValue = V.tuple(
          V.boolean(true)    -> ir.sdk.Basics.boolType,
          V.string("Batman") -> ir.sdk.String.stringType,
          V.float(42.5)      -> ir.sdk.Basics.floatType
        )
        val context = Context.Typed.createRoot()
        for {
          actual <- evaluateZIO(value, context)
        } yield assertTrue(actual == (true, "Batman", 42.5))
      },
      suite("Of Variables")(
        test("Should resolve nested variables") {
          val aName = Name.fromString("a")
          val nName = Name.fromString("n")
          val value = V.tuple(V.variable(aName) -> ir.sdk.Char.charType, V.variable(nName) -> ir.sdk.Basics.intType)
          val context = EvaluationEngine.Context.Typed.createRoot(
            Var(aName) := 'A',
            Var(nName) := 42
          )
          for {
            actual <- evaluateZIO(value, context)
          } yield assertTrue(actual == ('A', 42))
        }
      ),
      TupleAritySuite.evaluatesTupleArities2to22Suite
    )
  )

  def ifThenElseSuite = suite("ifThenElse")(
    test("Should evaluate true condition to ThenBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(true), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 2)
    },
    test("Should evaluate false condition to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(false), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 6)
    },
    test("Should evaluate any other non true conditions to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.int(3), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      val context           = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == 6)
    }
  )

  def letDefinitionSuite = suite("LetDefinition")(
    test("Should support simple let involving an Int literal") {
      val value   = V.let("n", 42, V.variable("n") :> ir.sdk.Basics.intType)
      val context = Context.Typed.createRoot()
      for {
        // initalVars <-  ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
        actual <- evaluateZIO(value, context)
        // finalVars  <- ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
      } yield assertTrue(actual == 42 /*, initalVars == finalVars*/ )
    },
    test("Should support nested let definitions") {

      // Let x = 3 in ((let x = 2 in x), x)
      val innerLet: TypedValue = V.let("x", 2, V.variable("x") :> ir.sdk.Basics.intType)
      val value =
        V.let(
          "x",
          3,
          V.tuple(
            T.tuple(ir.sdk.Basics.intType, ir.sdk.Basics.intType),
            innerLet,
            V.variable("x") :> ir.sdk.Basics.intType
          )
        )
      val context = Context.Typed.createRoot()
      for {
        actual <- evaluateZIO(value, context)
      } yield assertTrue(actual == (2, 3))
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
        val context = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- evaluateZIO(value, context)
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
        val context = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- EvaluationEngine.evaluateZIO(value, context)
        } yield assertTrue(actual == List(10, 20, 30, 40))
      }
    )
  )

  def literalSuite = suite("Literal")(
    test("Should evaluate True to true") {
      val value: TypedValue = V.boolean(true) :> ir.sdk.Basics.boolType
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == true)
    },
    test("Should evaluate False to false") {
      val value: TypedValue = V.boolean(false) :> ir.sdk.Basics.boolType
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == false)
    },
    test("Should evaluate a String Literal to its value") {
      check(Gen.string) { strValue =>
        val value: TypedValue = V.string(strValue) :> ir.sdk.String.stringType
        val context           = EvaluationEngine.Context.Typed.createRoot()
        for {
          actual <- EvaluationEngine.evaluateZIO(value, context)
        } yield assertTrue(actual == strValue)
      }
    }
  )

  def unitSuite: Spec[EvaluationEngine[Unit, T.Type[Unit]], EvaluationError] = suite("Unit")(
    test("Should be possible to evaluate a Unit value") {
      val value: TypedValue = V.unit(T.unit)
      val context           = EvaluationEngine.Context.Typed.createRoot()
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
      } yield assertTrue(actual == ())
    }
  )

  def variableSuite = suite("Variable")(
    test("Should be possible to resolve a variable that has the Scala unitValue") {
      val varName = Name.fromString("testVar")
      val value   = V.variable(varName) :> T.unit
      val context = EvaluationEngine.Context.Typed.createRoot(
        Var(varName) := ()
      )
      for {
        actual <- EvaluationEngine.evaluateZIO(value, context)
        _      <- Console.printLine(s"Actual has a type of ${actual.getClass.getSimpleName}")
      } yield assertTrue(actual == ())
    },
    test("Should be possible to resolve a variable that has a Scala boolean value") {
      val trueVarName  = Name.fromString("trueVar")
      val falseVarName = Name.fromString("falseVar")
      val trueValue    = V.variable(trueVarName) :> ir.sdk.Basics.boolType
      val falseValue   = V.variable(falseVarName) :> ir.sdk.Basics.boolType

      val context = EvaluationEngine.Context.Typed.createRoot(
        Var(trueVarName)  := true,
        Var(falseVarName) := false
      )
      for {
        actualTrueValue  <- EvaluationEngine.evaluateZIO(trueValue, context)
        actualFalseValue <- EvaluationEngine.evaluateZIO(falseValue, context)
      } yield assertTrue(actualTrueValue == true, actualFalseValue == false)
    }
  )
}
