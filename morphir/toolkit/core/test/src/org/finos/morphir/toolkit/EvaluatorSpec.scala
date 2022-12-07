package org.finos.morphir
package toolkit

import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.Value as V
import zio.{test as _, *}
import zio.test.*
import zio.test.TestAspect.{ignore, tag}
import org.finos.morphir.testing.MorphirBaseSpec
import EvaluationContext.{VariableRef, Variables}
import V.*
import org.finos.morphir.toolkit.EvaluationWithTypedValueVisitorSpecs.eval

object EvaluatorSpec extends MorphirBaseSpec with EvaluationWithTypedValueVisitorSpecs {
  def spec = suite("Evaluator Spec")(
    typedValueVisitorSuite
  )
}

trait EvaluationWithTypedValueVisitorSpecs {
  self: MorphirBaseSpec =>
  def typedValueVisitorSuite = suite("With TypedValueVisitor")(
    ifThenElseSuite,
    letDefinitionSuite,
    listSuite,
    literalSuite,
    tupleSuite,
    unitSuite,
    variableSuite
  )

  def ifThenElseSuite = suite("ifThenElse")(
    test("Should evaluate true condition to ThenBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(true), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      for {
        actual <- eval(value)
      } yield assertTrue(actual == 2)
    },
    test("Should evaluate false condition to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.boolean(false), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      for {
        actual <- eval(value)
      } yield assertTrue(actual == 6)
    },
    test("Should evaluate any other non true conditions to ElseBranch") {
      val value: TypedValue = V.ifThenElse(V.int(3), V.int(2), V.int(6)) :> ir.sdk.Basics.intType
      for {
        actual <- eval(value)
      } yield assertTrue(actual == 6)
    }
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

  def letDefinitionSuite = suite("LetDefinition")(
    test("Should suuport simple let involving an Int literal") {
      val value = V.let("n", 42, V.variable("n") :> ir.sdk.Basics.intType)
      ZIO.stateful(EvaluationContext.Typed.root) {
        for {
          initalVars <- ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
          actual     <- eval(value)
          finalVars  <- ZIO.getStateWith[EvaluationContext.Typed](ctx => ctx.allVariables)
        } yield assertTrue(actual == 42, initalVars == finalVars)
      }
    }
    // test("Should support nested let definitions") {
    //   // Let x = 3 in (let x = 2 in x) + x
    //   val innerLet: TypedValue = V.let("x", 2, V.variable("x") :> ir.sdk.Basics.intType)
    //   val value =
    //     V.let(
    //       "x",
    //       3,
    //       V.tuple(
    //         T.tuple(ir.sdk.Basics.intType, ir.sdk.Basics.intType),
    //         innerLet,
    //         V.variable("x") :> ir.sdk.Basics.intType
    //       )
    //     )
    //   for {
    //     actual <- eval(value)
    //   } yield assertTrue(actual == (3, 2))
    // }
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

  def tupleSuite = suite("Tuple")(
    suite("Of Literals")(
      test("Should be possible to evaluate a Tuple of literal values") {
        val value: TypedValue = V.tuple(
          V.boolean(true)    -> ir.sdk.Basics.boolType,
          V.string("Batman") -> ir.sdk.String.stringType /*, V.float(42.5) -> ir.sdk.Basics.floatType*/
        )
        for {
          actual <- eval(value)
        } yield assertTrue(actual == (true, "Batman" /*, 42.5*/ ))
      }
    ),
    suite("Of Variables")(
      test("Should resolve nested variables") {
        val aName = Name.fromString("a")
        val nName = Name.fromString("n")
        val value = V.tuple(V.variable(aName) -> ir.sdk.Char.charType, V.variable(nName) -> ir.sdk.Basics.intType)
        implicit val initialCtx = EvaluationContext.root(
          Variables.empty
            .set(aName, VariableRef.Evaluated('A', ir.sdk.Char.charType))
            .set(nName, VariableRef.Evaluated(42, ir.sdk.Basics.intType))
        )
        for {
          actual <- eval(value)
        } yield assertTrue(actual == ('A', 42))
      }
    ),
    TupleAritySuite.evaluatesTupleArities2to22Suite
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to evaluate a Unit value") {
      val value = V.unit(T.unit)
      for {
        actual <- eval(value)
      } yield assertTrue(actual == ())
    }
  )

  def variableSuite = suite("Variable")(
    test("Should be possible to resolve a variable that has the Scala unitValue") {
      val varName             = Name.fromString("testVar")
      val value               = V.variable(varName) :> T.unit
      implicit val initialCtx = EvaluationContext.root(Variables.empty.set(varName, VariableRef.Evaluated((), T.unit)))
      ZIO.stateful(initialCtx) {
        for {
          actual <- eval(value)
          _      <- Console.printLine(s"Actual has a type of ${actual.getClass.getSimpleName}")
        } yield assertTrue(actual == ())
      }
    },
    test("Should be possible to resolve a variable that has a Scala boolean value") {
      val trueVarName  = Name.fromString("trueVar")
      val falseVarName = Name.fromString("falseVar")
      val trueValue    = V.variable(trueVarName) :> ir.sdk.Basics.boolType
      val falseValue   = V.variable(falseVarName) :> ir.sdk.Basics.boolType

      implicit val initialCtx = EvaluationContext.root(
        Variables.empty
          .set(trueVarName, VariableRef.Evaluated(true, ir.sdk.Basics.boolType))
          .set(falseVarName, VariableRef.Evaluated(false, ir.sdk.Basics.boolType))
      )
      for {
        actualTrueValue  <- eval(trueValue)
        actualFalseValue <- eval(falseValue)
      } yield assertTrue(actualTrueValue == true, actualFalseValue == false)
    }
  )
}

object EvaluationWithTypedValueVisitorSpecs {
  def eval(
      value: TypedValue
  )(implicit
      evaluator: Evaluator.Typed = Evaluator.Typed(),
      ctx: Evaluator.Typed.Ctx = EvaluationContext.Typed.root
  ) = {
    var state = ZState.initial(ctx)
    evaluator.evaluate(value).provide(state)
  }
}
