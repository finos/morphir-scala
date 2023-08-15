package org.finos.morphir.runtime

import org.finos.morphir.naming._
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.{Type as T, Value as V}
import org.finos.morphir.ir.Value.{Value, Pattern, TypedValue, USpecification => UValueSpec}
import org.finos.morphir.ir.Type.{Type, UType, USpecification => UTypeSpec}
import org.finos.morphir.ir.sdk
import org.finos.morphir.ir.sdk.Basics
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import zio.test.TestAspect.{ignore, tag}
import zio.{Console, ZIO, ZLayer}
import org.finos.morphir.ir.Value.RawValueExtensions

object TypeCheckerTests extends MorphirBaseSpec {
  type MorphirRuntimeTyped = MorphirRuntime[Unit, UType]

  val morphirRuntimeLayer: ZLayer[Any, Throwable, MorphirRuntime[Unit, UType]] =
    ZLayer(for {
      irFilePath <- ZIO.succeed(os.pwd / "examples" / "morphir-elm-projects" / "evaluator-tests" / "morphir-ir.json")
      _          <- Console.printLine(s"Loading distribution from $irFilePath")
      dist       <- EvaluationLibrary.loadDistributionFromFileZIO(irFilePath.toString)
    } yield MorphirRuntime.quick(dist))

  val typeCheckerLayer: ZLayer[Any, Throwable, TypeChecker] =
    ZLayer(for {
      irFilePath <- ZIO.succeed(os.pwd / "examples" / "morphir-elm-projects" / "evaluator-tests" / "morphir-ir.json")
      _          <- Console.printLine(s"Loading distribution from $irFilePath")
      dist       <- EvaluationLibrary.loadDistributionFromFileZIO(irFilePath.toString)
    } yield TypeChecker(Distributions(dist)))

  def testTypeConforms(tpe1: UType, tpe2 : UType) : ZIO[TypeChecker, Throwable, TestResult] = {
    ZIO.serviceWithZIO[TypeChecker] { checker =>
      for{
        errors <- ZIO.succeed(checker.conformsTo(tpe1, tpe2))
        errorMsgs = errors.map(error => s"\n\t${error.getMsg}").mkString("")
        assert <- if (errors.length == expectedErrors) assertCompletes
        else assertTrue(errorMsgs == s"Expected $expectedErrors errors")
      } yield assert
    }
  }
  def testTypeCheck(value: TypedValue)(expectedErrors: Int): ZIO[TypeChecker, Throwable, TestResult] =
    ZIO.serviceWithZIO[TypeChecker] { checker =>
      for {
        errors <- ZIO.succeed(checker.check(value))
        errorMsgs = errors.map(error => s"\n\t${error.getMsg}").mkString("")
        assert <- if (errors.length == expectedErrors) assertCompletes
        else assertTrue(errorMsgs == s"Expected $expectedErrors errors")
      } yield assert // TODO: Cleaner "fails" impl
    }
  def runTypeCheck(value: TypedValue): ZIO[TypeChecker, Throwable, List[MorphirTypeError]] =
    ZIO.serviceWithZIO[TypeChecker] { checker =>
      ZIO.succeed(checker.check(value))
    }

  val validString: TypedValue = V.string(sdk.String.stringType, "Green")
  val invalidInt: TypedValue  = V.string("Red") :> sdk.Basics.intType
  val intToInt: TypedValue = V.reference(
    T.function(Basics.intType, Basics.intType),
    FQName.fromString("Morphir/Examples/App:TypeCheckerTests:intToInt")
  )
  val invalidFunction: TypedValue = V.reference(
    T.function(Basics.intType, Basics.boolType),
    FQName.fromString("Morphir/Examples/App:TypeCheckerTests:intToInt")
  )
  def spec =
    suite("Type Checker Tests")(
      suite("Apply Node")(
        test("Apply to non function") {
          val badApply: TypedValue = V.apply(Basics.intType, V.intTyped(1), V.intTyped(1))
          testTypeCheck(badApply)(1)
        },
        test("Apply arg type wrong") {
          val badApply: TypedValue = V.apply(Basics.intType, intToInt, validString)
          testTypeCheck(badApply)(1)
        },
        test("Apply return type wrong") {
          val badApply: TypedValue = V.apply(Basics.boolType, intToInt, V.intTyped(1))
          testTypeCheck(badApply)(1)
        },
        test("Args are recursively checked") {
          val badApply: TypedValue = V.apply(Basics.intType, intToInt, invalidInt)
          testTypeCheck(badApply)(1)
        },
        test("Body is recursively checked") {
          val badApply: TypedValue = V.apply(Basics.boolType, invalidFunction, V.intTyped(2))
          testTypeCheck(badApply)(-1)
        }
        // TODO: Aliased function type
      ),
      suite("Literal Node")(
        test("Strings are not Ints") {
          testTypeCheck(invalidInt)(1)
        },
        test("Ints are not Strings") {
          testTypeCheck(V.int(1) :> sdk.String.stringType)(1)
        },
        test("Ints are not Floats") {
          testTypeCheck(V.int(1) :> sdk.Basics.floatType)(1)
        },
        test("Bools are not Floats") {
          testTypeCheck(V.boolean(true) :> sdk.Basics.floatType)(1)
        }
        // TODO: Other lit tests
      ),
      suite("Type confomrity")(
        test("IntType is not StringType"){
          testTypeConforms(Basics.intType, sdk.String.stringType)
        }
      )
    ).provideLayerShared(typeCheckerLayer)
}
