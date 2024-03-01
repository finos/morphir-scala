package org.finos.morphir.runtime

import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import zio.test.TestAspect.{ignore, tag}
import zio.{Console, ZIO, ZLayer}

object UnitTestingSpec extends MorphirBaseSpec {
  val paths = List(
    os.pwd / "examples" / "morphir-elm-projects" / "unit-test-framework" / "morphir-unit-test" / "morphir-ir.json",
    os.pwd / "examples" / "morphir-elm-projects" / "unit-test-framework" / "example-project-tests" / "morphir-ir.json",
    os.pwd / "examples" / "morphir-elm-projects" / "unit-test-framework" / "example-project" / "morphir-ir.json"
  )
  val morphirRuntimeLayer: ZLayer[Any, Throwable, TypedMorphirRuntime] =
    ZLayer(for {
      dists <- ZIO.succeed(paths.map(path => EvaluationLibrary.loadDistribution(path.toString)))
    } yield MorphirRuntime.quick(dists: _*))

  def testEvaluation(label: String)(moduleName: String, functionName: String)(expected: => Data) =
    test(label) {
      val runResult = ZIO.serviceWithZIO[TypedMorphirRuntime] { runtime =>
        val fullName = s"ExampleTests:$moduleName:$functionName"
        runtime.evaluate(
          FQName.fromString(fullName),
          Data.Unit
        )
          .provideEnvironment(MorphirEnv.live)
          .toZIOWith(RTExecutionContext.typeChecked)
      }

      runResult.map { actual =>
        assertTrue(actual == expected)
      }
    }

  def testUnitTestingPasses(label: String)(moduleName: String, functionName: String) =
    test(label) {
      val runResult = ZIO.serviceWithZIO[TypedMorphirRuntime] { runtime =>
        runtime.runUnitTests()
          .provideEnvironment(MorphirEnv.live)
          .toZIOWith(RTExecutionContext.typeChecked)
      }

      runResult.map { actual =>
        println(actual.message)
        assertTrue(!actual.success)
      }
    }

  def spec =
    suite("Type Checker Tests")(
      suite("Happy Paths Tests")(
        testEvaluation("Single test result")("ExampleModuleTests", "runSimpleTest")(Data.String("PASSED")),
        testUnitTestingPasses("Suite Passed")("ExampleModuleTests", "runSimpleTest")
      )
    ).provideLayerShared(morphirRuntimeLayer)
}
