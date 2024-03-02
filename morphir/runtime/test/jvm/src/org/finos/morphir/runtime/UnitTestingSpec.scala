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
    os.pwd / "examples" / "morphir-elm-projects" / "unit-test-framework" / "example-project-tests" / "morphir-ir.json",
    os.pwd / "examples" / "morphir-elm-projects" / "unit-test-framework" / "example-project" / "morphir-ir.json"
  )
  val testSummaryLayer: ZLayer[Any, Throwable, TestSummary] =
    ZLayer(for {
      dists   <- ZIO.succeed(paths.map(path => EvaluationLibrary.loadDistribution(path.toString)))
      runtime <- ZIO.succeed(MorphirRuntime.quick(dists: _*))
      summary <- runtime.runUnitTests()
        .provideEnvironment(MorphirEnv.live)
        .toZIOWith(RTExecutionContext.typeChecked)
    } yield summary)

  // def testUnitTestingPasses(label: String) =
  //   test(label) {
  //     val runResult = ZIO.serviceWithZIO[TypedMorphirRuntime] { runtime =>
  //       runtime.runUnitTests()
  //         .provideEnvironment(MorphirEnv.live)
  //         .toZIOWith(RTExecutionContext.typeChecked)
  //     }

  //     runResult.map { actual =>
  //       assertTrue(!actual.passed)
  //     }
  //   }

  def getTestSummary =
    ZIO.serviceWithZIO[TestSummary] { summary }

  def spec = suite("Type Checker Tests")(
    suite("Happy Paths Tests")(
      // testEvaluation("Single test result")("ExampleModuleTests", "runSimpleTest")(Data.String("PASSED")),
      // testUnitTestingPasses("Suite Passed"),
      // test("Overall Status Failed") {
      //   getTestSummary.map(result => assertTrue(!result.passed))
      // }
      test("ZIOLess Test") {
        assertTrue(true)
      }
    )
  )
    .provideLayerShared(testSummaryLayer)
}
