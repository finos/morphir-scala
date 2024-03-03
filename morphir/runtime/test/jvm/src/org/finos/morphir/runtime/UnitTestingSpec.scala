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
  // We don't want to even re-run the tests for every test
  val testSummaryLayer: ZLayer[Any, Throwable, TestSummary] =
    ZLayer(for {
      dists   <- ZIO.succeed(paths.map(path => EvaluationLibrary.loadDistribution(path.toString)))
      runtime <- ZIO.succeed(MorphirRuntime.quick(dists: _*))
      summary <- runtime.runUnitTests()
        .provideEnvironment(MorphirEnv.live)
        .toZIOWith(RTExecutionContext.typeChecked)
    } yield summary)

  def getTestSummary =
    ZIO.serviceWithZIO[TestSummary] { summary => ZIO.succeed(summary) }

  def moduleCounts(moduleName: String) = {
    val pkgName = PackageName.fromString("ExampleTests")
    val modName = ModuleName.fromString(moduleName)
    getTestSummary.map {
      summary => summary.countsAtModule(pkgName, modName)
    }
  }

  def spec = suite("Type Checker Tests")(
    suite("Happy Paths Tests")(
      // testEvaluation("Single test result")("ExampleModuleTests", "runSimpleTest")(Data.String("PASSED")),
      // testUnitTestingPasses("Suite Passed"),
      test("Show Results (for human readability check - ignore)") {
        getTestSummary.map { result =>
          println(result)
          assertTrue(false)
        }
      },
      suite("Overall Checks")(
        test("Overall Status Failed") {
          getTestSummary.map { result =>
            assertTrue(!result.passed)
          }
        },
        test("Passed Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.passed == 8)
          }
        },
        test("Failed Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.failed == 31)
          }
        },
        test("Error Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.errors == 5)
          }
        },
        test("Skipped Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.skipped == 5)
          }
        },
        test("Todo Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.todo == 1)
          }
        }
      ),
      suite("Modules Correct")(
        test("Module One Counts") {
          moduleCounts("FailingModuleOne").map { counts =>
            assertTrue(counts == Some(TestResultCounts(
              passed = 6,
              failed = 30,
              errors = 1,
              skipped = 0,
              todo = 0
            )))
          }
        },
        test("Module One Status") {
          moduleCounts("FailingModuleOne").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Failed))
          }
        },
        test("Module Two Counts") {
          moduleCounts("FailingModuleTwo").map { counts =>
            assertTrue(counts == Some(TestResultCounts(
              passed = 1,
              failed = 1,
              errors = 4,
              skipped = 0,
              todo = 0
            )))
          }
        },
        test("Module Two Status") {
          moduleCounts("FailingModuleTwo").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Failed))
          }
        },
        test("Incomplete Counts") {
          moduleCounts("IncompleteModule").map { counts =>
            assertTrue(counts == Some(TestResultCounts(
              passed = 1,
              failed = 0,
              errors = 0,
              skipped = 5,
              todo = 1
            )))
          }
        },
        test("Incomplete Module Status") {
          moduleCounts("Incomplete").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Incomplete))
          }
        }
      )
    )
  )
    .provideLayerShared(testSummaryLayer)
}
