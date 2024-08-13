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
  val testFrameworkPath =
    "morphir-elm/sdks/morphir-unit-test/morphir-ir.json"
  val basicPath = "examples/morphir-elm-projects/unit-test-framework/example-project/morphir-ir.json"
  val failingPaths = List(
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests/morphir-ir.json",
    basicPath,
    testFrameworkPath
  )
  val passingPaths = List(
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests-passing/morphir-ir.json",
    basicPath,
    testFrameworkPath
  )
  val incompletePaths = List(
    "examples/morphir-elm-projects/unit-test-framework/example-project-tests-incomplete/morphir-ir.json",
    basicPath,
    testFrameworkPath
  )

  def makeTestSummaryLayer(paths: List[String]): ZLayer[Any, Throwable, TestSummary] =
    ZLayer(for {
      dists <- ZIO.collectAll(paths.map(path => EvaluationLibrary.loadDistributionFromFileZIO(path)))
      runtime = MorphirRuntime.quick(dists: _*)
      summary <- runtime.runUnitTests()
        .provideEnvironment(MorphirEnv.live)
        .toZIOWith(RTExecutionContext.typeChecked)
    } yield summary)
  val incompleteTestSummaryLayer: ZLayer[Any, Throwable, TestSummary] = makeTestSummaryLayer(incompletePaths)
  val passingTestSummaryLayer: ZLayer[Any, Throwable, TestSummary]    = makeTestSummaryLayer(passingPaths)
  val failingTestSummaryLayer: ZLayer[Any, Throwable, TestSummary]    = makeTestSummaryLayer(failingPaths)

  def getTestSummary =
    ZIO.serviceWithZIO[TestSummary] { summary => ZIO.succeed(summary) }

  def moduleCounts(packageName: String, moduleName: String) = {
    val pkgName = PackageName.fromString(packageName)
    val modName = ModuleName.fromString(moduleName)
    getTestSummary.map {
      summary => summary.countsAtModule(pkgName, modName)
    }
  }

  def spec = suite("Unit Testing Framework Tests")(
    suite("Failing Project")(
      test("Show Results (for human readability check - ignore)") {
        getTestSummary.map { result =>
          println(result)
          assertTrue(false)
        }
      } @@ TestAspect.ignore,
      suite("Overall Checks")(
        test("Overall Status Failed") {
          getTestSummary.map { result =>
            assertTrue(!result.passed)
          }
        },
        test("Passed Count Correct") {
          getTestSummary.map { result =>
            assertTrue(result.overallCounts.passed == 26)
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
        },
        test("Failed isn't Passed") {
          getTestSummary.map { result =>
            assertTrue(!result.passed)
          }
        },
        test("Failed isn't Incomplete") {
          getTestSummary.map { result =>
            assertTrue(!result.incomplete)
          }
        },
        test("Coverage is calculated") {
          getTestSummary.map { result =>
            assertTrue(result.coverageCounts == CoverageCounts(
              covered = 2,
              uncovered = 0
            ))
          }
        }
      ),
      suite("Modules Correct")(
        test("Module One Counts") {
          moduleCounts("ExampleTests", "FailingModuleOne").map { counts =>
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
          moduleCounts("ExampleTests", "FailingModuleOne").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Failed))
          }
        },
        test("Module Two Counts") {
          moduleCounts("ExampleTests", "FailingModuleTwo").map { counts =>
            assertTrue(counts == Some(TestResultCounts(
              passed = 1,
              failed = 1,
              errors = 4,
              skipped = 0,
              todo = 0
            )))
          }
        },
        test("Failing isn't passing") {
          moduleCounts("ExampleTests", "FailingModuleTwo").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Failed))
          }
        },
        test("Incomplete Counts") {
          moduleCounts("ExampleTests", "IncompleteModule").map { counts =>
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
          moduleCounts("ExampleTests", "IncompleteModule").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Incomplete))
          }
        },
        test("Passing Counts") {
          moduleCounts("ExampleTests", "PassingModule").map { counts =>
            assertTrue(counts == Some(TestResultCounts(
              passed = 18,
              failed = 0,
              errors = 0,
              skipped = 0,
              todo = 0
            )))
          }
        },
        test("Passing Module Status") {
          moduleCounts("ExampleTests", "PassingModule").map { counts =>
            assertTrue(counts.map(_.result) == Some(OverallStatus.Passed))
          }
        },
        test("Coverage is calculated") {
          getTestSummary.map { result =>
            assertTrue(result.coverageCounts == CoverageCounts(
              covered = 2,
              uncovered = 0
            ))
          }
        }
      )
    ).provideLayerShared(failingTestSummaryLayer),
    suite("Passing Project Spec")(
      test("Project passed") {
        getTestSummary.map { result =>
          assertTrue(result.passed)
        }
      },
      test("Project was not incomplete") {
        getTestSummary.map { result =>
          assertTrue(!result.incomplete)
        }
      },
      test("Project coverage is calculated") {
        getTestSummary.map { result =>
          assertTrue(result.coverageCounts == CoverageCounts(
            covered = 1,
            uncovered = 1
          ))
        }
      }
    ).provideLayerShared(passingTestSummaryLayer),
    suite("Incomplete Project Spec")(
      test("Project did not pass") {
        getTestSummary.map { result =>
          assertTrue(!result.passed)
        }
      },
      test("Project was incomplete") {
        getTestSummary.map { result =>
          assertTrue(result.incomplete)
        }
      },
      test("Project coverage is calculated") {
        getTestSummary.map { result =>
          assertTrue(result.coverageCounts == CoverageCounts(
            covered = 1,
            uncovered = 1
          ))
        }
      }
    ).provideLayerShared(incompleteTestSummaryLayer)
  )
}
