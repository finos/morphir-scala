package org.finos.morphir.runtime.quick

import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.runtime.{Distributions, RTValue}
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import org.finos.morphir.runtime.TestSummary
import org.finos.morphir.ir.Value.*
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.ir.sdk
import org.finos.morphir.runtime.SDKValue
import org.finos.morphir.runtime.EvaluationLibrary
import org.finos.morphir.runtime.SDKValue.SDKValueDefinition
import org.finos.morphir.runtime.quick.TestTree
import org.finos.morphir.runtime.SingleTestTree[SingleTestResult]
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
import org.finos.morphir.ir.distribution.Distribution

import org.finos.morphir.runtime.internal._

object UnitTesting {

  def testFrameworkPath =
    "elm-sdks/morphir-unit-test/morphir-ir.json"
  def testType        = T.reference("Morphir.UnitTest", "Test", "Test")
  def testResultType  = T.reference("Morphir.UnitTest", "Test", "TestTree[SingleTestResult]")
  def expectationType = T.reference("Morphir.UnitTest", "Expect", "Expectation")
  def testPrefix      = "Morphir.UnitTest:Test:"
  def expectPrefix    = "Morphir.UnitTest:Expect:"

  private[runtime] def runTests(
      userDists: Distributions
  ): RTAction[MorphirEnv, Nothing, TestSummary] =
    val testLibs = Distribution.toLibsMap(EvaluationLibrary.loadDistribution(testFrameworkPath))
    val userLibs = userDists.getDists
    val dists    = Distributions(userLibs ++ testLibs)
    val globals  = GlobalDefs.fromDistributions(dists)
    RTAction.environmentWithPure[MorphirSdk] { env =>
      val testNames = collectTests(globals, dists)
      val testIRs   = testNames.map(fqn => Value.Reference.Typed(testType, fqn))
      if (testIRs.isEmpty) {
        val emptySummary = TestSummary("No tests run", true)
        RTAction.succeed(emptySummary)
      } else {
        val testSuiteIR = if (testIRs.length == 1)
          testIRs.head
        else {
          val testList = V.list(sdk.List.listType(testType), testIRs: _*)
          V.applyInferType(
            testType,
            V.constructor(FQName.fromString("Morphir.UnitTest:Test:Concat")),
            testList
          )
        }

        val runTestsIR = V.applyInferType(
          testResultType,
          V.reference(FQName.fromString("Morphir.UnitTest:Test:run")),
          testSuiteIR
        )
        val testsPassedIR = V.applyInferType(
          sdk.Basics.boolType,
          V.reference(FQName.fromString("Morphir.UnitTest:Test:passed")),
          runTestsIR
        )

        // We evaluate twice (in failed cases) intentionally - once in a pure-elm manner to ensure we are using
        // the in-flight evaluator, then again with special code enabled to better analyze and report the failures.
        // The first assures us we have no "false nagative" (i.e., falsely passing) tests
        // The second lets us give the user lots of information about why and how tests failed
        val passedResult =
          try
            Right(EvaluatorQuick.eval(testsPassedIR, globals, dists))
          catch {
            case e => Left(e)
          }

        val report = passedResult match {
          case Right(Data.Boolean(true)) => passingResult(globals, dists, runTestsIR)
          // Anything else and we know we have a failure, it's just a matter of determining what
          case _ => nonPassingResult(globals, dists, testNames)
        }

        RTAction.succeed(report)
      }
    }

  private[runtime] def passingResult(
      globals: GlobalDefs,
      dists: Distributions,
      runTestsIR: TypedValue
  ): TestSummary = {
    val reportIR = V.applyInferType(
      sdk.String.stringType,
      V.reference(FQName.fromString("Morphir.UnitTest:Test:resultToString")),
      runTestsIR
    )
    val mdmReport = EvaluatorQuick.eval(reportIR, globals, dists)
    val report = mdmReport match {
      case Data.String(s) => s
      case _              => throw new OtherError("Test result: ", mdmReport)
    }
    TestSummary(report, true)
  }

  private[runtime] def nonPassingResult(
      globals: GlobalDefs,
      dists: Distributions,
      testNames: List[FQName]
  ): TestSummary = {

    // We rewrite the IR to replace expect calls (in common patterns) with thunky versions
    def thunkifyTransform =
      TypedValue.transform(MorphirExpect.thunkifyAll)
    val newGlobalDefs = globals.definitions.map {
      case (fqn, SDKValue.SDKValueDefinition(dfn)) =>
        (fqn, SDKValue.SDKValueDefinition(dfn.copy(body = thunkifyTransform(dfn.body))))
      case other => other
    }
    // ... and we replace the elm Expect functions with more privileged native ones
    val newGlobals = globals
      .copy(definitions = newGlobalDefs)
      .withBindingsFrom(MorphirExpect.newDefs)

    // Make IRs for our tests
    val testIRs: List[(FQName, TypedValue)] =
      testNames.map(fqn => (fqn, Value.Reference.Typed(testType, fqn)))

    // Then we try to run them, and catch any errors that we encounter
    val testRTValues: List[(FQName, Either[Throwable, RTValue])] = testIRs
      .map { case (fqn, ir) =>
        try
          (fqn, Right(Loop(newGlobals).loop(ir, Store.empty)))
        catch {
          case e => (fqn, Left(e))
        }
      }

    // We make this into a test tree, using FQNs for things not already described
    // val byPackage = testRTValues.groupBy((_._1.getPackagePath))

    val testTree: TestSet[RT] =
      TestSet(
        testRTValues
          .groupBy { case (fqn, _) => (fqn.getPackagePath, fqn.getModulePath) }
          .map { case ((packagePath, modulePath), tests) =>
            ModuleTests(
              s"$packagePath:$modulePath",
              tests.map {
                case (fqn, Left(err)) => TestTree.Error(fqn.localName.toCamelCase, err)
                case (fqn, Right(rt)) =>
                  TestTree.fromRTValue(rt) match {
                    case d: TestTree.Describe[_]   => d
                    case s: TestTree.SingleTest[_] => s
                    case other                     => TestTree.Describe(fqn.localName.toCamelCase, List(other))
                  }
              }
            )
          }.toList
      ).resolveOnly // "Only" requires special handling, so do that here

    // Recursive walk of tree, running the user-defined thunks in the "test" code
    // Non-introspected tests are "Run" at this point
    val withExpects = TestTree.getExpects(newGlobals)(testTree)
    // Another walk of the tree, running introspected tests this time
    val treeWithResults = TestTree.processExpects(newGlobals)(withExpects)
    TestSummary(TestTree.toReport(treeWithResults), false)
  }

  private[runtime] def collectTests(
      globals: GlobalDefs,
      dists: Distributions
  ): List[FQName] = {
    val tests = globals.definitions.collect {
      case (fqn -> SDKValueDefinition(definition: TypedDefinition))
          if (definition.inputTypes.isEmpty && definition.outputType == testType) =>
        fqn
    }.toList
    tests
  }

}
