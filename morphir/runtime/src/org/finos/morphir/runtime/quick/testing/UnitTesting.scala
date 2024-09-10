package org.finos.morphir.runtime.quick.testing

import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.runtime.{Distributions, RTValue}
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import org.finos.morphir.ir.Value.*
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.ir.sdk
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.*
import org.finos.morphir.runtime.quick.*
import org.finos.morphir.runtime.SDKValue.SDKValueDefinition
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.Extractors.Values.ApplyChain
import org.finos.morphir.ir.distribution.Distribution

import org.finos.morphir.runtime.internal._

object UnitTesting {

  def testType        = T.reference("Morphir.UnitTest", "Test", "Test")
  def testResultType  = T.reference("Morphir.UnitTest", "Test", "TestTree[SingleTestResult]")
  def expectationType = T.reference("Morphir.UnitTest", "Expect", "Expectation")
  def testPrefix      = "Morphir.UnitTest:Test:"
  def expectPrefix    = "Morphir.UnitTest:Expect:"
  def testPackagePath = fqn"Morphir.UnitTest:Test:foo".packagePath

  def containsTestCode(
      fqn: FQName,
      definition: TypedDefinition
  ): Boolean =
    fqn.packagePath == testPackagePath || definition.outputType == testType

  /**
   * Run all tests in the given distributions. By this point, the distributions need to include the actual test
   * framework (as this is a ZPure, it should not touch file handling) This function works in two passes - first it uses
   * a minimal pure-elm evaluation pass, ensuring we are testing like we fly (and not hiding user bugs behind test
   * framework bugs) Then it uses a more complex and priviliged pass to give us a neatly formatted report of the
   * results. If the complex pass disagree with the simple pass, we messed something up and an error is thrown.
   */
  private[runtime] def runTests(
      dists: Distributions
  ): RTAction[MorphirEnv, Nothing, TestSummary] = {
    val globals = GlobalDefs.fromDistributions(dists)
    RTAction.environmentWithPure[MorphirSdk] { env =>
      val testNames            = collectTests(globals)
      val userDefinedFunctions = collectNonTests(globals)
      val testIRs              = testNames.map(fqn => Value.Reference.Typed(testType, fqn))
      if (testIRs.isEmpty) {
        val emptySummary = TestSummary(
          "No tests run",
          Map(),
          CoverageInfo(Set.empty[FQName], userDefinedFunctions, CoverageCounts.empty)
        )
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

        // We evaluate twice intentionally - once in a pure-morphir manner to ensure we are using
        // the in-flight evaluator, then again with special code enabled to better analyze and report the failures.
        // The first assures us we have no "false nagative" (i.e., falsely passing) tests
        // The second lets us give the user lots of information about why and how tests failed
        val passedResult =
          try
            Right(EvaluatorQuick.eval(testsPassedIR, globals, dists))
          catch {
            case e: Throwable => Left(e)
          }

        val simplePassed = passedResult match {
          case Right(Data.Boolean(true)) => true
          // Anything else and we know we have a failure, it's just a matter of determining what (error or failure are all the same to us at this point)
          case _ => false
        }

        val detailedReport = reportResult(globals, dists, testNames)

        if (detailedReport.passed == simplePassed)
          RTAction.succeed(detailedReport)
        // If the results were different, something went wrong in our test framework - it either hid an error, or created one that didn't exist in the simple pass
        else if (detailedReport.passed && (!simplePassed))
          throw InvalidState(
            s"""Detailed Test Report passed, but simple morphir-based testing failed.
          Detailed:  $detailedReport""",
            location = None
          )
        else
          throw InvalidState(
            s"""Detailed Test Report found failures, but simple morphir-based testing passed.
          Detailed:  $detailedReport""",
            location = None
          )
      }
    }
  }

  /**
   * Takes the discovered test names and does all the back-end magic to produce a report of the results
   *
   * @param globals
   *   The global definitions to use (without the magic functions from Expect)
   * @param dists
   *   The distributions to use, including the test framework
   * @param testNames
   *   The set of FQNs to evaluate as tests
   */
  private[runtime] def reportResult(
      globals: GlobalDefs,
      dists: Distributions,
      testNames: List[FQName]
  ): TestSummary = {
    // We rewrite the IR to replace expect calls (in common patterns) with thunky versions.
    // Specifics are up to the logic in Expect, but in general, `Expect.foo x y` becomes `\() -> Expect.foo x y`
    // Since expects are already usually in thunks, this mostly means that
    //  Test.test "testName" \_ -> Expect.foo x y
    // becoems
    //  Test.test "testName" \_ -> (\_ -> Expect.foo x y)
    // This lets us run any user logic used in the thunk (which may be sustantial), but still recover the actual IR of the expect call for introspection
    def thunkifyTransform =
      TypedValue.transform(Expect.thunkifyAll)
    val newGlobalDefs = globals.definitions.map {
      case (fqn, SDKValue.SDKValueDefinition(dfn)) =>
        (fqn, SDKValue.SDKValueDefinition(dfn.copy(body = thunkifyTransform(dfn.body))))
      case other => other
    }
    // ... and we replace the elm Expect functions with more privileged native ones
    val newGlobals = globals
      .copy(definitions = newGlobalDefs)
      .withBindingsFrom(Expect.newDefs)

    // Make IRs for our tests
    val testIRs: List[(FQName, TypedValue)] =
      testNames.map(fqn => (fqn, Value.Reference.Typed(testType, fqn)))

    // Then we try to evalute them, and catch any errors that we encounter
    // Note that this just evaluated the test definitions - it doesn't run even the user-defined thunks, yet
    val testRTValues: List[(FQName, Either[Throwable, RTValue])] = testIRs
      .map { case (fqn, ir) =>
        try
          (fqn, Right(Loop(newGlobals).loop(ir, Store.empty, CodeLocation.EntryPoint)))
        catch {
          case e: Throwable => (fqn, Left(e))
        }
      }

    // We make this into a test set
    // Tests are grouped by the module they belong to
    // Top-level tests without a name are given a name from their FQN
    // TODO: Preserve the FQName either way, for better tooling integration (code coverage?)
    val testSet: TestSet[RT] =
      TestSet[RT](
        testRTValues
          .groupBy { case (fqn, _) => (fqn.pack, fqn.getModuleName) }
          .map { case ((pkgName, modName), tests) =>
            ModuleTests[RT](
              pkgName,
              modName,
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
    // User-defined thunks, and non-introspected expect calls (the magic SDK functions) are run here
    val withExpects = TestSet.getExpects(newGlobals, testSet)
    // And then the generated thunks are introspected, giving us our final SingleTestResults
    val withResults = TestSet.processExpects(newGlobals, withExpects)

    // begin collecting coverage related information
    val userDefinedFunctions = collectNonTests(globals)
    val referencedFunctions  = testNames.flatMap(name => GlobalDefs.getStaticallyReachable(name, globals)).toSet
    val coverageInfo = CoverageInfo(
      referencedFunctions,
      userDefinedFunctions,
      CoverageCounts.getCounts(userDefinedFunctions, referencedFunctions)
    )

    TestSet.toSummary(coverageInfo, withResults)
  }

  /**
   * Collects all tests from the given distributions. A "Test" is any top-level definition of type `Test`. This exludes:
   *   - Functions that result in Tests
   *   - Lists, maps or tuples of Tests
   *   - Things declared as an alias to type Test (intentional omission, in case users really want to hide tests for
   *     whatever reason)
   *
   * @param globals
   *   The global definitions to collect tests from
   */
  private[runtime] def collectTests(
      globals: GlobalDefs
  ): List[FQName] = {
    val tests = globals.definitions.collect {
      case (fqn -> SDKValueDefinition(definition: TypedDefinition))
          if (definition.inputTypes.isEmpty && definition.outputType == testType) =>
        fqn
    }.toList
    tests
  }

  /**
   * The inverse of collect Tests - returns non test definitions
   *
   * @param globals
   *   The global definitions to collect non tests from
   */
  private[runtime] def collectNonTests(
      globals: GlobalDefs
  ): Set[FQName] =
    globals.definitions.collect {
      case (fqn -> SDKValue.SDKValueDefinition(definition: TypedDefinition))
          if !containsTestCode(fqn, definition) => fqn
    }.toSet
}
