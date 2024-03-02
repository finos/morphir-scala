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
import org.finos.morphir.runtime.SDKValue.SDKValueDefinition
import org.finos.morphir.runtime.quick.TestTree
import org.finos.morphir.runtime.SingleTestResult
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.runtime.RTValue as RT
import org.finos.morphir.util.PrintRTValue
import org.finos.morphir.ir.printing.PrintIR
import org.finos.morphir.runtime.Extractors.{FQString, FQStringTitleCase}
import org.finos.morphir.runtime.Extractors.Values.ApplyChain

import org.finos.morphir.runtime.internal._

// object UnitTestingSDK {
//   def expectation(result: RTValue) =
//     RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
//   val passed = RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
//   def failed(msg: String) =
//     RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(Primitive.String(msg)))

//   val equal: SDKValue =
//     SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
//       val result = if (a == b) passed else failed(s"${PrintRTValue(a).plainText} != ${PrintRTValue(b).plainText}")
//       expectation(result)
//     }

//   val greaterThan = DynamicNativeFunction2("greaterThan") {
//     (_: NativeContext) => (a: Comparable, b: Comparable) =>
//       val res = if (RTValue.Comparable.compareOrThrow(a, b) > 0)
//         passed
//       else failed(s"${PrintRTValue(a).plainText} was not greater than ${PrintRTValue(b).plainText}")
//       expectation(res)
//   }

//   def extract(f: RTValue.Function, ctx: NativeContext): (TypedValue, TypedValue, RTValue, RTValue) = {
//     val out = ctx.evaluator.handleApplyResult(T.unit, f, RTValue.Unit())
//     val (ir1, ir2) = f match {
//       case RT.LambdaFunction(Value.Tuple(_, elements), _, _) => (elements(0), elements(1))
//       case other                                             => throw OtherError("This should not be!", other)
//     }
//     val (rt1, rt2) = out match {
//       case RT.Tuple(List(rt1_, rt2_)) => (rt1_, rt2_)
//       case other                      => throw new Exception("This should not be!")
//     }
//     (ir1, ir2, rt1, rt2)
//   }
//   val equalIntrospected = DynamicNativeFunction1("equalIntrospected") {
//     (ctx: NativeContext) => (f: RTValue.Function) =>
//       {
//         val (ir1, ir2, rt1, rt2)   = extract(f, ctx)
//         val (irString1, irString2) = (ir1.toString, ir2.toString)
//         val (rtString1, rtString2) = (PrintRTValue(rt1).plainText, PrintRTValue(rt2).plainText)
//         val res = if (rt1 != rt2)
//           failed(s"($irString1 => $rtString1) != ($irString2 => $rtString2")
//         else passed
//         expectation(res)
//       }
//   }
//   val notEqualIntrospected = DynamicNativeFunction1("notEqualIntrospected") {
//     (ctx: NativeContext) => (f: RTValue.Function) =>
//       {
//         val (ir1, ir2, rt1, rt2)   = extract(f, ctx)
//         val (rtString1, rtString2) = (PrintRTValue(rt1).plainText, PrintRTValue(rt2).plainText)
//         val res = if (rt1 == rt2)
//           failed(s"($ir1 => $rtString1) == ($ir2 => $rtString2")
//         else passed
//         expectation(res)
//       }
//   }

//   val newDefs = GlobalDefs(
//     Map(
//       FQName.fromString("Morphir.UnitTest:Expect:greaterThan") -> NativeFunctionAdapter.Fun2(
//         greaterThan
//       ).realize,
//       FQName.fromString("Morphir.UnitTest:Expect:equalIntrospected") -> NativeFunctionAdapter.Fun1(
//         equalIntrospected
//       ).realize,
//       FQName.fromString("Morphir.UnitTest:Expect:notEqualIntrospected") -> NativeFunctionAdapter.Fun1(
//         notEqualIntrospected
//       ).realize
//     ),
//     Map()
//   )

// }

object UnitTesting {

  def testType        = T.reference("Morphir.UnitTest", "Test", "Test")
  def testResultType  = T.reference("Morphir.UnitTest", "Test", "TestResult")
  def expectationType = T.reference("Morphir.UnitTest", "Expect", "Expectation")
  def testPrefix      = "Morphir.UnitTest:Test:"
  def expectPrefix    = "Morphir.UnitTest:Expect:"

  private[runtime] def runTests(
      globals: GlobalDefs,
      dists: Distributions
  ): RTAction[MorphirEnv, Nothing, TestSummary] =
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

    val testTree: MorphirUnitTest =
      TestTree.Concat(
        testRTValues
          .groupBy { case (fqn, _) => (fqn.getPackagePath, fqn.getModulePath) }
          .map { case ((packagePath, modulePath), tests) =>
            TestTree.Describe(
              s"$packagePath:$modulePath",
              tests.map {
                case (fqn, Left(err)) => TestTree.Error(fqn.toString, err)
                case (fqn, Right(rt)) =>
                  TestTree.fromRTValue(rt) match {
                    case d: TestTree.Describe[_]   => d
                    case s: TestTree.SingleTest[_] => s
                    case other                     => TestTree.Describe(fqn.toString, List(other))
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
