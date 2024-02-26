package org.finos.morphir.runtime.quick

import org.finos.morphir.datamodel.{Concept, Data, EnumLabel, Label}
import org.finos.morphir.runtime.{Distributions, RTValue}
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.runtime.exports.*
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.services.sdk.MorphirSdk
import org.finos.morphir.runtime.TestSummary
import org.finos.morphir.runtime.SDKValue.SDKValueDefinition
import org.finos.morphir.ir.Value.*
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.internal.NativeFunctionAdapter
import org.finos.morphir.ir.sdk
import org.finos.morphir.runtime.SDKValue
import org.finos.morphir.runtime.TestTree
import org.finos.morphir.runtime.MorphirUnitTest
import org.finos.morphir.runtime.RTValue.Primitive
import org.finos.morphir.util.PrintRTValue

import org.finos.morphir.runtime.internal._

object UnitTestingSDK {
  def expectation(result: RTValue) =
    RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Expectation"), List(result))
  val passed = RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Pass"), List())
  def failed(msg: String) =
    RTValue.ConstructorResult(FQName.fromString("Morphir.UnitTest:Expect:Fail"), List(Primitive.String(msg)))

  val equal: SDKValue =
    SDKValue.SDKNativeFunction.fun2 { (a: RTValue, b: RTValue) =>
      val result = if (a == b) passed else failed(s"${PrintRTValue(a).plainText} != ${PrintRTValue(b).plainText}")
      expectation(result)
    }

}

object UnitTesting {

  // What should this method's types be?
  // Could return test failures on the error channel and have a unit return type
  // Or could return a test report, with no error (barring evaluation failures?)
  // Need to return a summary even on success, so not that first one
  // Seems messy to return a summary either way
  // Also, I would say "Testing" didn't fail even if the tests did
  def testType       = T.reference("Morphir.UnitTest", "Test", "Test")
  def testResultType = T.reference("Morphir.UnitTest", "Test", "TestResult")
  private[runtime] def runTests(
      globals: GlobalDefs,
      dists: Distributions
  ): RTAction[MorphirEnv, Nothing, TestSummary] =
    RTAction.environmentWithPure[MorphirSdk] { env =>
      val testNames = collectTests(globals, dists)
      val testVals  = testNames.map(fqn => Value.Reference.Typed(testType, fqn))
      if (testVals.isEmpty) {
        val emptySummary = TestSummary("No tests run", true)
        RTAction.succeed(emptySummary)
      } else {
        val testSuiteIR = if (testVals.length == 1)
          testVals.head
        else {
          val testList = V.list(sdk.List.listType(testType), testVals: _*)
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
          case _ => nonpassingResult()
        }

        // val newGlobals =
        //   globals.withDefinition(FQName.fromString("Morphir.UnitTest:Expect:equal"), UnitTestingSDK.equal)
        RTAction.succed(report)
      }
    }

  private[runtime] def passingResult(
      globals: GlobalDefs,
      dists: Distributions,
      runTestIR: TypedValue
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
    // Let's just eat the whole horse
    // val testSuiteRT = Loop(globals).loop(testSuiteIR, Store.empty)
    val testIRs: List[(FQName, TypedValue)] =
      testNames.map(fqn => (fqn, Value.Reference.Typed(testType, fqn)))

    def thunkify(value: TypedValue): Option[TypedValue] = None // Placeholder
    def thunkifTransform                                = transform(thunkify)
    val thunkifiedTests = testIRs.map { case (fqn, value) => (fqn -> thunkifyTransform(value)) }

    // Wait we want to RUN the expect function, but w/ a superprivileged SDK function replacing the test function
    // So that means that any call that looks like
    // (Apply(F, Arg) : Expect) //No wait this includes the wrong stuffs
    // Okay if the users are that determined to break test reporting they can, it won't change passes to fails
    // So that's
    // Match once and in this order:
    // Apply(Apply(Ref(OneOfThem), Arg1), Arg2) -> Apply(Ref(OneOfThem), () -> (Arg1, Arg2)) //Okay?
    // Apply(Reference(OneOfThem), Arg) -> Apply(Reference(OneOfThem), () -> Arg)
    // Tho TBH, which even need this?
    // Let's just say all of them. It's nice to be able to SEE the IR.

    // Transformation is hard tho - revisit that idea with greater patience.

    // Then There is...
    // Apply(Apply(Ref(OnFail)), Apply(...))
    // So I think OnFail we DO replace but we DO NOT convert, right? That sounds good.

    val testRTValues: List[(FQName, Either[Error, RTValue])] = thunkifiedTests
      .map { case (fqn, ir) =>
        try
          Right(Loop(globals).loop(ir, Store.empty))
        catch {
          case e => Left(e)
        }
      }

    // Try to convert these to actual Test trees
    val tests: List[Either[UnitTest, Error]] = testRTValues.map{
      case (fqn, Right(rt)) => (fqn, Right(TestTree.fromRTValue(rt)))
      case err => err
    }

    

    // Each test leaf contains a thunk
    // We evaluate the thunks, we get back:
    // Expects or
    // More Thunks (Re-evaluate once)
    // () -> Expect.Something(args)
    // () -> (() -> Expect.somethig(args)) //WAIT WRONG THAT'S A MIX OF TYPE AND VALUE
    // Deconvert first? Or what?
    // Umm... wait did we think this thru?
    // We have a number of
    // Replace all Apply(Apply(Expect.whatever)) w/ Lambda (() -> That Nonsense)
    // Evaluate again; failures caught as errors, not thrown
    //
  }

  private[runtime] def collectTests(
      globals: GlobalDefs,
      dists: Distributions
  ): List[FQName] = {
    val tests = globals.definitions.collect { // TODO: Improved test recognition (aliasing of return type, cleanup, ???)
      case (fqn -> SDKValueDefinition(definition: TypedDefinition))
          if (definition.inputTypes.isEmpty && definition.outputType == testType) =>
        fqn
    }.toList
    tests
  }

  def transform(partial: PartialFunction[TypedValue, TypedValue])(value: TypedValue): TypedValue = {
    def recurse = transform(partial)
    value match {
      case Apply(va, function, argument) => Apply(va, recurse(function), recurse(argument))
      case Destructure(va, pattern, valueToDestruct, inValue) =>
        Apply(va, pattern, recurse(valueToDestruct), recurse(inValue))
      case Field(va, recordValue, name) => Field(va, recurse(recordValue), name)
      case IfThenElse(va, condition, thenValue, elseValue) =>
        IfThenElse(va, recurse(condition), recurse(thenValue), recurse(elseValue))
      case Lambda(va, pattern, body) => Lambda(va, pattern, recurse(body))
      case LetDefinition(va, name, definition, inValue) =>
        LetDefinition(va, name, definition.copy(body = recurse(definition.body)), recurse(inValue))
      case LetRecursion(va, definitions, inValue) => Recursion(
          va,
          definitions.map(dfn => dfn.copy(body = recurse(dfn.body))),
          recurse(inValue)
        )
      case ListValue(va, elements) => ListValue(va, elements.map(recurse))
      case PatternMatch(va, value, cases) =>
        PatternMatch(va, recurse(value), cases.map((csePattern, caseValue) => (casePattern, recurse(caseValue))))
      case Record(va, fields) => Record(
          va,
          fields.map((fieldName, fieldValue) => (fieldName, recurse(fieldValue)))
        )
      case Tuple(va, elements) => Tuple(
          va,
          elements.map(recurse)
        )
      case UpdateRecord(va, valueToUpdate, fields) => UpdateRecord(
          va,
          recurse(valueToUpdate),
          fields.map((fieldName, fieldValue) => (fieldName, recurse(fieldValue)))
        )
    }
  }

}

//COPIED:

// package org.finos.morphir.runtime.quick

// private[runtime] def runTests(
//       globals: GlobalDefs,
//       dists: Distributions
//   ): TestResult = {
//     val testType = T.reference("Morphir.Testing", "Test", "Test")
//     val tests = globals.definitions.collect { // TODO: Improved test recognition (aliasing of return type, cleanup, ???)
//       case (fqn -> SDKValue.SDKValueDefinition(definition: RuntimeDefinition))
//           if (definition.inputTypes.isEmpty && definition.outputType == testType) =>
//         fqn
//     }.toList
//     val testResults = UnitTesting.runTests(globals, dists, "Example Test Suite", tests: _*)
//     testResults
//   }

// import org.finos.morphir.naming.*
// import org.finos.morphir.ir.{Type as T, Value as V}
// import org.finos.morphir.ir.Literal.Lit
// import org.finos.morphir.ir.Value.{
//   Pattern,
//   TypedValue,
//   Value,
//   TypedDefinition as TypedValueDef,
//   USpecification as UValueSpec
// }
// import org.finos.morphir.ir.Type.{Field, Type, UType, USpecification as UTypeSpec}
// import org.finos.morphir.runtime.{
//   RTValue,
//   SDKConstructor,
//   SDKValue,
//   Utils,
//   Extractors,
//   Distributions,
//   TestResult,
//   MorphirRuntimeError
// }
// import org.finos.morphir.ir.printing.PrintIR
// import Extractors.*

// object UnitTesting {
//   val testType = T.reference("Morphir.Testing", "Test", "Test")

//   def runTest(
//       globals: GlobalDefs,
//       dists: Distributions,
//       testName: FQName
//   ): TestResult = {
//     val refValue  = Value.Reference.Typed(testType, testName)
//     val loop      = Loop(globals)
//     val evaluated = loop.loop(refValue, Store.empty)
//     evaluated match {
//       case RTValue.ConstructorResult(FQString("Morphir.Testing:Test:assert"), List(RTValue.Primitive.Boolean(true))) =>
//         TestResult.Success(testName.toString)
//       case RTValue.ConstructorResult(FQString("Morphir.Testing:Test:assert"), List(RTValue.Primitive.Boolean(false))) =>
//         formatFailure(globals, dists, testName.toString, refValue)
//       case RTValue.ConstructorResult(otherName, _) =>
//         TestResult.Failure(testName.toString, s"Unrecognized constructor result: $otherName")
//       case other => TestResult.Failure(testName.toString, s"Unrecognized value: ${other}")

//     }
//   }

//   private def formatFailure(
//       globals: GlobalDefs,
//       dists: Distributions,
//       testName: String,
//       ir: TypedValue
//   ): TestResult.Failure =
//     ir match {
//       case Value.Reference(_, name) =>
//         val found = dists.lookupValueDefinition(name) match {
//           case Left(value)  => throw value.withContext("Definition not found while formatting error")
//           case Right(value) => value
//         }
//         formatFailure(globals, dists, testName, found.body)
//       case Value.Apply(
//             _,
//             Value.Constructor(_, FQString("Morphir.Testing:Test:assert")),
//             Value.Apply(_, Value.Apply(_, _, actual), expected)
//           ) =>
//         val loop              = Loop(globals)
//         val actualEvaluated   = loop.loop(actual, Store.empty)
//         val expectedEvaluated = loop.loop(expected, Store.empty)
//         TestResult.Failure(
//           testName,
//           s"""
//              |\t\t($actual != $expected)
//              |\t\t${PrintIR(actualEvaluated)} != ${PrintIR(expectedEvaluated)}""".stripMargin
//         )
//       case other => TestResult.Failure(testName, s"($ir) evaluated to false")
//     }

//   def runTests(
//       globals: GlobalDefs,
//       dists: Distributions,
//       suiteName: String,
//       testNames: FQName*
//   ): TestResult = {
//     val results = testNames.toList.map(runTest(globals, dists, _))
//     TestResult.Suite(suiteName, results)
//   }

// }
