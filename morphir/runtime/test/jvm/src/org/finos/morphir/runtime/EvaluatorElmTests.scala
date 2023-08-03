package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.{test, *}

object EvaluatorElmTests extends MorphirBaseSpec {
  lazy val lib =
    EvaluationLibrary("./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json", "Morphir.Examples.App")
  def runTest(moduleName: String, functionName: String) = lib.runTestDDL(moduleName, functionName, ())
  def spec =
    suite("EvaluatorElmTests")(
      suite("Constructor Tests")(
        test("Zero Arg") {
          val actual   = runTest("constructorTests", "constructorZeroArgTest")
          val expected = ("ZeroArg", List())
          assertTrue(actual == expected)
        },
        test("One Arg") {
          val actual =
            runTest("constructorTests", "constructorOneArgAppliedTest")
          val expected = ("OneArg", List(5))
          assertTrue(actual == expected)
        },
        test("Two Arg") {
          val actual =
            runTest("constructorTests", "constructorTwoArgAppliedTest")
          val expected = ("TwoArg", List(5, "Red"))
          assertTrue(actual == expected)
        },
        test("Two Arg Curried") {
          val actual =
            runTest("constructorTests", "constructorTwoArgCurriedTest")
          val expected = ("TwoArg", List(5, "Blue"))
          assertTrue(actual == expected)
        },
        test("Lazy Function") {
          val actual   = runTest("constructorTests", "lazyFunctionTest")
          val expected = (5, 5)
          assertTrue(actual == expected)
        }
      ),
      suite("Destructure Tests")(
        test("As") {
          val actual   = runTest("destructureTests", "destructureAsTest")
          val expected = 5
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("destructureTests", "destructureTupleTest")
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("destructureTests", "destructureConstructorTest")
          val expected = (5, "red")
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest("destructureTests", "destructureUnitTest")
          val expected = 4L
          assertTrue(actual == expected)
        },
        test("AsTwice") {
          val actual   = runTest("destructureTests", "destructureAsTwiceTest")
          val expected = (5L, 5L)
          assertTrue(actual == expected)
        },
        test("Tuple Twice") {
          val actual =
            runTest("destructureTests", "destructureTupleTwiceTest")
          val expected = ("Blue", 5L, (5L, "Blue"))
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest("destructureTests", "destructureDirectTest")
          val expected = (6L, "Green")
          assertTrue(actual == expected)
        }
      ),
      suite("IfThenElse Tests")(
        test("True Branch") {
          val actual   = runTest("ifThenElseTests", "ifThenElseTrueTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("False Branch") {
          val actual   = runTest("ifThenElseTests", "ifThenElseFalseTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Else Unevaluated") {
          val actual   = runTest("ifThenElseTests", "ifThenElseElseBranchUnevaluatedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Then Unevaluated") {
          val actual   = runTest("ifThenElseTests", "ifThenElseThenBranchUnevaluatedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        }
      ),
      suite("Lambda Tests")(
        test("As") {
          val actual   = runTest("lambdaTests", "lambdaAsTest")
          val expected = (5L, 5L)
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("lambdaTests", "lambdaTupleTest")
          val expected = (0L, 1L)
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("lambdaTests", "lambdaConstructorTest")
          val expected = ("Red", 5L)
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest("lambdaTests", "lambdaUnitTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest("lambdaTests", "lambdaDirectTest")
          val expected = (0L, 1L)
          assertTrue(actual == expected)
        },
        test("Scope") {
          val actual   = runTest("lambdaTests", "lambdaScopeTest")
          val expected = (3L, (4L, 5L))
          assertTrue(actual == expected)
        },
        test("Higher Order") {
          val actual   = runTest("lambdaTests", "lambdaHigherOrderTest")
          val expected = (3L, 4L, 5L)
          assertTrue(actual == expected)
        },
        test("User Defined Constructor") {
          val actual   = runTest("lambdaTests", "lambdaUserDefinedTest")
          val expected = (5L, "Red")
          assertTrue(actual == expected)
        }
      ),
      suite("Let Definition")(
        test("Make Tuple") {
          val actual   = runTest("letDefinitionTests", "letDefinitionMakeTupleTest")
          val expected = (1L, 1L)
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual   = runTest("letDefinitionTests", "letDefinitionNestedTest")
          val expected = (2L, 2L)
          assertTrue(actual == expected)
        },
        test("Simple Function") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionSimpleFunctionTest")
          val expected = (3L, 3L)
          assertTrue(actual == expected)
        },
        test("Two Argument Function") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionTwoArgFunctionFunctionTest")
          val expected = (3L, 2L)
          assertTrue(actual == expected)
        },
        test("Curried Function") {
          val actual   = runTest("letDefinitionTests", "letDefinitionCurriedTest")
          val expected = (2L, 0L)
          assertTrue(actual == expected)
        },
        test("Apply Twice") {
          val actual = runTest(
            "letDefinitionTests",
            "letDefinitionApplyTwiceTest"
          )
          val expected = ((1L, 0L), (2L, 0L))
          assertTrue(actual == expected)
        },
        test("Only runs if applied") {
          val actual   = runTest("letDefinitionTests", "letDefinitionDoNotRunTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Lexical cope") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionScopeTest")
          val expected = (3L, (4L, 5L))
          assertTrue(actual == expected)
        }
      ),
      suite("Let Recursion")(
        test("Fibbonacci") {
          val actual   = runTest("letRecursionTests", "letRecursionFibonacciTest")
          val expected = 34L
          assertTrue(actual == expected)
        },
        test("Mutual Recursion") {
          val actual   = runTest("letRecursionTests", "letRecursionMutualTest")
          val expected = (8L, 9L)
          assertTrue(actual == expected)
        }
      ),
      suite("Lists")(
        test("Empty") {
          val actual   = runTest("listTests", "listEmptyTest")
          val expected = List()
          assertTrue(actual == expected)
        },
        test("Single") {
          val actual   = runTest("listTests", "listSingleTest")
          val expected = List(0L)
          assertTrue(actual == expected)
        },
        test("Several") {
          val actual   = runTest("listTests", "listSeveralTest")
          val expected = List(0L, 1, 2, 3, 4, 5)
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual   = runTest("listTests", "listNestedTest")
          val expected = List(List("Red", "Blue"), List(), List("Car", "Plane", "Truck"))
          assertTrue(actual == expected)
        },
        test("Flatten") {
          val actual   = runTest("listTests", "listFlattenTest")
          val expected = List("Red", "Blue", "Car", "Plane", "Truck")
          assertTrue(actual == expected)
        }
      ),
      suite("Literals")(
        test("String") {
          val actual   = runTest("literalTests", "litStringTest")
          val expected = "Bloop"
          assertTrue(actual == expected)
        },
        test("Float") {
          val actual   = runTest("literalTests", "litFloatTest")
          val expected = 5.0
          assertTrue(actual == expected)
        },
        test("Char") {
          val actual   = runTest("literalTests", "litCharTest")
          val expected = 'f'
          assertTrue(actual == expected)
        },
        test("Boolean") {
          val actual   = runTest("literalTests", "litBoolTest")
          val expected = true
          assertTrue(actual == expected)
        },
        test("Whole Number") {
          val actual   = runTest("literalTests", "litWholeNumberLiteralTest")
          val expected = 5
          assertTrue(actual == expected)
        }
      ),
      suite("Native References")(
        test("Map") {
          val actual =
            runTest("nativeReferenceTests", "nativeReferenceMapTest")
          val expected = List((1L, 1L), (2L, 2L), (3L, 3L))
          assertTrue(actual == expected)
        },
        test("Add") {
          val actual   = runTest("nativeReferenceTests", "nativeReferenceAddTest")
          val expected = 3L
          assertTrue(actual == expected)
        },
        test("Curried Log") {
          val actual   = runTest("nativeReferenceTests", "nativeReferenceCurriedLogTest")
          val expected = Double.PositiveInfinity
          assertTrue(actual == expected)
        },
        test("Pi") {
          val actual   = runTest("nativeReferenceTests", "nativeReferencePiTest")
          val expected = 3
          assertTrue(actual == expected)
        }
      ),
      suite("Patern Matching")(
        test("Wildcard") {
          val actual   = runTest("patternMatchTests", "patternMatchWildcardTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("patternMatchTests", "patternMatchTupleTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("patternMatchTests", "patternMatchConstructorTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Zero Arg Constructor") {
          val actual   = runTest("patternMatchTests", "patternMatchEmptyListTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Head Tail") {
          val actual   = runTest("patternMatchTests", "patternMatchHeadTailTest")
          val expected = ("Dog", "Red")
          assertTrue(actual == expected)
        },
        test("Literal") {
          val actual   = runTest("patternMatchTests", "patternMatchLiteralTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Repeated As") {
          val actual =
            runTest("patternMatchTests", "patternMatchRepeatedAsTest")
          val expected = (2L, (1L, 2L))
          assertTrue(actual == expected)
        }
      ),
      suite("Records")(
        test("Field") {
          val actual   = runTest("recordTests", "recordFieldTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field from Bound Record") {
          val actual   = runTest("recordTests", "recordFieldFromBoundTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field Function Apply") {
          val actual   = runTest("recordTests", "fieldFunctionApplyTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field Function Apply Twice") {
          val actual   = runTest("recordTests", "fieldFunctionApplyTwiceTest")
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Field") {
          val actual   = runTest("recordTests", "fieldFunctionMapTest")
          val expected = List("Soso", "Ponyo", "Odin")
          assertTrue(actual == expected)
        },
        test("Simple Record") {
          val actual   = runTest("recordTests", "recordSimpleTest")
          val expected = Map("name" -> "Fido", "number" -> 5)
          assertTrue(actual == expected)
        },
        test("Nested Record") {
          val actual = runTest("recordTests", "recordNestedTest")
          val expected = Map(
            "name"    -> "Dogs",
            "records" -> List(Map("name" -> "Ponyo", "number" -> 3), Map("name" -> "Soso", "number" -> 3))
          )
          assertTrue(actual == expected)
        },
        test("Record Update Single Field") {
          val actual   = runTest("recordTests", "updateRecordSimpleTest")
          val expected = "Soso"
          assertTrue(actual == expected)
        },
        test("Record Update Full") {
          val actual   = runTest("recordTests", "updateRecordFullTest")
          val expected = Map("name" -> "Soso", "number" -> 5)
          assertTrue(actual == expected)
        },
        test("Record Updates are not mutation") {
          val actual   = runTest("recordTests", "updateRecordImmutableTest")
          val expected = List(Map("name" -> "Soso", "number" -> 4), Map("name" -> "Ponyo", "number" -> 5))
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Unit") {
          val actual   = runTest("simpleTests", "simpleUnitTest")
          val expected = ()
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Tuple(2)") {
          val actual   = runTest("tupleTests", "tupleTwoTest")
          val expected = (5L, 4L)
          assertTrue(actual == expected)
        },
        test("Tuple(3)") {
          val actual   = runTest("tupleTests", "tupleThreeTest")
          val expected = (0L, true, "Green")
          assertTrue(actual == expected)
        },
        test("Nested Tuple") {
          val actual   = runTest("tupleTests", "tupleNestedTest")
          val expected = (5L, ("Four", (4L, "Five")))
          assertTrue(actual == expected)
        }
      ),
      suite("References To user Defined Members")(
        test("Reference to value") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceValueTest")
          val expected = 5L
          assertTrue(actual == expected)
        },
        test("Curried Function Application") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceCurriedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Simple Function Application") {
          val actual = runTest(
            "userDefinedReferenceTests",
            "userDefinedReferenceSimpleFunctionTest"
          )
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Calling public function which calls private function") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferencePublicPrivateTest")
          val expected = 10L
          assertTrue(actual == expected)
        },
        test("Reference to Record") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceRecordTest")
          val expected = "Tom Tit Tot"
          assertTrue(actual == expected)
        },
        test("Reference to Union Type") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceUnionTest")
          val expected = -6L
          assertTrue(actual == expected)
        }
      )
    ) @@ TestAspect.ignore @@ TestAspect.tag("Will re-enable when code-gen of a test are part of the pipeline")
}
