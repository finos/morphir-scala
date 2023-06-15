package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import EvaluatorJsonHelpers.*
import zio.test.*

object EvaluatorElmTests extends MorphirBaseSpec {
  def spec =
    suite("Json Evaluation")(
      suite("Constructor Tests")(
        test("Zero Arg") {
          val actual   = runTest[(String, List[Any])]("constructorTests", "constructorTests", "constructorZeroArgTest")
          val expected = ("ZeroArg", List())
          assertTrue(actual == expected)
        },
        test("One Arg") {
          val actual =
            runTest[(String, List[Any])]("constructorTests", "constructorTests", "constructorOneArgAppliedTest")
          val expected = ("OneArg", List(5))
          assertTrue(actual == expected)
        },
        test("Two Arg") {
          val actual =
            runTest[(String, List[Any])]("constructorTests", "constructorTests", "constructorTwoArgAppliedTest")
          val expected = ("TwoArg", List(5, "Red"))
          assertTrue(actual == expected)
        },
        test("Two Arg Curried") {
          val actual =
            runTest[(String, List[Any])]("constructorTests", "constructorTests", "constructorTwoArgCurriedTest")
          val expected = ("TwoArg", List(5, "Blue"))
          assertTrue(actual == expected)
        },
        test("Lazy Function") {
          val actual   = runTest[(Int, Int)]("constructorTests", "constructorTests", "lazyFunctionTest")
          val expected = (5, 5)
          assertTrue(actual == expected)
        }
      ),
      suite("Destructure Tests")(
        test("As") {
          val actual   = runTest[Long]("destructureTests", "destructureTests", "destructureAsTest")
          val expected = 5
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest[(Long, Long)]("destructureTests", "destructureTests", "destructureTupleTest")
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest[(Int, String)]("destructureTests", "destructureTests", "destructureConstructorTest")
          val expected = (5, "red")
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest[Long]("destructureTests", "destructureTests", "destructureUnitTest")
          val expected = 4L
          assertTrue(actual == expected)
        },
        test("AsTwice") {
          val actual   = runTest[(Long, Long)]("destructureTests", "destructureTests", "destructureAsTwiceTest")
          val expected = (5L, 5L)
          assertTrue(actual == expected)
        },
        test("Tuple Twice") {
          val actual =
            runTest[(String, Long, (Long, String))]("destructureTests", "destructureTests", "destructureTupleTwiceTest")
          val expected = ("Blue", 5L, (5L, "Blue"))
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest[(Long, String)]("destructureTests", "destructureTests", "destructureDirectTest")
          val expected = (6L, "Green")
          assertTrue(actual == expected)
        }
      ),
      suite("IfThenElse Tests")(
        test("True Branch") {
          val actual   = runTest[String]("ifThenElse", "ifThenElseTests", "ifThenElseTrueTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("False Branch") {
          val actual   = runTest[String]("ifThenElse", "ifThenElseTests", "ifThenElseFalseTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Else Unevaluated") {
          val actual   = runTest[String]("ifThenElse", "ifThenElseTests", "ifThenElseElseBranchUnevaluatedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Then Unevaluated") {
          val actual   = runTest[String]("ifThenElse", "ifThenElseTests", "ifThenElseThenBranchUnevaluatedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        }
      ),
      suite("Lambda Tests")(
        test("As") {
          val actual   = runTest[(Long, Long)]("lambdaTests", "lambdaTests", "lambdaAsTest")
          val expected = (5L, 5L)
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest[(Long, Long)]("lambdaTests", "lambdaTests", "lambdaTupleTest")
          val expected = (0L, 1L)
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest[(String, Long)]("lambdaTests", "lambdaTests", "lambdaConstructorTest")
          val expected = ("Red", 5L)
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest[String]("lambdaTests", "lambdaTests", "lambdaUnitTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest[(Long, Long)]("lambdaTests", "lambdaTests", "lambdaDirectTest")
          val expected = (0L, 1L)
          assertTrue(actual == expected)
        },
        test("Scope") {
          val actual   = runTest[(Long, (Long, Long))]("lambdaTests", "lambdaTests", "lambdaScopeTest")
          val expected = (3L, (4L, 5L))
          assertTrue(actual == expected)
        },
        test("Higher Order") {
          val actual   = runTest[(Long, Long, Long)]("lambdaTests", "lambdaTests", "lambdaHigherOrderTest")
          val expected = (3L, 4L, 5L)
          assertTrue(actual == expected)
        },
        test("User Defined Constructor") {
          val actual   = runTest[(Long, String)]("lambdaTests", "lambdaTests", "lambdaUserDefinedTest")
          val expected = (5L, "Red")
          assertTrue(actual == expected)
        }
      ),
      suite("Let Definition")(
        test("Make Tuple") {
          val actual   = runTest[(Long, Long)]("letDefinitionTests", "letDefinitionTests", "letDefinitionMakeTupleTest")
          val expected = (1L, 1L)
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual   = runTest[(Long, Long)]("letDefinitionTests", "letDefinitionTests", "letDefinitionNestedTest")
          val expected = (2L, 2L)
          assertTrue(actual == expected)
        },
        test("Simple Function") {
          val actual =
            runTest[(Long, Long)]("letDefinitionTests", "letDefinitionTests", "letDefinitionSimpleFunctionTest")
          val expected = (3L, 3L)
          assertTrue(actual == expected)
        },
        test("Two Argument Function") {
          val actual =
            runTest[(Long, Long)]("letDefinitionTests", "letDefinitionTests", "letDefinitionTwoArgFunctionFunctionTest")
          val expected = (3L, 2L)
          assertTrue(actual == expected)
        },
        test("Curried Function") {
          val actual   = runTest[(Long, Long)]("letDefinitionTests", "letDefinitionTests", "letDefinitionCurriedTest")
          val expected = (2L, 0L)
          assertTrue(actual == expected)
        },
        test("Apply Twice") {
          val actual = runTest[((Long, Long), (Long, Long))](
            "letDefinitionTests",
            "letDefinitionTests",
            "letDefinitionApplyTwiceTest"
          )
          val expected = ((1L, 0L), (2L, 0L))
          assertTrue(actual == expected)
        },
        test("Only runs if applied") {
          val actual   = runTest[String]("letDefinitionTests", "letDefinitionTests", "letDefinitionDoNotRunTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Lexical cope") {
          val actual =
            runTest[(Long, (Long, Long))]("letDefinitionTests", "letDefinitionTests", "letDefinitionScopeTest")
          val expected = (3L, (4L, 5L))
          assertTrue(actual == expected)
        }
      ),
      suite("Let Recursion")(
        test("Fibbonacci") {
          val actual   = runTest[Long]("letRecursionTests", "letRecursionTests", "letRecursionFibonacciTest")
          val expected = 34L
          assertTrue(actual == expected)
        },
        test("Mutual Recursion") {
          val actual   = runTest[(Long, Long)]("letRecursionTests", "letRecursionTests", "letRecursionMutualTest")
          val expected = (8L, 9L)
          assertTrue(actual == expected)
        }
      ),
      suite("Lists")(
        test("Empty") {
          val actual   = runTest[List[Long]]("listTests", "listTests", "listEmptyTest")
          val expected = List()
          assertTrue(actual == expected)
        },
        test("Single") {
          val actual   = runTest[List[Long]]("listTests", "listTests", "listSingleTest")
          val expected = List(0L)
          assertTrue(actual == expected)
        },
        test("Several") {
          val actual   = runTest[List[Long]]("listTests", "listTests", "listSeveralTest")
          val expected = List(0L, 1, 2, 3, 4, 5)
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual   = runTest[List[List[String]]]("listTests", "listTests", "listNestedTest")
          val expected = List(List("Red", "Blue"), List(), List("Car", "Plane", "Truck"))
          assertTrue(actual == expected)
        },
        test("Flatten") {
          val actual   = runTest[List[String]]("listTests", "listTests", "listFlattenTest")
          val expected = List("Red", "Blue", "Car", "Plane", "Truck")
          assertTrue(actual == expected)
        }
      ),
      suite("Literals")(
        test("String") {
          val actual   = runTest[String]("literalTests", "literalTests", "litStringTest")
          val expected = "Bloop"
          assertTrue(actual == expected)
        },
        test("Float") {
          val actual   = runTest[Double]("literalTests", "literalTests", "litFloatTest")
          val expected = 5.0
          assertTrue(actual == expected)
        },
        test("Char") {
          val actual   = runTest[Char]("literalTests", "literalTests", "litCharTest")
          val expected = 'f'
          assertTrue(actual == expected)
        },
        test("Boolean") {
          val actual   = runTest[Boolean]("literalTests", "literalTests", "litBoolTest")
          val expected = true
          assertTrue(actual == expected)
        },
        test("Whole Number") {
          val actual   = runTest[Long]("literalTests", "literalTests", "litWholeNumberLiteralTest")
          val expected = 5
          assertTrue(actual == expected)
        }
      ),
      suite("Native References")(
        test("Map") {
          val actual =
            runTest[List[(Long, Long)]]("nativeReferenceTests", "nativeReferenceTests", "nativeReferenceMapTest")
          val expected = List((1L, 1L), (2L, 2L), (3L, 3L))
          assertTrue(actual == expected)
        },
        test("Add") {
          val actual   = runTest[Long]("nativeReferenceTests", "nativeReferenceTests", "nativeReferenceAddTest")
          val expected = 3L
          assertTrue(actual == expected)
        },
        test("Curried Log") {
          val actual = runTest[Double]("nativeReferenceTests", "nativeReferenceTests", "nativeReferenceCurriedLogTest")
          val expected = Double.PositiveInfinity
          assertTrue(actual == expected)
        },
        test("Pi") {
          val actual   = runTest[Long]("nativeReferenceTests", "nativeReferenceTests", "nativeReferencePiTest")
          val expected = 3
          assertTrue(actual == expected)
        }
      ),
      suite("Patern Matching")(
        test("Wildcard") {
          val actual   = runTest[String]("patternMatchTests", "patternMatchTests", "patternMatchWildcardTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest[String]("patternMatchTests", "patternMatchTests", "patternMatchTupleTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest[String]("patternMatchTests", "patternMatchTests", "patternMatchConstructorTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Zero Arg Constructor") {
          val actual   = runTest[String]("patternMatchTests", "patternMatchTests", "patternMatchEmptyListTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Head Tail") {
          val actual   = runTest[(String, String)]("patternMatchTests", "patternMatchTests", "patternMatchHeadTailTest")
          val expected = ("Dog", "Red")
          assertTrue(actual == expected)
        },
        test("Literal") {
          val actual   = runTest[String]("patternMatchTests", "patternMatchTests", "patternMatchLiteralTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Repeated As") {
          val actual =
            runTest[(Long, (Long, Long))]("patternMatchTests", "patternMatchTests", "patternMatchRepeatedAsTest")
          val expected = (2L, (1L, 2L))
          assertTrue(actual == expected)
        }
      ),
      suite("Records")(
        test("Field") {
          val actual   = runTest[String]("recordTests", "recordTests", "recordFieldTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field from Bound Record") {
          val actual   = runTest[String]("recordTests", "recordTests", "recordFieldFromBoundTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field Function Apply") {
          val actual   = runTest[String]("recordTests", "recordTests", "fieldFunctionApplyTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Field Function Apply Twice") {
          val actual   = runTest[(Long, Long)]("recordTests", "recordTests", "fieldFunctionApplyTwiceTest")
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Field") {
          val actual   = runTest[List[String]]("recordTests", "recordTests", "fieldFunctionMapTest")
          val expected = List("Soso", "Ponyo", "Odin")
          assertTrue(actual == expected)
        },
        test("Simple Record") {
          val actual   = runTest[Map[String, Any]]("recordTests", "recordTests", "recordSimpleTest")
          val expected = Map("name" -> "Fido", "number" -> 5)
          assertTrue(actual == expected)
        },
        test("Nested Record") {
          val actual = runTest[Map[String, Any]]("recordTests", "recordTests", "recordNestedTest")
          val expected = Map(
            "name"    -> "Dogs",
            "records" -> List(Map("name" -> "Ponyo", "number" -> 3), Map("name" -> "Soso", "number" -> 3))
          )
          assertTrue(actual == expected)
        },
        test("Record Update Single Field") {
          val actual   = runTest[String]("recordTests", "recordTests", "updateRecordSimpleTest")
          val expected = "Soso"
          assertTrue(actual == expected)
        },
        test("Record Update Full") {
          val actual   = runTest[Map[String, Any]]("recordTests", "recordTests", "updateRecordFullTest")
          val expected = Map("name" -> "Soso", "number" -> 5)
          assertTrue(actual == expected)
        },
        test("Record Updates are not mutation") {
          val actual   = runTest[List[Map[String, Any]]]("recordTests", "recordTests", "updateRecordImmutableTest")
          val expected = List(Map("name" -> "Soso", "number" -> 4), Map("name" -> "Ponyo", "number" -> 5))
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Unit") {
          val actual   = runTest[Unit]("simpleAndTupleTests", "simpleTests", "simpleUnitTest")
          val expected = ()
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Tuple(2)") {
          val actual   = runTest[(Long, Long)]("simpleAndTupleTests", "tupleTests", "tupleTwoTest")
          val expected = (5L, 4L)
          assertTrue(actual == expected)
        },
        test("Tuple(3)") {
          val actual   = runTest[(Long, Boolean, String)]("simpleAndTupleTests", "tupleTests", "tupleThreeTest")
          val expected = (0L, true, "Green")
          assertTrue(actual == expected)
        },
        test("Nested Tuple") {
          val actual = runTest[(Long, (String, (Long, String)))]("simpleAndTupleTests", "tupleTests", "tupleNestedTest")
          val expected = (5L, ("Four", (4L, "Five")))
          assertTrue(actual == expected)
        }
      ),
      suite("References To user Defined Members")(
        test("Reference to value") {
          val actual =
            runTest[Long]("userDefinedReferences", "userDefinedReferenceTests", "userDefinedReferenceValueTest")
          val expected = 5L
          assertTrue(actual == expected)
        },
        test("Curried Function Application") {
          val actual =
            runTest[String]("userDefinedReferences", "userDefinedReferenceTests", "userDefinedReferenceCurriedTest")
          val expected = "Correct"
          assertTrue(actual == expected)
        },
        test("Simple Function Application") {
          val actual = runTest[(Long, Long)](
            "userDefinedReferences",
            "userDefinedReferenceTests",
            "userDefinedReferenceSimpleFunctionTest"
          )
          val expected = (1L, 2L)
          assertTrue(actual == expected)
        },
        test("Calling public function which calls private function") {
          val actual =
            runTest[Long]("userDefinedReferences", "userDefinedReferenceTests", "userDefinedReferencePublicPrivateTest")
          val expected = 10L
          assertTrue(actual == expected)
        },
        test("Reference to Record") {
          val actual =
            runTest[String]("userDefinedReferences", "userDefinedReferenceTests", "userDefinedReferenceRecordTest")
          val expected = "Tom Tit Tot"
          assertTrue(actual == expected)
        },
        test("Reference to Union Type") {
          val actual =
            runTest[Long]("userDefinedReferences", "userDefinedReferenceTests", "userDefinedReferenceUnionTest")
          val expected = -6L
          assertTrue(actual == expected)
        }
      )
    )
}
