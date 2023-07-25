package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.{test, *}
import org.finos.morphir.datamodel.{Data, Concept, Label, EnumLabel}
import org.finos.morphir.datamodel.namespacing.Namespace.ns
import org.finos.morphir.datamodel.namespacing.PackageName.root
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.ir.FQName

object EvaluatorDDLTests extends MorphirBaseSpec {
  lazy val lib =
    EvaluationLibrary("./examples/morphir-elm-projects/evaluator-tests/morphir-ir.json", "Morphir.Examples.App")

  def runTest(moduleName: String, functionName: String)             = lib.runTestDDL(moduleName, functionName, ())
  def runTest(moduleName: String, functionName: String, value: Any) = lib.runTestDDL(moduleName, functionName, value)

  val dogRecordConceptRaw = Concept.Struct(
    List(
      (Label("name"), Concept.String),
      (Label("number"), Concept.Int32)
    )
  )
  def dogRecordData(name: String, number: Int) = Data.Record(
    qn"Morphir/Examples/App:RecordTests:RecordType",
    (Label("name"), Data.String(name)),
    (Label("number"), Data.Int32(number))
  )
//  val dogRecordConcept = Concept.Alias(
//    qn"Morphir/Examples/App:RecordTests:RecordType",
//    dogRecordConceptRaw
//  )
//  def dogRecordData(name: String, number: Int) = Data.Aliased(
//    dogRecordDataRaw(name, number),
//    dogRecordConcept
//  )

  def unionEnumShape: Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ConstructorTests:UnionType",
    List(
      Concept.Enum.Case(
        Label("oneArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32)
        )
      ),
      Concept.Enum.Case(
        Label("twoArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32),
          (EnumLabel.Named("arg2"), Concept.String)
        )
      ),
      Concept.Enum.Case(
        Label("zeroArg"),
        List()
      )
    )
  )

  def typeArgUnionShape(c1: Concept, c2: Concept): Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ExampleModule:TypeArgUnion",
    List(
      Concept.Enum.Case(
        Label("b"),
        List(Tuple2(
          EnumLabel.Named("arg1"),
          c2
        ))
      ),
      Concept.Enum.Case(
        Label("aB"),
        List(
          (
            EnumLabel.Named("arg1"),
            c1
          ),
          (
            EnumLabel.Named("arg2"),
            c2
          )
        )
      ),
      Concept.Enum.Case(
        Label("a"),
        List((
          EnumLabel.Named("arg1"),
          c1
        ))
      ),
      Concept.Enum.Case(
        Label("dictBA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Map(
            c2,
            c1
          )
        ))
      ),
      Concept.Enum.Case(
        Label("maybeA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Optional(c1)
        ))
      )
    )
  )

  val zeroArg: Data = Data.Case(
    List(),
    "Morphir.Examples.App:ConstructorTests:zeroArg",
    unionEnumShape
  )
  def oneArg(i: Int): Data = Data.Case(
    List((EnumLabel.Named("arg1"), Data.Int(i))),
    "Morphir.Examples.App:ConstructorTests:oneArg",
    unionEnumShape
  )
  def twoArg(i: Int, s: String): Data = Data.Case(
    List(
      (EnumLabel.Named("arg1"), Data.Int(i)),
      (EnumLabel.Named("arg2"), Data.String(s))
    ),
    "Morphir.Examples.App:ConstructorTests:twoArg",
    unionEnumShape
  )

  def spec =
    suite("Json Evaluation")(
      suite("Constructor Tests")(
        test("Zero Arg") {
          val actual   = runTest("constructorTests", "constructorZeroArgTest")
          val expected = zeroArg
          assertTrue(actual == expected)
        },
        test("One Arg") {
          val actual =
            runTest("constructorTests", "constructorOneArgAppliedTest")
          val expected = oneArg(5)
          assertTrue(actual == expected)
        },
        test("Two Arg") {
          val actual =
            runTest("constructorTests", "constructorTwoArgAppliedTest")
          val expected = twoArg(5, "Red")
          assertTrue(actual == expected)
        },
        test("Two Arg Curried") {
          val actual =
            runTest("constructorTests", "constructorTwoArgCurriedTest")
          val expected = twoArg(5, "Blue")
          assertTrue(actual == expected)
        },
        test("Lazy Function") {
          val actual   = runTest("constructorTests", "lazyFunctionTest")
          val expected = Data.Tuple(Data.Int(5), Data.Int(5))
          assertTrue(actual == expected)
        }
      ),
      suite("Destructure Tests")(
        test("As") {
          val actual   = runTest("destructureTests", "destructureAsTest")
          val expected = Data.Int(5)
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("destructureTests", "destructureTupleTest")
          val expected = Data.Tuple(Data.Int(1), Data.Int(2))
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("destructureTests", "destructureConstructorTest")
          val expected = Data.Tuple(Data.Int(5), Data.String("red"))
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest("destructureTests", "destructureUnitTest")
          val expected = Data.Int(4)
          assertTrue(actual == expected)
        },
        test("AsTwice") {
          val actual   = runTest("destructureTests", "destructureAsTwiceTest")
          val expected = Data.Tuple(Data.Int(5), Data.Int(5))
          assertTrue(actual == expected)
        },
        test("Tuple Twice") {
          val actual =
            runTest("destructureTests", "destructureTupleTwiceTest")
          val expected = Data.Tuple(Data.String("Blue"), Data.Int(5), Data.Tuple(Data.Int(5), Data.String("Blue")))
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest("destructureTests", "destructureDirectTest")
          val expected = Data.Tuple(Data.Int(6), Data.String("Green"))
          assertTrue(actual == expected)
        }
      ),
      suite("IfThenElse Tests")(
        test("True Branch") {
          val actual   = runTest("ifThenElseTests", "ifThenElseTrueTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("False Branch") {
          val actual   = runTest("ifThenElseTests", "ifThenElseFalseTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Else Unevaluated") {
          val actual   = runTest("ifThenElseTests", "ifThenElseElseBranchUnevaluatedTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Then Unevaluated") {
          val actual   = runTest("ifThenElseTests", "ifThenElseThenBranchUnevaluatedTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        }
      ),
      suite("Lambda Tests")(
        test("As") {
          val actual   = runTest("lambdaTests", "lambdaAsTest")
          val expected = Data.Tuple(Data.Int(5), Data.Int(5))
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("lambdaTests", "lambdaTupleTest")
          val expected = Data.Tuple(Data.Int(0), Data.Int(1))
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("lambdaTests", "lambdaConstructorTest")
          val expected = Data.Tuple(Data.String("Red"), Data.Int(5))
          assertTrue(actual == expected)
        },
        test("Unit") {
          val actual   = runTest("lambdaTests", "lambdaUnitTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Directly Nested") {
          val actual   = runTest("lambdaTests", "lambdaDirectTest")
          val expected = Data.Tuple(Data.Int(0), Data.Int(1))
          assertTrue(actual == expected)
        },
        test("Scope") {
          val actual   = runTest("lambdaTests", "lambdaScopeTest")
          val expected = Data.Tuple(Data.Int(3), Data.Tuple(Data.Int(4), Data.Int(5)))
          assertTrue(actual == expected)
        },
        test("Higher Order") {
          val actual   = runTest("lambdaTests", "lambdaHigherOrderTest")
          val expected = Data.Tuple(Data.Int(3), Data.Int(4), Data.Int(5))
          assertTrue(actual == expected)
        },
        test("User Defined Constructor") {
          val actual   = runTest("lambdaTests", "lambdaUserDefinedTest")
          val expected = Data.Tuple(Data.Int(5), Data.String("Red"))
          assertTrue(actual == expected)
        }
      ),
      suite("Let Definition")(
        test("Make Tuple") {
          val actual   = runTest("letDefinitionTests", "letDefinitionMakeTupleTest")
          val expected = Data.Tuple(Data.Int(1), Data.Int(1))
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual   = runTest("letDefinitionTests", "letDefinitionNestedTest")
          val expected = Data.Tuple(Data.Int(2), Data.Int(2))
          assertTrue(actual == expected)
        },
        test("Simple Function") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionSimpleFunctionTest")
          val expected = Data.Tuple(Data.Int(3), Data.Int(3))
          assertTrue(actual == expected)
        },
        test("Two Argument Function") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionTwoArgFunctionFunctionTest")
          val expected = Data.Tuple(Data.Int(3), Data.Int(2))
          assertTrue(actual == expected)
        },
        test("Curried Function") {
          val actual   = runTest("letDefinitionTests", "letDefinitionCurriedTest")
          val expected = Data.Tuple(Data.Int(2), Data.Int(0))
          assertTrue(actual == expected)
        },
        test("Apply Twice") {
          val actual = runTest(
            "letDefinitionTests",
            "letDefinitionApplyTwiceTest"
          )
          val expected = Data.Tuple(Data.Tuple(Data.Int(1), Data.Int(0)), Data.Tuple(Data.Int(2), Data.Int(0)))
          assertTrue(actual == expected)
        },
        test("Only runs if applied") {
          val actual   = runTest("letDefinitionTests", "letDefinitionDoNotRunTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Lexical cope") {
          val actual =
            runTest("letDefinitionTests", "letDefinitionScopeTest")
          val expected = Data.Tuple(Data.Int(3), Data.Tuple(Data.Int(4), Data.Int(5)))
          assertTrue(actual == expected)
        }
      ),
      suite("Let Recursion")(
        test("Fibbonacci") {
          val actual   = runTest("letRecursionTests", "letRecursionFibonacciTest")
          val expected = Data.Int(34)
          assertTrue(actual == expected)
        },
        test("Mutual Recursion") {
          val actual   = runTest("letRecursionTests", "letRecursionMutualTest")
          val expected = Data.Tuple(Data.Int(8), Data.Int(9))
          assertTrue(actual == expected)
        }
      ),
      suite("Lists")(
        test("Empty") {
          val actual   = runTest("listTests", "listEmptyTest")
          val expected = Data.List(List(), Concept.Int32)
          assertTrue(actual == expected)
        },
        test("Single") {
          val actual   = runTest("listTests", "listSingleTest")
          val expected = Data.List(Data.Int(0))
          assertTrue(actual == expected)
        },
        test("Several") {
          val actual   = runTest("listTests", "listSeveralTest")
          val expected = Data.List(Data.Int(0), Data.Int(1), Data.Int(2), Data.Int(3), Data.Int(4), Data.Int(5))
          assertTrue(actual == expected)
        },
        test("Nested") {
          val actual = runTest("listTests", "listNestedTest")
          val expected = Data.List(
            Data.List(Data.String("Red"), Data.String("Blue")),
            Data.List(List(), Concept.String),
            Data.List(Data.String("Car"), Data.String("Plane"), Data.String("Truck"))
          )
          assertTrue(actual == expected)
        },
        test("Flatten") {
          val actual = runTest("listTests", "listFlattenTest")
          val expected = Data.List(
            Data.String("Red"),
            Data.String("Blue"),
            Data.String("Car"),
            Data.String("Plane"),
            Data.String("Truck")
          )
          assertTrue(actual == expected)
        }
      ),
      suite("Literals")(
        test("String") {
          val actual   = runTest("literalTests", "litStringTest")
          val expected = Data.String("Bloop")
          assertTrue(actual == expected)
        },
        test("Float") {
          val actual   = runTest("literalTests", "litFloatTest")
          val expected = Data.Decimal(scala.BigDecimal("5.0"))
          assertTrue(actual == expected)
        },
        test("Char") {
          val actual   = runTest("literalTests", "litCharTest")
          val expected = Data.Char('f')
          assertTrue(actual == expected)
        },
        test("Boolean") {
          val actual   = runTest("literalTests", "litBoolTest")
          val expected = Data.Boolean(true)
          assertTrue(actual == expected)
        },
        test("Whole Number") {
          val actual   = runTest("literalTests", "litWholeNumberLiteralTest")
          val expected = Data.Int(5)
          assertTrue(actual == expected)
        }
      ),
      suite("Native References")(
        test("Map") {
          val actual =
            runTest("nativeReferenceTests", "nativeReferenceMapTest")
          val expected = Data.List(
            Data.Tuple(Data.Int(1), Data.Int(1)),
            Data.Tuple(Data.Int(2), Data.Int(2)),
            Data.Tuple(Data.Int(3), Data.Int(3))
          )
          assertTrue(actual == expected)
        },
        test("Add") {
          val actual   = runTest("nativeReferenceTests", "nativeReferenceAddTest")
          val expected = Data.Int(3)
          assertTrue(actual == expected)
        },
//        test("Curried Log") {
//          val actual   = runTest("nativeReferenceTests", "nativeReferenceCurriedLogTest")
//          val expected = Double.PositiveInfinity
//          assertTrue(actual == expected)
//        }, //No DDL equivalent
        test("Pi") {
          val actual   = runTest("nativeReferenceTests", "nativeReferencePiTest")
          val expected = Data.Decimal(scala.BigDecimal("3"))
          assertTrue(actual == expected)
        }
      ),
      suite("Patern Matching")(
        test("Wildcard") {
          val actual   = runTest("patternMatchTests", "patternMatchWildcardTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Tuple") {
          val actual   = runTest("patternMatchTests", "patternMatchTupleTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Constructor") {
          val actual   = runTest("patternMatchTests", "patternMatchConstructorTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Zero Arg Constructor") {
          val actual   = runTest("patternMatchTests", "patternMatchEmptyListTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Head Tail") {
          val actual   = runTest("patternMatchTests", "patternMatchHeadTailTest")
          val expected = Data.Tuple(Data.String("Dog"), Data.String("Red"))
          assertTrue(actual == expected)
        },
        test("Literal") {
          val actual   = runTest("patternMatchTests", "patternMatchLiteralTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Repeated As") {
          val actual =
            runTest("patternMatchTests", "patternMatchRepeatedAsTest")
          val expected = Data.Tuple(Data.Int(2), Data.Tuple(Data.Int(1), Data.Int(2)))
          assertTrue(actual == expected)
        }
      ),
      suite("Records")(
        test("Field") {
          val actual   = runTest("recordTests", "recordFieldTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Field from Bound Record") {
          val actual   = runTest("recordTests", "recordFieldFromBoundTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Field Function Apply") {
          val actual   = runTest("recordTests", "fieldFunctionApplyTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Field Function Apply Twice") {
          val actual   = runTest("recordTests", "fieldFunctionApplyTwiceTest")
          val expected = Data.Tuple(Data.Int(1), Data.Int(2))
          assertTrue(actual == expected)
        },
        test("Field") {
          val actual   = runTest("recordTests", "fieldFunctionMapTest")
          val expected = Data.List(Data.String("Soso"), Data.String("Ponyo"), Data.String("Odin"))
          assertTrue(actual == expected)
        },
        test("Simple Record") {
          val actual   = runTest("recordTests", "recordSimpleTest")
          val expected = dogRecordData("Fido", 5)
          assertTrue(actual == expected)
        },
        test("Nested Record") {
          val actual = runTest("recordTests", "recordNestedTest")
          val expected = Data.Record(
            qn"Morphir/Examples/App:RecordTests:NestedRecordType",
            (Label("name"), Data.String("Dogs")),
            (
              Label("records"),
              Data.List(
                dogRecordData("Ponyo", 3),
                dogRecordData("Soso", 3)
              )
            )
          )
          assertTrue(actual == expected)
        },
        test("Record Update Single Field") {
          val actual   = runTest("recordTests", "updateRecordSimpleTest")
          val expected = Data.String("Soso")
          assertTrue(actual == expected)
        },
        test("Record Update Full") {
          val actual   = runTest("recordTests", "updateRecordFullTest")
          val expected = dogRecordData("Soso", 5)
          assertTrue(actual == expected)
        },
        test("Record Updates are not mutation") {
          val actual = runTest("recordTests", "updateRecordImmutableTest")
          val expected =
            Data.List(
              dogRecordData("Soso", 4),
              dogRecordData("Ponyo", 5)
            )
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Unit") {
          val actual   = runTest("simpleTests", "simpleUnitTest")
          val expected = Data.Unit
          assertTrue(actual == expected)
        }
      ),
      suite("Simple")(
        test("Tuple(2)") {
          val actual   = runTest("tupleTests", "tupleTwoTest")
          val expected = Data.Tuple(List(Data.Int(5), Data.Int(4)))
          assertTrue(actual == expected)
        },
        test("Tuple(3)") {
          val actual   = runTest("tupleTests", "tupleThreeTest")
          val expected = Data.Tuple(Data.Int(0), Data.Boolean(true), Data.String("Green"))
          assertTrue(actual == expected)
        },
        test("Nested Tuple") {
          val actual = runTest("tupleTests", "tupleNestedTest")
          val expected = Data.Tuple(
            Data.Int(5),
            Data.Tuple(
              Data.String("Four"),
              Data.Tuple(Data.Int(4), Data.String("Five"))
            )
          )
          assertTrue(actual == expected)
        }
      ),
      suite("References To user Defined Members")(
        test("Reference to value") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceValueTest")
          val expected = Data.Int(5)
          assertTrue(actual == expected)
        },
        test("Curried Function Application") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceCurriedTest")
          val expected = Data.String("Correct")
          assertTrue(actual == expected)
        },
        test("Simple Function Application") {
          val actual = runTest(
            "userDefinedReferenceTests",
            "userDefinedReferenceSimpleFunctionTest"
          )
          val expected = Data.Tuple(List(Data.Int(1), Data.Int(2)))
          assertTrue(actual == expected)
        },
        test("Calling public function which calls private function") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferencePublicPrivateTest")
          val expected = Data.Int(10)
          assertTrue(actual == expected)
        },
        test("Reference to Record") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceRecordTest")
          val expected = Data.String("Tom Tit Tot")
          assertTrue(actual == expected)
        },
        test("Reference to Union Type") {
          val actual =
            runTest("userDefinedReferenceTests", "userDefinedReferenceUnionTest")
          val expected = Data.Int(-6)
          assertTrue(actual == expected)
        },
        test("Reference to Union with type args") {
          val actual =
            runTest("userDefinedReferenceTests", "typeArgUnionTest", (1, "Red"))
          val expected = Data.Case(
            List(
              (EnumLabel.Named("arg1"), Data.Int(1)),
              (EnumLabel.Named("arg2"), Data.String("Red"))
            ),
            "Morphir.Examples.App:ExampleModule:aB",
            typeArgUnionShape(Concept.Int32, Concept.String)
          )
          assertTrue(actual == expected)
        }
      ),
      suite("Dictionary Tests")(
        test("Returns a dictionary") {
          val actual =
            runTest("dictionaryTests", "returnDictionaryTest")
          val expected = Data.Map((Data.Int(1), Data.String("Red")), (Data.Int(2), Data.String("Blue")))
          assertTrue(actual == expected)

        }
      ),
      suite("Optional Tests")(
        test("Returns a Just 1") {
          val actual =
            runTest("optionTests", "returnJustIntTest")
          val expected = Data.Optional.Some(Data.Int(1))
          assertTrue(actual == expected)
        },
        test("Returns a None") {
          val actual =
            runTest("optionTests", "returnNoneIntTest")
          val expected = Data.Optional.None(Concept.Int32)
          assertTrue(actual == expected)
        }
      )
    ) @@ TestAspect.ignore @@ TestAspect.tag("Will re-enable when code-gen of a test are part of the pipeline")
}
