package org.finos.morphir
package runtime

import org.finos.morphir.testing.MorphirBaseSpec
import zio.{Console, Task, ZIO}
import zio.test.{test, *}
import org.finos.morphir.runtime.MorphirRuntime
import org.finos.morphir.datamodel.{Data, Concept, Label, EnumLabel}
import org.finos.morphir.datamodel.namespacing.Namespace.ns
import org.finos.morphir.datamodel.namespacing.PackageName.root
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.ir.FQName
import org.finos.morphir.ir.conversion.*
import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*

object EvaluatorDDLTests extends MorphirBaseSpec {
  val morphirRuntime =
    for {
      irFilePath <- ZIO.succeed(os.pwd / "examples" / "morphir-elm-projects" / "evaluator-tests" / "morphir-ir.json")
      _          <- Console.printLine(s"Loading distribution from $irFilePath")
      dist       <- EvaluationLibrary.loadDistributionFromFileZIO(irFilePath.toString)
    } yield MorphirRuntime.quick(dist)

  def deriveData(input: Any): Data =
    input match {
      case u: Unit             => Deriver.toData(u)
      case i: Int              => Deriver.toData(i)
      case s: String           => Deriver.toData(s)
      case (i: Int, s: String) => Data.Tuple(Deriver.toData(i), Deriver.toData(s))
      case other               => throw new Exception(s"Couldn't derive $other")
    }

  def checkEvaluation(moduleName: String, functionName: String)(expected: => Data) =
    runTest(moduleName, functionName).map { actual =>
      assertTrue(actual == expected)
    }

  def checkEvaluation(moduleName: String, functionName: String, value: Any)(expected: => Data) =
    runTest(moduleName, functionName, value).map { actual =>
      assertTrue(actual == expected)
    }

  def runTest(moduleName: String, functionName: String): Task[Data] = runTest(moduleName, functionName, ())
  def runTest(moduleName: String, functionName: String, value: Any): Task[Data] =
    morphirRuntime.flatMap { runtime =>
      val fullName = s"Morphir.Examples.App:$moduleName:$functionName"
      val data     = deriveData(value)

      ZIO.fromEither(runtime.evaluate(FQName.fromString(fullName), data))
    }

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
          for {
            actual <- runTest("constructorTests", "constructorZeroArgTest")
            expected = zeroArg
          } yield assertTrue(actual == expected)
        },
        test("One Arg") {
          for {
            actual <- runTest("constructorTests", "constructorOneArgAppliedTest")
            expected = oneArg(5)
          } yield assertTrue(actual == expected)
        },
        test("Two Arg") {
          for {
            actual <- runTest("constructorTests", "constructorTwoArgAppliedTest")
            expected = twoArg(5, "Red")
          } yield assertTrue(actual == expected)
        },
        test("Two Arg Curried") {
          for {
            actual <- runTest("constructorTests", "constructorTwoArgCurriedTest")
            expected = twoArg(5, "Blue")
          } yield assertTrue(actual == expected)
        },
        test("Lazy Function") {
          for {
            actual <- runTest("constructorTests", "lazyFunctionTest")
            expected = Data.Tuple(Data.Int(5), Data.Int(5))
          } yield assertTrue(actual == expected)
        }
      ),
      suite("Destructure Tests")(
        test("As") {
          for {
            actual <- runTest("destructureTests", "destructureAsTest")
            expected = Data.Int(5)
          } yield assertTrue(actual == expected)
        },
        test("Tuple") {
          for {
            actual <- runTest("destructureTests", "destructureTupleTest")
            expected = Data.Tuple(Data.Int(1), Data.Int(2))
          } yield assertTrue(actual == expected)
        },
        test("Constructor") {
          for {
            actual <- runTest("destructureTests", "destructureConstructorTest")
            expected = Data.Tuple(Data.Int(5), Data.String("red"))
          } yield assertTrue(actual == expected)
        },
        test("Unit") {
          checkEvaluation("destructureTests", "destructureUnitTest")(Data.Int(4))
        },
        test("AsTwice") {
          checkEvaluation("destructureTests", "destructureAsTwiceTest")(Data.Tuple(Data.Int(5), Data.Int(5)))
        },
        test("Tuple Twice") {
          checkEvaluation("destructureTests", "destructureTupleTwiceTest")(Data.Tuple(
            Data.String("Blue"),
            Data.Int(5),
            Data.Tuple(Data.Int(5), Data.String("Blue"))
          ))
        },
        test("Directly Nested") {
          checkEvaluation("destructureTests", "destructureDirectTest")(Data.Tuple(Data.Int(6), Data.String("Green")))
        }
      ),
      suite("IfThenElse Tests")(
        test("True Branch") {
          checkEvaluation("ifThenElseTests", "ifThenElseTrueTest")(Data.String("Correct"))
        },
        test("False Branch") {
          checkEvaluation("ifThenElseTests", "ifThenElseFalseTest")(Data.String("Correct"))
        },
        test("Else Unevaluated") {
          checkEvaluation("ifThenElseTests", "ifThenElseElseBranchUnevaluatedTest")(Data.String("Correct"))
        },
        test("Then Unevaluated") {
          checkEvaluation("ifThenElseTests", "ifThenElseThenBranchUnevaluatedTest")(Data.String("Correct"))
        }
      ),
      suite("Lambda Tests")(
        test("As") {
          checkEvaluation("lambdaTests", "lambdaAsTest")(Data.Tuple(Data.Int(5), Data.Int(5)))
        },
        test("Tuple") {
          checkEvaluation("lambdaTests", "lambdaTupleTest")(Data.Tuple(Data.Int(0), Data.Int(1)))
        },
        test("Constructor") {
          checkEvaluation("lambdaTests", "lambdaConstructorTest")(Data.Tuple(Data.String("Red"), Data.Int(5)))
        },
        test("Unit") {
          checkEvaluation("lambdaTests", "lambdaUnitTest")(Data.String("Correct"))
        },
        test("Directly Nested") {
          checkEvaluation("lambdaTests", "lambdaDirectTest")(Data.Tuple(Data.Int(0), Data.Int(1)))
        },
        test("Scope") {
          checkEvaluation("lambdaTests", "lambdaScopeTest")(Data.Tuple(
            Data.Int(3),
            Data.Tuple(Data.Int(4), Data.Int(5))
          ))
        },
        test("Higher Order") {
          checkEvaluation("lambdaTests", "lambdaHigherOrderTest")(Data.Tuple(Data.Int(3), Data.Int(4), Data.Int(5)))
        },
        test("User Defined Constructor") {
          checkEvaluation("lambdaTests", "lambdaUserDefinedTest")(Data.Tuple(Data.Int(5), Data.String("Red")))
        }
      ),
      suite("Let Definition")(
        test("Make Tuple") {
          checkEvaluation("letDefinitionTests", "letDefinitionMakeTupleTest")(Data.Tuple(Data.Int(1), Data.Int(1)))
        },
        test("Nested") {
          checkEvaluation("letDefinitionTests", "letDefinitionNestedTest")(Data.Tuple(Data.Int(2), Data.Int(2)))
        },
        test("Simple Function") {
          checkEvaluation("letDefinitionTests", "letDefinitionSimpleFunctionTest")(Data.Tuple(Data.Int(3), Data.Int(3)))
        },
        test("Two Argument Function") {
          checkEvaluation("letDefinitionTests", "letDefinitionTwoArgFunctionFunctionTest")(Data.Tuple(
            Data.Int(3),
            Data.Int(2)
          ))
        },
        test("Curried Function") {
          checkEvaluation("letDefinitionTests", "letDefinitionCurriedTest")(Data.Tuple(Data.Int(2), Data.Int(0)))
        },
        test("Apply Twice") {
          checkEvaluation("letDefinitionTests", "letDefinitionApplyTwiceTest")(Data.Tuple(
            Data.Tuple(Data.Int(1), Data.Int(0)),
            Data.Tuple(Data.Int(2), Data.Int(0))
          ))
        },
        test("Only runs if applied") {
          checkEvaluation("letDefinitionTests", "letDefinitionDoNotRunTest")(Data.String("Correct"))
        },
        test("Lexical scope") {
          checkEvaluation("letDefinitionTests", "letDefinitionScopeTest")(Data.Tuple(
            Data.Int(3),
            Data.Tuple(Data.Int(4), Data.Int(5))
          ))
        }
      ),
      suite("Let Recursion")(
        test("Fibbonacci") {
          checkEvaluation("letRecursionTests", "letRecursionFibonacciTest")(Data.Int(34))
        },
        test("Mutual Recursion") {
          checkEvaluation("letRecursionTests", "letRecursionMutualTest")(Data.Tuple(Data.Int(8), Data.Int(9)))
        }
      ),
      suite("Lists")(
        test("Empty") {
          checkEvaluation("listTests", "listEmptyTest")(Data.List(List(), Concept.Int32))
        },
        test("Single") {
          checkEvaluation("listTests", "listSingleTest")(Data.List(Data.Int(0)))
        },
        test("Several") {
          checkEvaluation("listTests", "listSeveralTest")(Data.List(
            Data.Int(0),
            Data.Int(1),
            Data.Int(2),
            Data.Int(3),
            Data.Int(4),
            Data.Int(5)
          ))
        },
        test("Nested") {
          checkEvaluation("listTests", "listNestedTest")(Data.List(
            Data.List(Data.String("Red"), Data.String("Blue")),
            Data.List(List(), Concept.String),
            Data.List(Data.String("Car"), Data.String("Plane"), Data.String("Truck"))
          ))
        },
        test("Flatten") {
          checkEvaluation("listTests", "listFlattenTest")(Data.List(
            Data.String("Red"),
            Data.String("Blue"),
            Data.String("Car"),
            Data.String("Plane"),
            Data.String("Truck")
          ))
        }
      ),
      suite("Literals")(
        test("String") {
          checkEvaluation("literalTests", "litStringTest")(Data.String("Bloop"))
        },
        test("Float") {
          checkEvaluation("literalTests", "litFloatTest")(Data.Decimal(scala.BigDecimal("5.0")))
        },
        test("Char") {
          checkEvaluation("literalTests", "litCharTest")(Data.Char('f'))
        },
        test("Boolean") {
          checkEvaluation("literalTests", "litBoolTest")(Data.Boolean(true))
        },
        test("Whole Number") {
          checkEvaluation("literalTests", "litWholeNumberLiteralTest")(Data.Int(5))
        }
      ),
      suite("Native References")(
        test("Map") {
          checkEvaluation("nativeReferenceTests", "nativeReferenceMapTest")(Data.List(
            Data.Tuple(Data.Int(1), Data.Int(1)),
            Data.Tuple(Data.Int(2), Data.Int(2)),
            Data.Tuple(Data.Int(3), Data.Int(3))
          ))
        },
        test("Add") {
          checkEvaluation("nativeReferenceTests", "nativeReferenceAddTest")(Data.Int(3))
        },
//        test("Curried Log") {
//          val actual   = runTest("nativeReferenceTests", "nativeReferenceCurriedLogTest")
//          val expected = Double.PositiveInfinity
//          assertTrue(actual == expected)
//        }, //No DDL equivalent
        test("Pi") {
          checkEvaluation("nativeReferenceTests", "nativeReferencePiTest")(Data.Decimal(scala.BigDecimal("3")))
        }
      ),
      suite("Patern Matching")(
        test("Wildcard") {
          checkEvaluation("patternMatchTests", "patternMatchWildcardTest")(Data.String("Correct"))
        },
        test("Tuple") {
          checkEvaluation("patternMatchTests", "patternMatchTupleTest")(Data.String("Correct"))
        },
        test("Constructor") {
          checkEvaluation("patternMatchTests", "patternMatchConstructorTest")(Data.String("Correct"))
        },
        test("Zero Arg Constructor") {
          checkEvaluation("patternMatchTests", "patternMatchEmptyListTest")(Data.String("Correct"))
        },
        test("Head Tail") {
          checkEvaluation("patternMatchTests", "patternMatchHeadTailTest")(Data.Tuple(
            Data.String("Dog"),
            Data.String("Red")
          ))
        },
        test("Literal") {
          checkEvaluation("patternMatchTests", "patternMatchLiteralTest")(Data.String("Correct"))
        },
        test("Repeated As") {
          checkEvaluation("patternMatchTests", "patternMatchRepeatedAsTest")(Data.Tuple(
            Data.Int(2),
            Data.Tuple(Data.Int(1), Data.Int(2))
          ))
        }
      ),
      suite("Records")(
        test("Field") {
          checkEvaluation("recordTests", "recordFieldTest")(Data.String("Correct"))
        },
        test("Field from Bound Record") {
          checkEvaluation("recordTests", "recordFieldFromBoundTest")(Data.String("Correct"))
        },
        test("Field Function Apply") {
          checkEvaluation("recordTests", "fieldFunctionApplyTest")(Data.String("Correct"))
        },
        test("Field Function Apply Twice") {
          checkEvaluation("recordTests", "fieldFunctionApplyTwiceTest")(Data.Tuple(Data.Int(1), Data.Int(2)))
        },
        test("Field") {
          checkEvaluation("recordTests", "fieldFunctionMapTest")(Data.List(
            Data.String("Soso"),
            Data.String("Ponyo"),
            Data.String("Odin")
          ))
        },
        test("Simple Record") {
          checkEvaluation("recordTests", "recordSimpleTest")(dogRecordData("Fido", 5))
        },
        test("Nested Record") {
          checkEvaluation("recordTests", "recordNestedTest")(Data.Record(
            qn"Morphir/Examples/App:RecordTests:NestedRecordType",
            (Label("name"), Data.String("Dogs")),
            (
              Label("records"),
              Data.List(
                dogRecordData("Ponyo", 3),
                dogRecordData("Soso", 3)
              )
            )
          ))
        },
        test("Record Update Single Field") {
          checkEvaluation("recordTests", "updateRecordSimpleTest")(Data.String("Soso"))
        },
        test("Record Update Full") {
          checkEvaluation("recordTests", "updateRecordFullTest")(dogRecordData("Soso", 5))
        },
        test("Record Updates are not mutation") {
          checkEvaluation("recordTests", "updateRecordImmutableTest")(
            Data.List(
              dogRecordData("Soso", 4),
              dogRecordData("Ponyo", 5)
            )
          )
        }
      ),
      suite("Simple")(
        test("Unit") {
          checkEvaluation("simpleTests", "simpleUnitTest")(Data.Unit)
        }
      ),
      suite("Simple")(
        test("Tuple(2)") {
          checkEvaluation("tupleTests", "tupleTwoTest")(Data.Tuple(List(Data.Int(5), Data.Int(4))))
        },
        test("Tuple(3)") {
          checkEvaluation("tupleTests", "tupleThreeTest")(Data.Tuple(
            Data.Int(0),
            Data.Boolean(true),
            Data.String("Green")
          ))
        },
        test("Nested Tuple") {
          checkEvaluation("tupleTests", "tupleNestedTest")(Data.Tuple(
            Data.Int(5),
            Data.Tuple(
              Data.String("Four"),
              Data.Tuple(Data.Int(4), Data.String("Five"))
            )
          ))
        }
      ),
      suite("References To user Defined Members")(
        test("Reference to value") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferenceValueTest")(Data.Int(5))
        },
        test("Curried Function Application") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferenceCurriedTest")(Data.String("Correct"))
        },
        test("Simple Function Application") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferenceSimpleFunctionTest")(Data.Tuple(List(
            Data.Int(1),
            Data.Int(2)
          )))
        },
        test("Calling public function which calls private function") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferencePublicPrivateTest")(Data.Int(10))
        },
        test("Reference to Record") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferenceRecordTest")(Data.String("Tom Tit Tot"))
        },
        test("Reference to Union Type") {
          checkEvaluation("userDefinedReferenceTests", "userDefinedReferenceUnionTest")(Data.Int(-6))
        },
        test("Reference to Union with type args") {
          checkEvaluation("userDefinedReferenceTests", "typeArgUnionTest", (1, "Red"))(Data.Case(
            List(
              (EnumLabel.Named("arg1"), Data.Int(1)),
              (EnumLabel.Named("arg2"), Data.String("Red"))
            ),
            "Morphir.Examples.App:ExampleModule:aB",
            typeArgUnionShape(Concept.Int32, Concept.String)
          ))
        }
      ),
      suite("Dictionary Tests")(
        test("Returns a dictionary") {
          checkEvaluation("dictionaryTests", "returnDictionaryTest")(Data.Map(
            (Data.Int(1), Data.String("Red")),
            (Data.Int(2), Data.String("Blue"))
          ))
        }
      ),
      suite("Optional Tests")(
        test("Returns a Just 1") {
          checkEvaluation("optionTests", "returnJustIntTest")(Data.Optional.Some(Data.Int(1)))
        },
        test("Returns a None") {
          checkEvaluation("optionTests", "returnNoneIntTest")(Data.Optional.None(Concept.Int32))
        }
      )
    ) @@ TestAspect.ignore @@ TestAspect.tag("Will re-enable when code-gen of a test are part of the pipeline")
}
