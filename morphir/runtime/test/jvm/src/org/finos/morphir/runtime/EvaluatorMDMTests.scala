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

/**
 * The below reads as “This is a Scala test named ModBy which refers to a function named nativeReferenceModByTest in the
 * module nativeReferenceTests; it should be called with 7, and the result is expected to be 1.
 *
 * testEval("ModBy")("nativeReferenceTests", "nativeReferenceModByTest", 7)( Data.Int(1) )
 */
object EvaluatorMDMTests extends MorphirBaseSpec {
  val morphirRuntimeLayer: ZLayer[Any, Throwable, TypedMorphirRuntime] =
    ZLayer(for {
      irFilePath <- ZIO.succeed(os.pwd / "examples" / "morphir-elm-projects" / "evaluator-tests" / "morphir-ir.json")
      _          <- Console.printLine(s"Loading distribution from $irFilePath")
      dist       <- EvaluationLibrary.loadDistributionFromFileZIO(irFilePath.toString)
    } yield MorphirRuntime.quick(dist))

  val localDate = java.time.LocalDate.of(1900, 1, 20)
  val localTime = java.time.LocalTime.of(10, 43, 26)

  def deriveData(input: Any): Data =
    input match {
      // If the data is already derived, just use it!
      case alreadyData: Data       => alreadyData
      case u: Unit                 => Deriver.toData(u)
      case b: Boolean              => Deriver.toData(b)
      case i: Int                  => Deriver.toData(i)
      case c: Char                 => Deriver.toData(c)
      case d: Double               => Deriver.toData(d)
      case s: String               => Deriver.toData(s)
      case ld: java.time.LocalDate => Deriver.toData(ld)
      case lt: java.time.LocalTime => Deriver.toData(lt)
      case list: List[_] =>
        val mapped = list.map(deriveData(_))
        Data.List(mapped.head, mapped.tail: _*)
      case map: Map[_, _] =>
        val pairs = map.toList.map { case (key, value) => (deriveData(key), deriveData(value)) }
        Data.Map(pairs.head, pairs.tail: _*)
      case Some(a: Any)           => Data.Optional.Some(deriveData(a))
      case (first, second)        => Data.Tuple(deriveData(first), deriveData(second))
      case (first, second, third) => Data.Tuple(deriveData(first), deriveData(second), deriveData(third))
      case e: Either[_, _] => throw new Exception(
          s"Couldn't derive $e (Hint: I can't tell what the other side of the either would be. Use Data constructors directly instead."
        )
      case other => throw new Exception(s"Couldn't derive $other")
    }

  def checkEvaluation(
      moduleName: String,
      functionName: String
  )(expected: => Data): ZIO[TypedMorphirRuntime, Throwable, TestResult] =
    runTest(moduleName, functionName).map { actual =>
      assertTrue(actual == expected)
    }

  def checkEvaluation(
      moduleName: String,
      functionName: String,
      values: List[Any]
  )(expected: => Data): ZIO[TypedMorphirRuntime, Throwable, TestResult] =
    runTest(moduleName, functionName, values).map { actual =>
      assertTrue(actual == expected)
    }

  def testEvaluation(label: String)(moduleName: String, functionName: String)(expected: => Data) =
    test(label) {
      checkEvaluation(moduleName, functionName)(expected)
    }

  def testEval(label: String)(moduleName: String, functionName: String, value: Any)(expected: => Data) =
    test(label) {
      checkEvaluation(moduleName, functionName, List(value))(expected)
    }

  def testEvalMultiple(label: String)(moduleName: String, functionName: String, values: List[Any])(expected: => Data) =
    test(label) {
      checkEvaluation(moduleName, functionName, values)(expected)
    }

  def runTest(moduleName: String, functionName: String): ZIO[TypedMorphirRuntime, Throwable, Data] =
    runTest(moduleName, functionName, List())

  def runTest(
      moduleName: String,
      functionName: String,
      values: List[Any]
  ): ZIO[TypedMorphirRuntime, Throwable, Data] =
    ZIO.serviceWithZIO[TypedMorphirRuntime] { runtime =>
      val data = values.map(deriveData(_))
      runTestMDM(moduleName, functionName, data)
    }

  def runTestMDM(
      moduleName: String,
      functionName: String,
      data: List[Data]
  ): ZIO[TypedMorphirRuntime, Throwable, Data] =
    ZIO.serviceWithZIO[TypedMorphirRuntime] { runtime =>
      val fullName = s"Morphir.Examples.App:$moduleName:$functionName"
      if (data.isEmpty)
        runtime.evaluate(
          FQName.fromString(fullName),
          Data.Record(FQName.fromString("Morphir.Examples.App:TestUtils:testContext "))
        )
          .provideEnvironment(MorphirEnv.live)
          .toZIOWith(RTExecutionContext.typeChecked)
      else
        runtime.evaluate(FQName.fromString(fullName), data.head, data.tail: _*)
          .provideEnvironment(MorphirEnv.live)
          .toZIOWith(RTExecutionContext.typeChecked)
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

  def resultStringIntShape = Concept.Result(Concept.String, Concept.Int32)

  def resultBoolIntShape = Concept.Result(Concept.Boolean, Concept.Int32)

  def unionEnumShape: Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ConstructorTests:UnionType",
    List(
      Concept.Enum.Case(
        Label("OneArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32)
        )
      ),
      Concept.Enum.Case(
        Label("TwoArg"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32),
          (EnumLabel.Named("arg2"), Concept.String)
        )
      ),
      Concept.Enum.Case(
        Label("ZeroArg"),
        List()
      )
    )
  )

  def typeArgUnionShape(c1: Concept, c2: Concept): Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ExampleModule:TypeArgUnion",
    List(
      Concept.Enum.Case(
        Label("B"),
        List(Tuple2(
          EnumLabel.Named("arg1"),
          c2
        ))
      ),
      Concept.Enum.Case(
        Label("AB"),
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
        Label("A"),
        List((
          EnumLabel.Named("arg1"),
          c1
        ))
      ),
      Concept.Enum.Case(
        Label("DictBA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Map(
            c2,
            c1
          )
        ))
      ),
      Concept.Enum.Case(
        Label("MaybeA"),
        List((
          EnumLabel.Named("arg1"),
          Concept.Optional(c1)
        ))
      )
    )
  )

  val zeroArg: Data = Data.Case(
    List(),
    "ZeroArg",
    unionEnumShape
  )

  def oneArg(i: Int): Data = Data.Case(
    List((EnumLabel.Named("arg1"), Data.Int(i))),
    "OneArg",
    unionEnumShape
  )

  def twoArg(i: Int, s: String): Data = Data.Case(
    List(
      (EnumLabel.Named("arg1"), Data.Int(i)),
      (EnumLabel.Named("arg2"), Data.String(s))
    ),
    "TwoArg",
    unionEnumShape
  )

  def spec =
    suite("Evaluator MDM Specs")(
      suite("Constructor Tests")(
        test("Zero Arg Input") {
          for {
            actual <- runTestMDM("constructorTests", "constructorInputTest", List(zeroArg))
            expected = Data.Tuple(Data.Int(0), Data.String("ZeroArg"))
          } yield assertTrue(actual == expected)
        },
        test("One Arg Input") {
          for {
            actual <- runTestMDM("constructorTests", "constructorInputTest", List(oneArg(4)))
            expected = Data.Tuple(Data.Int(4), Data.String("OneArg"))
          } yield assertTrue(actual == expected)
        },
        test("Two Arg Input") {
          for {
            actual <- runTest("constructorTests", "constructorInputTest", List(twoArg(5, "TwoArgActualArg")))
            expected = Data.Tuple(Data.Int(5), Data.String("TwoArgActualArg"))
          } yield assertTrue(actual == expected)
        },
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
        testEvaluation("Unit")("destructureTests", "destructureUnitTest")(Data.Int(4)),
        testEvaluation("AsTwice")("destructureTests", "destructureAsTwiceTest")(Data.Tuple(Data.Int(5), Data.Int(5))),
        testEvaluation("Tuple Twice")("destructureTests", "destructureTupleTwiceTest")(Data.Tuple(
          Data.String("Blue"),
          Data.Int(5),
          Data.Tuple(Data.Int(5), Data.String("Blue"))
        )),
        testEvaluation("Directly Nested")("destructureTests", "destructureDirectTest")(Data.Tuple(
          Data.Int(6),
          Data.String("Green")
        ))
      ),
      suite("IfThenElse Tests")(
        testEvaluation("True Branch")("ifThenElseTests", "ifThenElseTrueTest")(Data.String("Correct")),
        testEvaluation("False Branch")("ifThenElseTests", "ifThenElseFalseTest")(Data.String("Correct")),
        testEvaluation("Else Unevaluated")("ifThenElseTests", "ifThenElseElseBranchUnevaluatedTest")(
          Data.String("Correct")
        ),
        testEvaluation("Then Unevaluated")("ifThenElseTests", "ifThenElseThenBranchUnevaluatedTest")(
          Data.String("Correct")
        )
      ),
      suite("Decimal Tests")(
        testEvaluation("fromFloat")("decimalTests", "decimalFromFloatTest")(Data.Decimal(1.2)),
        testEvaluation("toFloat")("decimalTests", "decimalToFloatTest")(Data.Float(1.5)),
        testEvaluation("toString")("decimalTests", "decimalToStringTest")(Data.String("1.2"))
      ),
      suite("Lambda Tests")(
        testEvaluation("As")("lambdaTests", "lambdaAsTest")(Data.Tuple(Data.Int(5), Data.Int(5))),
        testEvaluation("Tuple")("lambdaTests", "lambdaTupleTest")(Data.Tuple(Data.Int(0), Data.Int(1))),
        testEvaluation("Constructor")("lambdaTests", "lambdaConstructorTest")(Data.Tuple(
          Data.String("Red"),
          Data.Int(5)
        )),
        testEvaluation("Unit")("lambdaTests", "lambdaUnitTest")(Data.String("Correct")),
        testEvaluation("Directly Nested")("lambdaTests", "lambdaDirectTest")(Data.Tuple(Data.Int(0), Data.Int(1))),
        testEvaluation("Scope")("lambdaTests", "lambdaScopeTest")(Data.Tuple(
          Data.Int(3),
          Data.Tuple(Data.Int(4), Data.Int(5))
        )),
        testEvaluation("Higher Order")("lambdaTests", "lambdaHigherOrderTest")(Data.Tuple(
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEvaluation("User Defined Constructor")("lambdaTests", "lambdaUserDefinedTest")(Data.Tuple(
          Data.Int(5),
          Data.String("Red")
        ))
      ),
      suite("Let Definition")(
        testEvaluation("Make Tuple")("letDefinitionTests", "letDefinitionMakeTupleTest")(Data.Tuple(
          Data.Int(1),
          Data.Int(1)
        )),
        testEvaluation("Nested")("letDefinitionTests", "letDefinitionNestedTest")(Data.Tuple(Data.Int(2), Data.Int(2))),
        testEvaluation("Simple Function")("letDefinitionTests", "letDefinitionSimpleFunctionTest")(Data.Tuple(
          Data.Int(3),
          Data.Int(3)
        )),
        testEvaluation("Two Argument Function")("letDefinitionTests", "letDefinitionTwoArgFunctionFunctionTest")(
          Data.Tuple(
            Data.Int(3),
            Data.Int(2)
          )
        ),
        testEvaluation("Curried Function")("letDefinitionTests", "letDefinitionCurriedTest")(Data.Tuple(
          Data.Int(2),
          Data.Int(0)
        )),
        testEvaluation("Apply Twice")("letDefinitionTests", "letDefinitionApplyTwiceTest")(Data.Tuple(
          Data.Tuple(Data.Int(1), Data.Int(0)),
          Data.Tuple(Data.Int(2), Data.Int(0))
        )),
        testEvaluation("Only runs if applied")("letDefinitionTests", "letDefinitionDoNotRunTest")(
          Data.String("Correct")
        ),
        testEvaluation("Lexical scope")("letDefinitionTests", "letDefinitionScopeTest")(Data.Tuple(
          Data.Int(3),
          Data.Tuple(Data.Int(4), Data.Int(5))
        ))
      ),
      suite("Let Recursion")(
        testEvaluation("Fibbonacci")("letRecursionTests", "letRecursionFibonacciTest")(Data.Int(34)),
        testEvaluation("Mutual Recursion")("letRecursionTests", "letRecursionMutualTest")(Data.Tuple(
          Data.Int(8),
          Data.Int(9)
        ))
      ),
      suite("Lists")(
        testEvaluation("Single")("listTests", "listSingleTest")(Data.List(Data.Int(0))),
        testEvaluation("Several")("listTests", "listSeveralTest")(Data.List(
          Data.Int(0),
          Data.Int(1),
          Data.Int(2),
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEvaluation("Nested")("listTests", "listNestedTest")(Data.List(
          Data.List(Data.String("Red"), Data.String("Blue")),
          Data.List(List(), Concept.String),
          Data.List(Data.String("Car"), Data.String("Plane"), Data.String("Truck"))
        )),
        testEvaluation("Concat")("listTests", "listConcatTest")(Data.List(
          Data.Int(1),
          Data.Int(2),
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEvaluation("Flatten")("listTests", "listFlattenTest")(Data.List(
          Data.String("Red"),
          Data.String("Blue"),
          Data.String("Car"),
          Data.String("Plane"),
          Data.String("Truck")
        )),
        testEvaluation("Filter")("listTests", "listFilterTest")(Data.List(
          Data.Int(4),
          Data.Int(5),
          Data.Int(6)
        )),
        testEvaluation("Map")("listTests", "listMapTest")(Data.List(
          Data.Int(4),
          Data.Int(5),
          Data.Int(6)
        )),
        testEval("MapDefinition")("listTests", "listMapDefinitionTest", List(1, 2, 3))(Data.List(
          Data.Int(2),
          Data.Int(3),
          Data.Int(4)
        )),
        testEvaluation("Map Native")("listTests", "listMapTestNative")(Data.List(
          Data.Float(3.0),
          Data.Float(4.0),
          Data.Float(5.0)
        )),
        testEvaluation("Map with casting")("listTests", "listMapTestWithCasting")(Data.List(
          Data.Float(4.0),
          Data.Float(5.0),
          Data.Float(6.0)
        )),
        testEvaluation("Map2")("listTests", "listMapTest2")(Data.List(
          Data.Boolean(false),
          Data.Boolean(true),
          Data.Boolean(false)
        )),
        testEvaluation("Fold Left")("listTests", "listFoldLeftTest")(
          Data.String("<foo|bar|baz|")
        ),
        testEvaluation("Fold Left Advanced")("listTests", "listFoldLeftAdvTest")(
          Data.Map(
            Data.String("foo")   -> Data.Int(3),
            Data.String("barr")  -> Data.Int(4),
            Data.String("bazzz") -> Data.Int(5)
          )
        ),
        testEvaluation("List.any with True Output")("listTests", "listAnyTrueTest")(
          Data.Boolean(true)
        ),
        testEvaluation("List.any with True Output")("listTests", "listAnyFalseTest")(
          Data.Boolean(false)
        ),
        testEvaluation("List Partition")("listTests", "listPartitionTest")(
          Data.Tuple(
            Data.List(Data.Int(1), Data.Int(3), Data.Int(5)),
            Data.List(Data.Int(2), Data.Int(4))
          )
        ),
        testEvalMultiple("Append (and infer type")(
          "ListTests",
          "listAppend",
          List(List(true, true), List(false, true))
        )(
          Data.List(Data.True, Data.True, Data.False, Data.True)
        ),
        testEvaluation("Singleton")("listTests", "listSingletonTest")(
          Data.List(Data.Int(6))
        ),
        testEvaluation("isEmpty")("listTests", "listIsEmptyTest1")(
          (Data.Boolean(true))
        ),
        testEvaluation("isEmpty")("listTests", "listIsEmptyTest2")(
          (Data.Boolean(false))
        ),
        testEvaluation("length")("listTests", "listLengthTest")(
          (Data.Int32(6))
        )
      ),
      suite("Literals")(
        testEvaluation("String")("literalTests", "litStringTest")(Data.String("Bloop")),
        testEvaluation("Float")("literalTests", "litFloatTest")(Data.Float(5.0)),
        testEvaluation("Char")("literalTests", "litCharTest")(Data.Char('f')),
        testEvaluation("Boolean")("literalTests", "litBoolTest")(Data.Boolean(true)),
        testEvaluation("Whole Number")("literalTests", "litWholeNumberLiteralTest")(Data.Int(5))
      ),
      suite("LocalDate")(
        testEvaluation("fromParts")("localDateTests", "fromPartsTest")(
          Data.Optional.Some(Data.LocalDate(localDate))
        ),
        testEvaluation("fromParts invalid")("localDateTests", "fromPartsInvalidTest")(
          Data.Optional.None(Concept.LocalDate)
        ),
        suite("fromOrdinalDate")(
          testEvalMultiple("fromOrdinalDate valid")("localDateTests", "fromOrdinalDateTest", List(1900, 20))(
            Data.LocalDate(localDate)
          ),
          testEvalMultiple("fromOrdinalDate clamped day pos, not leap year")(
            "localDateTests",
            "fromOrdinalDateTest",
            List(1900, 366)
          )(
            Data.LocalDate(java.time.LocalDate.of(1900, 12, 31))
          ),
          testEvalMultiple("fromOrdinalDate clamped day pos, leap year")(
            "localDateTests",
            "fromOrdinalDateTest",
            List(1904, 367)
          )(
            Data.LocalDate(java.time.LocalDate.of(1904, 12, 31))
          ),
          testEvalMultiple("fromOrdinalDate clamped day neg")(
            "localDateTests",
            "fromOrdinalDateTest",
            List(1900, -9999)
          )(
            Data.LocalDate(java.time.LocalDate.of(1900, 1, 1))
          ),
          testEvalMultiple("fromOrdinalDate clamped year pos")(
            "localDateTests",
            "fromOrdinalDateTest",
            List(java.time.Year.MAX_VALUE + 1, 20)
          )(
            Data.LocalDate(java.time.LocalDate.of(java.time.Year.MAX_VALUE, 1, 20))
          ),
          testEvalMultiple("fromOrdinalDate clamped year neg")(
            "localDateTests",
            "fromOrdinalDateTest",
            List(java.time.Year.MIN_VALUE - 1, 20)
          )(
            Data.LocalDate(java.time.LocalDate.of(java.time.Year.MIN_VALUE, 1, 20))
          )
        ),
        testEvalMultiple("addWeeks")("localDateTests", "addWeeksTest", List(2, localDate))(
          Data.LocalDate(localDate.plusWeeks(2))
        ),
        testEvalMultiple("diffInDays")("localDateTests", "diffInDaysTest", List(localDate, localDate.plusDays(999)))(
          Data.Int(999)
        ),
        testEval("fromISO valid iso date")("localDateTests", "fromISOTest", "1900-01-20")(
          Data.Optional.Some(Data.LocalDate(localDate))
        ),
        testEval("fromISO valid iso week")("localDateTests", "fromISOTest", "1900-W03-6")(
          Data.Optional.Some(Data.LocalDate(localDate))
        ),
        testEval("fromISO valid iso ordinal")("localDateTests", "fromISOTest", "1900-020")(
          Data.Optional.Some(Data.LocalDate(localDate))
        ),
        testEval("fromISO invalid iso date")("localDateTests", "fromISOTest", "1900-44-55")(
          Data.Optional.None(Concept.LocalDate)
        ),
        testEval("fromISO invalid iso week")("localDateTests", "fromISOTest", "1900-W01-8")(
          Data.Optional.None(Concept.LocalDate)
        ),
        testEval("fromISO invalid iso ordinal")("localDateTests", "fromISOTest", "1900-366")(
          Data.Optional.None(Concept.LocalDate)
        )
      ),
      suite("LocalTime")(
        testEvaluation("fromMilliseconds")("localTimeTests", "fromMillisecondsTest")(Data.LocalTime(localTime)),
        testEvalMultiple("addHours")("localTimeTests", "addHoursTest", List(2, localTime))(
          Data.LocalTime(localTime.plusHours(2))
        ),
        testEvalMultiple("addHours negative")("localTimeTests", "addHoursTest", List(-2, localTime))(
          Data.LocalTime(localTime.minusHours(2))
        ),
        testEvalMultiple("addMinutes")("localTimeTests", "addMinutesTest", List(2, localTime))(
          Data.LocalTime(localTime.plusMinutes(2))
        ),
        testEvalMultiple("addMinutes negative")("localTimeTests", "addMinutesTest", List(-2, localTime))(
          Data.LocalTime(localTime.minusMinutes(2))
        ),
        testEvalMultiple("addSeconds")("localTimeTests", "addSecondsTest", List(2, localTime))(
          Data.LocalTime(localTime.plusSeconds(2))
        ),
        testEvalMultiple("addSeconds negative")("localTimeTests", "addSecondsTest", List(-2, localTime))(
          Data.LocalTime(localTime.minusSeconds(2))
        ),
        // NOTE: diffInSeconds is implemented a - b (instead of b - a) for conformity to morphir-elm impl
        testEvalMultiple("diffInSeconds")(
          "localTimeTests",
          "diffInSecondsTest",
          List(localTime, localTime.plusSeconds(2))
        )(Data.Int(-2)),
        testEvalMultiple("diffInSeconds negative")(
          "localTimeTests",
          "diffInSecondsTest",
          List(localTime, localTime.minusSeconds(2))
        )(Data.Int(2)),
        testEval("fromISO valid iso time")("localTimeTests", "fromISOTest", "10:43:26.111111111")(
          Data.Optional.Some(Data.LocalTime(java.time.LocalTime.of(10, 43, 26, 111111111)))
        ),
        testEval("fromISO valid iso time no seconds")("localTimeTests", "fromISOTest", "10:43")(
          Data.Optional.Some(Data.LocalTime(java.time.LocalTime.of(10, 43, 0)))
        ),
        testEval("fromISO invalid iso time")("localTimeTests", "fromISOTest", "10:43:26+00:00")(
          Data.Optional.None(Concept.LocalTime)
        )
      ),
      suite("Native References")(
        testEvaluation("Map")("nativeReferenceTests", "nativeReferenceMapTest")(Data.List(
          Data.Tuple(Data.Int(1), Data.Int(1)),
          Data.Tuple(Data.Int(2), Data.Int(2)),
          Data.Tuple(Data.Int(3), Data.Int(3))
        )),
        testEvaluation("Add")("nativeReferenceTests", "nativeReferenceAddTest")(Data.Int(3)),
//        test("Curried Log") {
//          val actual   = runTest("nativeReferenceTests", "nativeReferenceCurriedLogTest")
//          val expected = Double.PositiveInfinity
//          assertTrue(actual == expected)
//        }, //No DDL equivalent
        testEvaluation("Pi")("nativeReferenceTests", "nativeReferencePiTest")(Data.Float(3)),
        testEval("ModBy")("nativeReferenceTests", "nativeReferenceModByTest", 7)(
          Data.Int(1)
        )
      ),
      suite("Morphir Types")(
        testEval("LocalDate")("nativeReferenceTests", "localDatePassthrough", localDate)(Data.LocalDate(localDate)),
        testEval("LocalDate")("nativeReferenceTests", "localTimePassthrough", localTime)(Data.LocalTime(localTime))
      ),
      suite("Pattern Matching")(
        testEvaluation("Wildcard")("patternMatchTests", "patternMatchWildcardTest")(Data.String("Correct")),
        testEvaluation("Tuple")("patternMatchTests", "patternMatchTupleTest")(Data.String("Correct")),
        testEvaluation("Constructor")("patternMatchTests", "patternMatchConstructorTest")(Data.String("Correct")),
        testEvaluation("Zero Arg Constructor")("patternMatchTests", "patternMatchEmptyListTest")(
          Data.String("Correct")
        ),
        testEvaluation("Head Tail")("patternMatchTests", "patternMatchHeadTailTest")(Data.Tuple(
          Data.String("Dog"),
          Data.String("Red")
        )),
        testEvaluation("Literal")("patternMatchTests", "patternMatchLiteralTest")(Data.String("Correct")),
        testEvaluation("Repeated As")("patternMatchTests", "patternMatchRepeatedAsTest")(Data.Tuple(
          Data.Int(2),
          Data.Tuple(Data.Int(1), Data.Int(2))
        ))
      ),
      suite("Records")(
        testEvaluation("Field")("recordTests", "recordFieldTest")(Data.String("Correct")),
        testEvaluation("Field from Bound Record")("recordTests", "recordFieldFromBoundTest")(Data.String("Correct")),
        testEvaluation("Field Function Apply")("recordTests", "fieldFunctionApplyTest")(Data.String("Correct")),
        testEvaluation("Field Function Apply Twice")("recordTests", "fieldFunctionApplyTwiceTest")(Data.Tuple(
          Data.Int(1),
          Data.Int(2)
        )),
        testEvaluation("Field")("recordTests", "fieldFunctionMapTest")(Data.List(
          Data.String("Soso"),
          Data.String("Ponyo"),
          Data.String("Odin")
        )),
        testEvaluation("Simple Record")("recordTests", "recordSimpleTest")(dogRecordData("Fido", 5)),
        testEvaluation("Nested Record")("recordTests", "recordNestedTest")(Data.Record(
          qn"Morphir/Examples/App:RecordTests:NestedRecordType",
          (Label("name"), Data.String("Dogs")),
          (
            Label("records"),
            Data.List(
              dogRecordData("Ponyo", 3),
              dogRecordData("Soso", 3)
            )
          )
        )),
        testEvaluation("Record Update Single Field")("recordTests", "updateRecordSimpleTest")(Data.String("Soso")),
        testEvaluation("Record Update Full")("recordTests", "updateRecordFullTest")(dogRecordData("Soso", 5)),
        testEvaluation("Record Updates are not mutation")("recordTests", "updateRecordImmutableTest")(
          Data.List(
            dogRecordData("Soso", 4),
            dogRecordData("Ponyo", 5)
          )
        )
      ),
      suite("Set")(
        testEvaluation("fromList")("setTests", "setFromListTest")(Data.Set(
          Data.Int(0),
          Data.Int(1),
          Data.Int(2),
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEvaluation("toList")("setTests", "setToListTest")(Data.List(
          Data.Int(0),
          Data.Int(1),
          Data.Int(2),
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEvaluation("member")("setTests", "setMemberTest1")(Data.Boolean(true)),
        testEvaluation("member")("setTests", "setMemberTest2")(Data.Boolean(false)),
        testEvaluation("size")("setTests", "setSizeTest")(Data.Int(3))
      ),
      suite("Simple")(
        testEvaluation("Unit")("simpleTests", "simpleUnitTest")(Data.Unit)
      ),
      suite("Tuple")(
        testEvaluation("Tuple(2)")("tupleTests", "tupleTwoTest")(Data.Tuple(List(Data.Int(5), Data.Int(4)))),
        testEvaluation("Tuple(3)")("tupleTests", "tupleThreeTest")(Data.Tuple(
          Data.Int(0),
          Data.Boolean(true),
          Data.String("Green")
        )),
        testEvaluation("Nested Tuple")("tupleTests", "tupleNestedTest")(Data.Tuple(
          Data.Int(5),
          Data.Tuple(
            Data.String("Four"),
            Data.Tuple(Data.Int(4), Data.String("Five"))
          )
        )),
        testEvaluation("First")("tupleTests", "tupleFirstTest")(Data.Int(1)),
        testEvaluation("Second")("tupleTests", "tupleSecondTest")(Data.Int(2)),
        testEval("Derive from Destructure Int")("tupleTests", "tupleDeriveDestructureTest", (1, "Red"))(Data.Int(1)),
        testEval("Derive from Destructure String")("tupleTests", "tupleDeriveDestructureTest", ("Red", 1))(
          Data.String("Red")
        )
      ),
      suite("String")(
        testEvalMultiple("append")("stringTests", "stringAppend", List(Data.String("Do"), Data.String("Bop")))(
          Data.String("DoBop")
        ),
        testEval("concat")(
          "stringTests",
          "stringConcat",
          List(
            "abc",
            "def",
            " ",
            "ghi"
          )
        )(
          Data.String("abcdef ghi")
        ),
        testEvalMultiple("contains true")(
          "stringTests",
          "stringContains",
          List("cat", "cataracts")
        )(
          Data.True
        ),
        testEvalMultiple("contains false")(
          "stringTests",
          "stringContains",
          List("dog", "cataracts")
        )(
          Data.False
        ),
        testEvalMultiple("dropLeft")(
          "stringTests",
          "stringDropLeft",
          List(3, "String")
        )(
          Data.String("ing")
        ),
        testEvalMultiple("dropRight")(
          "stringTests",
          "stringDropRight",
          List(3, "String")
        )(
          Data.String("Str")
        ),
        testEvalMultiple("endsWith true")(
          "stringTests",
          "stringEndsWith",
          List("ing", "Singing")
        )(
          Data.True
        ),
        testEvalMultiple("endsWith false")(
          "stringTests",
          "stringEndsWith",
          List("sing", "Singing")
        )(
          Data.False
        ),
        testEvalMultiple("join")(
          "stringTests",
          "stringJoin",
          List(", ", List("Apple", "Orange", "Yellow"))
        )(
          Data.String("Apple, Orange, Yellow")
        ),
        testEval("length")("StringTests", "stringLength", "Length Of String")(Data.Int(16)),
        testEvalMultiple("padLeft")(
          "stringTests",
          "stringPadLeft",
          List(3, 'm', " yum")
        )(
          Data.String("mmm yum")
        ),
        testEvalMultiple("padRight")(
          "stringTests",
          "stringPadRight",
          List(3, 'm', "yum")
        )(
          Data.String("yummmm")
        ),
        testEvalMultiple("slice")(
          "stringTests",
          "stringSlice",
          List(4, 11, "This is a complete sentence.")
        )(
          Data.String(" is a c")
        ),
        testEvalMultiple("startsWith true")(
          "stringTests",
          "stringStartsWith",
          List("Doctor", "Doctor Smith")
        )(
          Data.True
        ),
        testEvalMultiple("startsWith false")(
          "stringTests",
          "stringStartsWith",
          List("Mister", "Doctor Smith")
        )(
          Data.False
        ),
        testEval("toLower")(
          "stringTests",
          "stringToLower",
          "CAPITALIZED"
        )(
          Data.String("capitalized")
        ),
        testEval("toUpper")(
          "stringTests",
          "stringToUpper",
          "lowercased"
        )(
          Data.String("LOWERCASED")
        ),
        testEval("trim")(
          "stringTests",
          "stringTrim",
          "                   hello world        "
        )(
          Data.String("hello world")
        ),
        testEval("trimLeft")(
          "stringTests",
          "stringTrimLeft",
          "                   hello world        "
        )(
          Data.String("hello world        ")
        ),
        testEval("trimRight")(
          "stringTests",
          "stringTrimRight",
          "                   hello world        "
        )(
          Data.String("                   hello world")
        ),
        testEvaluation("left")("StringTests", "stringLeftTest")(Data.String("Mu")),
        testEvaluation("right")("StringTests", "stringRightTest")(Data.String("ly")),
        testEvaluation("fromInt")("StringTests", "stringFromIntTest")(Data.String("25")),
        testEvaluation("fromFloat")("StringTests", "stringFromFloatTest")(Data.String("1.5")),
        testEvaluation("toFloat")("StringTests", "stringToFloatTest1")(Data.Optional.Some(Data.Float(1.5))),
        testEvaluation("toFloat")("StringTests", "stringToFloatTest2")(Data.Optional.None(Concept.Float)),
        testEvaluation("toInt")("StringTests", "stringToIntTest1")(Data.Optional.Some(Data.Int(25))),
        testEvaluation("toInt")("StringTests", "stringToIntTest2")(Data.Optional.None(Concept.Int32)),
        testEvaluation("isEmpty")("StringTests", "stringIsEmptyTest1")(Data.Boolean(true)),
        testEvaluation("isEmpty")("StringTests", "stringIsEmptyTest2")(Data.Boolean(false))
      ),
      suite("References To user Defined Members")(
        testEvaluation("Reference to value")("userDefinedReferenceTests", "userDefinedReferenceValueTest")(Data.Int(5)),
        testEvaluation("Curried Function Application")("userDefinedReferenceTests", "userDefinedReferenceCurriedTest")(
          Data.String("Correct")
        ),
        testEvaluation("Simple Function Application")(
          "userDefinedReferenceTests",
          "userDefinedReferenceSimpleFunctionTest"
        )(Data.Tuple(List(
          Data.Int(1),
          Data.Int(2)
        ))),
        testEvaluation("Calling public function which calls private function")(
          "userDefinedReferenceTests",
          "userDefinedReferencePublicPrivateTest"
        )(Data.Int(10)),
        testEvaluation("Reference to Record")("userDefinedReferenceTests", "userDefinedReferenceRecordTest")(
          Data.String("Tom Tit Tot")
        ),
        testEvaluation("Reference to Union Type")("userDefinedReferenceTests", "userDefinedReferenceUnionTest")(
          Data.Int(-6)
        ),
        testEval("Reference to Union with type args")(
          "userDefinedReferenceTests",
          "typeArgUnionTest",
          (1, "Red")
        )(Data.Case(
          List(
            (EnumLabel.Named("arg1"), Data.Int(1)),
            (EnumLabel.Named("arg2"), Data.String("Red"))
          ),
          "Morphir.Examples.App:ExampleModule:aB",
          typeArgUnionShape(Concept.Int32, Concept.String)
        )) @@ ignore @@ tag("Failing because of non-matching order of union cases")
      ),
      suite("Type-based tests")(
        testEvalMultiple("Applies arguments in correct order")(
          "typeCheckerTests",
          "twoArgEntry",
          List(Data.Int(3), Data.String("Green"))
        )(Data.Tuple(Data.Int(3), Data.String("Green")))
      ),
      suite("Dictionary Tests")(
        testEvaluation("Returns a dictionary")("dictionaryTests", "dictFromListTest")(Data.Map(
          (Data.Int(1), Data.String("Red")),
          (Data.Int(2), Data.String("Blue")),
          (Data.Int(3), Data.String("Orange")),
          (Data.Int(4), Data.String("White")),
          (Data.Int(5), Data.String("Green"))
        )),
        testEvaluation("Converts a dictionary into a list")("dictionaryTests", "dictToListTest")(Data.List(
          Data.Tuple(Data.Int(1), Data.String("Red")),
          Data.Tuple(Data.Int(2), Data.String("Blue")),
          Data.Tuple(Data.Int(3), Data.String("Orange"))
        )),
        testEvaluation("Get")("dictionaryTests", "dictGetTest")(Data.Optional.Some(Data.String("Cat"))),
        testEvaluation("GetMissing")("dictionaryTests", "dictGetMissingTest")(Data.Optional.None(Concept.String)),
        testEval("MemberTrue")(
          "dictionaryTests",
          "dictMemberTest",
          Data.Map(
            (Data.String("Bob"), Data.Int(0)),
            (Data.String("Waldo"), Data.Int(1))
          )
        )(Data.Boolean(true)),
        testEval("MemberFalse")(
          "dictionaryTests",
          "dictMemberTest",
          Data.Map(
            (Data.String("Bob"), Data.Int(0))
          )
        )(Data.Boolean(false)),
        testEval("MemberEmpty")("dictionaryTests", "dictMemberTest", Data.Map.empty(Concept.String, Concept.Boolean))(
          Data.Boolean(false)
        ),
        testEval("Size 2")(
          "dictionaryTests",
          "dictSizeTest",
          Data.Map(
            (Data.String("Bob"), Data.Int(0)),
            (Data.String("Waldo"), Data.Int(1))
          )
        )(Data.Int(2)),
        testEval("Size Empty")("dictionaryTests", "dictSizeTest", Data.Map.empty(Concept.Integer, Concept.Boolean))(
          Data.Int(0)
        ),
        testEval("isEmpty false")(
          "dictionaryTests",
          "dictIsEmptyTest",
          Data.Map(
            (Data.String("Bob"), Data.Int(0)),
            (Data.String("Waldo"), Data.Int(1))
          )
        )(Data.Boolean(false)),
        testEval("isEmpty True")(
          "dictionaryTests",
          "dictIsEmptyTest",
          Data.Map.empty(Concept.Integer, Concept.Boolean)
        )(Data.Boolean(true)),
        testEvaluation("Filters a dictionary")("dictionaryTests", "dictFilterTest")(Data.Map(
          (Data.Int(3), Data.String("Blue")),
          (Data.Int(4), Data.String("Blue"))
        )),
        testEvaluation("Empty")("dictionaryTests", "dictEmptyTest")(Data.Map.empty(Concept.String, Concept.Int32)),
        testEvaluation("Singleton")("dictionaryTests", "dictSingletonTest")(Data.Map((
          Data.Int(6),
          Data.String("Puppies")
        ))),
        testEvaluation("Keys")("dictionaryTests", "dictKeysTest")(Data.List(
          Data.Int(1),
          Data.Int(2),
          Data.Int(3),
          Data.Int(4),
          Data.Int(5)
        )),
        testEval("Values")(
          "dictionaryTests",
          "dictValuesTest",
          Data.Map(
            (Data.String("Waldo"), Data.Int(0)),
            (Data.String("Bob"), Data.Int(1))
          )
        )(Data.List(
          Data.Int(0),
          Data.Int(1)
        )),
        testEval("Values Empty")("dictionaryTests", "dictValuesTest", Data.Map.empty(Concept.Integer, Concept.Boolean))(
          Data.List.empty(Concept.Boolean)
        ),
        testEvaluation("Update")("dictionaryTests", "dictUpdateTest")(Data.Map(
          (Data.String("Alice"), Data.Int(1)),
          (Data.String("Bob"), Data.Int(6))
        )),
        testEvaluation("Update - delete key")("dictionaryTests", "dictUpdateTest2")(Data.Map(
          (Data.String("Alice"), Data.Int(1))
        ))
      ),
      suite("Maybe Tests")(
        testEvaluation("Returns a Just 1")("maybeTests", "returnJustIntTest")(Data.Optional.Some(Data.Int(1))),
        testEvaluation("Option String")("maybeTests", "returnJustStringTest")(
          Data.Optional.Some(Data.String("Hello"))
        ),
        testEvaluation("Returns a None")("maybeTests", "returnNoneIntTest")(Data.Optional.None(Concept.Int32)),
        testEval("Match Just(Blue) input")("maybeTests", "matchInput", Some("Blue"))(Data.String("Blue")),
        testEval("Match Nothing input")("maybeTests", "matchInput", Data.Optional.None(Concept.String))(
          Data.String("Octarine")
        ),
        testEval("Map Just(Red) input")("maybeTests", "maybeMap", Some("Red"))(Data.String("Bright Red")),
        testEval("MapWithContext Just(Red) input")("maybeTests", "maybeMapWithContext", Some("Red"))(
          Data.Optional.Some(Data.String("Dark Red"))
        ),
        testEval("Map Nothing input")("maybeTests", "maybeMap", Data.Optional.None(Concept.String))(
          Data.String("Ultraviolet")
        ),
        testEval("withDefault Just(True) input")("maybeTests", "maybeWithDefault", Some(true))(Data.Boolean(true)),
        testEval("withDefault Nothing input")("maybeTests", "maybeWithDefault", Data.Optional.None(Concept.Boolean))(
          Data.Boolean(false)
        )
      ),
      suite("SDK Result Tests")(
        testEval("Returns success result")("resultTests", "returnResultType", 0)(Data.Result.Ok(
          Data.Int(0),
          resultStringIntShape
        )),
        testEval("Returns error result")("resultTests", "returnResultType", -1)(Data.Result.Err(
          Data.String("Negative"),
          resultStringIntShape
        )),
        testEval("Resolves success input")(
          "resultTests",
          "resolveResultType",
          Data.Result.Ok.withErrConcept(Data.Int(5), Concept.Boolean)
        )(Data.Int(5)),
        testEval("Resolves error input")(
          "resultTests",
          "resolveResultType",
          Data.Result.Err.withOkConcept(Data.Boolean(true), Concept.Integer)
        )(Data.Int(1)),
        testEval("Map Ok(Red) input")(
          "resultTests",
          "resultMap",
          Data.Result.Ok.withErrConcept(Data.String("Red"), Concept.String)
        )(Data.String("Bright Red")),
        testEval("Map Err(MyError) input")(
          "resultTests",
          "resultMap",
          Data.Result.Err.withOkConcept(Data.String("MyError"), Concept.String)
        )(
          Data.String("Error: MyError")
        ),
        testEval("MapWithContext Ok(Red) input")(
          "resultTests",
          "resultMapWithContext",
          Data.Result.Ok.withErrConcept(Data.String("Red"), Concept.Boolean)
        )(
          Data.Result.Ok.withErrConcept(Data.String("Dark Red"), Concept.Boolean)
        ),
        testEval("MapError Ok(Energy) input")(
          "resultTests",
          "resultMapError",
          Data.Result.Ok.withErrConcept(Data.String("Energy"), Concept.String)
        )(Data.String("Fine: Energy")),
        testEval("MapError Err(Matter) input")(
          "resultTests",
          "resultMapError",
          Data.Result.Err.withOkConcept(Data.String("Matter"), Concept.String)
        )(
          Data.String("Anti-Matter")
        ),
        testEval("withDefault Ok(True) input")(
          "resultTests",
          "resultWithDefault",
          Data.Result.Ok.withErrConcept(Data.Boolean(true), Concept.Boolean)
        )(Data.Boolean(true)),
        testEval("withDefault Err(true) input")(
          "resultTests",
          "resultWithDefault",
          Data.Result.Err.withOkConcept(Data.Boolean(true), Concept.Boolean)
        )(
          Data.Boolean(false)
        ),
        testEval("toMaybe Ok(Red) input")(
          "resultTests",
          "resultToMaybe",
          Data.Result.Ok.withErrConcept(Data.String("Red"), Concept.Boolean)
        )(Data.Optional.Some(Data.String("Red"))),
        testEval("toMaybe Err(true) input")(
          "resultTests",
          "resultToMaybe",
          Data.Result.Err.withOkConcept(Data.Boolean(true), Concept.String)
        )(Data.Optional.None(Concept.String)),
        testEval("fromMaybe Just 3 input")(
          "resultTests",
          "resultFromMaybe",
          Data.Optional.Some(Data.Int(3))
        )(Data.Result.Ok.withErrConcept(Data.Int(3), Concept.String)),
        testEval("fromMaybe Nothing input")(
          "resultTests",
          "resultFromMaybe",
          Data.Optional.None(Concept.Int32)
        )(Data.Result.Err.withOkConcept(Data.String("Undefined"), Concept.Int32))
      ),
      suite("SDK Basics Tests")(
        testEval("Ceiling")("sdkBasicsTests", "basicsCeilingTest", 3.88)(Data.Int(4)),
        testEval("Floor")("sdkBasicsTests", "basicsFloorTest", 3.88)(Data.Int(3)),
        testEval("Truncate")("sdkBasicsTests", "basicsTruncateTest", 1.2)(Data.Int(1)),
        testEval("Truncate 2")("sdkBasicsTests", "basicsTruncateTest", -1.2)(Data.Int(-1)),
        testEval("Truncate 3")("sdkBasicsTests", "basicsTruncateTest", .4)(Data.Int(0)),
        testEval("Truncate 4")("sdkBasicsTests", "basicsTruncateTest", -.4)(Data.Int(0)),
        testEvalMultiple("IntegerDivide")("sdkBasicsTests", "basicsIntegerDivideTest", List(12, 2))(
          Data.Int(6)
        ),
        testEvalMultiple("IntegerDivide 2")("sdkBasicsTests", "basicsIntegerDivideTest", List(12, 0))(
          Data.Int(0)
        ),
        testEval("Abs")("sdkBasicsTests", "basicsAbsTest", Data.Float(-5.0))(Data.Float(5.0)),
        testEval("Always")("sdkBasicsTests", "basicsAlwaysTest", 0)(Data.List(Data.Int(0))),
        testEval("Always 2")("sdkBasicsTests", "basicsAlwaysTest", Data.Char('z'))(Data.List(Data.Char('z'))),
        testEvalMultiple("Clamp")("sdkBasicsTests", "basicsClampTest", List(100, 200, 1000))(Data.Int(200)),
        testEvalMultiple("Clamp 2")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 50.0))(Data.Float(100.0)),
        testEvalMultiple("Clamp 3")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 150.0))(Data.Float(150.0)),
        testEvalMultiple("Clamp 4")("sdkBasicsTests", "basicsClampTest", List(100, 200, 150))(Data.Int(150)),
        testEvalMultiple("Power")("sdkBasicsTests", "basicsPowerTest", List(4.0, 5.0))(Data.Float(1024)),
        testEvalMultiple("Power")("sdkBasicsTests", "basicsPowerTest", List(4, 5))(Data.Int(1024)),
        testEvalMultiple("RemainderBy")("sdkBasicsTests", "basicsRemainderByTest", List(4, 21))(Data.Int(1)),
        testEvalMultiple("RemainderBy 2")("sdkBasicsTests", "basicsRemainderByTest", List(4, -21))(Data.Int(-1)),
        testEvalMultiple("RemainderBy 3")("sdkBasicsTests", "basicsRemainderByTest", List(0, 4))(
          Data.Int(0)
        ) @@ ignore @@ TestAspect.tag("remainderBy 0 throws"),
        testEval("Sqrt")("sdkBasicsTests", "basicsSqrtTest", Data.Float(9.0))(Data.Float(3.0)),
        testEval("Identity")("sdkBasicsTests", "basicsIdentityTest", Data.Float(-5.0))(Data.Float(-5.0)),
        testEvalMultiple("Xor")("sdkBasicsTests", "basicsXorTest", List(Data.Boolean(true), Data.Boolean(true)))(
          Data.Boolean(false)
        ),
        testEvalMultiple("Xor 2")("sdkBasicsTests", "basicsXorTest", List(Data.Boolean(true), Data.Boolean(false)))(
          Data.Boolean(true)
        ),
        testEvalMultiple("Xor 3")("sdkBasicsTests", "basicsXorTest", List(Data.Boolean(false), Data.Boolean(true)))(
          Data.Boolean(true)
        ),
        testEvalMultiple("Xor 4")("sdkBasicsTests", "basicsXorTest", List(Data.Boolean(false), Data.Boolean(false)))(
          Data.Boolean(false)
        ),
        testEvaluation("Plus")("sdkBasicsTests", "sdkAddTest")(Data.Int(3)),
        testEvaluation("Minus")("sdkBasicsTests", "sdkSubtractTest")(Data.Int(2)),
        testEval("Plus(64)")("sdkBasicsTests", "sdkAddTest64", abStruct(1L, 2L))(
          Data.Int64(3)
        ) @@ ignore @@ TestAspect.tag("Not properly typed"),
        testEval("Minus(64)")("sdkBasicsTests", "sdkSubtractTest64", abStruct(4L, 2L))(
          Data.Int64(2)
        ) @@ ignore @@ TestAspect.tag("Not properly typed"),
        testEvaluation("Divide")("sdkBasicsTests", "sdkDivideTest")(Data.Float(2.0)),
        testEvaluation("Multiply")("sdkBasicsTests", "sdkMultiplyIntTest")(Data.Int(20)),
        testEvaluation("Multiply")("sdkBasicsTests", "sdkMultiplyFloatTest")(Data.Float(20.0)),
        testEvaluation("Round")("sdkBasicsTests", "sdkRoundTest")(Data.Int(123)),
        testEvaluation("Round")("sdkBasicsTests", "sdkRoundTest2")(Data.Int(123)),
        testEvaluation("ModBy")("sdkBasicsTests", "sdkModByTest")(Data.Int(2)),
        testEvaluation("And")("sdkBasicsTests", "sdkAndTest")(Data.Boolean(false)),
        testEvaluation("x < y - True")("sdkBasicsTests", "sdkLessThanTestIntTrue")(Data.Boolean(true)),
        testEvaluation("x < y - False")("sdkBasicsTests", "sdkLessThanTestIntFalse")(Data.Boolean(false)),
        testEvaluation("x > y - True")("sdkBasicsTests", "sdkGreaterThanTestIntTrue")(Data.Boolean(true)),
        testEvaluation("x > y - False")("sdkBasicsTests", "sdkGreaterThanTestIntFalse")(Data.Boolean(false)),
        testEvaluation("x >= y - True A")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntTrue1")(Data.Boolean(true)),
        testEvaluation("x >= y - True B")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntTrue2")(Data.Boolean(true)),
        testEvaluation("x >= y - False")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntFalse")(Data.Boolean(false)),
        testEvaluation("x <= y - True A")("sdkBasicsTests", "sdkLessThanOrEqualTestIntTrue1")(Data.Boolean(true)),
        testEvaluation("x <= y - True B")("sdkBasicsTests", "sdkLessThanOrEqualTestIntTrue2")(Data.Boolean(true)),
        testEvaluation("x <= y - False")("sdkBasicsTests", "sdkLessThanOrEqualTestIntFalse")(Data.Boolean(false)),
        testEvaluation("ToFloat")("sdkBasicsTests", "toFloatTest")(Data.Float(2.0)),
        testEvaluation("Negate")("sdkBasicsTests", "sdkNegateTest")(Data.Int(-3)),
        testEvaluation("Negate")("sdkBasicsTests", "sdkNegateTest2")(Data.Int(3)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest2")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest3")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest4")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest5")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest6")(Data.Boolean(true)),
        testEvaluation("Equal")("sdkBasicsTests", "sdkEqualTest7")(Data.Boolean(true)),
        testEvaluation("Or")("sdkBasicsTests", "sdkOrTest")(Data.Boolean(true)),
        testEvaluation("Not")("sdkBasicsTests", "sdkNotTest")(Data.Boolean(false)),
        testEvaluation("LogBase")("sdkBasicsTests", "sdkLogBaseTest")(Data.Float(2.0)),
        testEvaluation("Plus overflow")("sdkBasicsTests", "sdkIntOverflowTest")(
          Data.Int(3)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEvaluation("Plus Float")("sdkBasicsTests", "sdkAddFloatTest")(
          Data.Decimal(3.0)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEvaluation("Multiply")("sdkBasicsTests", "sdkMultiplyTest")(Data.Int(6)) @@ ignore @@ TestAspect.tag(
          "Not Implemented yet"
        ),
        testEvaluation("Divide by 0")("sdkBasicsTests", "sdkDivideByZeroTest")(
          Data.Decimal(2.0)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEvaluation("LessThanFloat")("sdkBasicsTests", "sdkLessThanTestFloat")(
          Data.Boolean(true)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEvaluation("LessThanChar")("sdkBasicsTests", "sdkLessThanTestChar")(
          Data.Boolean(true)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet")
      )
    ).provideLayerShared(morphirRuntimeLayer)

  def abStruct(a: Long, b: Long) = Data.Struct(Label("a") -> Data.Int64(a), Label("b") -> Data.Int64(b))
}
