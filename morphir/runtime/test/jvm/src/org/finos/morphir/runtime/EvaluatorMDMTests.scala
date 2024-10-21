package org.finos.morphir.runtime

import org.finos.morphir.datamodel.Util.*
import org.finos.morphir.datamodel.*
import org.finos.morphir.ir.Type
import org.finos.morphir.naming.*
import org.finos.morphir.runtime.EvaluatorMDMTests.testExceptionMultiple
import org.finos.morphir.runtime.environment.MorphirEnv
import org.finos.morphir.runtime.MorphirRuntimeError.*
import org.finos.morphir.runtime.MorphirRuntimeError.RTValueToMDMError.ResultTypeMismatch
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
      case alreadyData: Data        => alreadyData
      case u: Unit                  => Deriver.toData(u)
      case b: Boolean               => Deriver.toData(b)
      case i: Int                   => Deriver.toData(i)
      case c: Char                  => Deriver.toData(c)
      case d: Double                => Deriver.toData(d)
      case s: String                => Deriver.toData(s)
      case ld: java.time.LocalDate  => Deriver.toData(ld)
      case m: java.time.Month       => Deriver.toData(m)
      case dow: java.time.DayOfWeek => Deriver.toData(dow)
      case lt: java.time.LocalTime  => Deriver.toData(lt)
      case list: List[_] =>
        val mapped = list.map(deriveData(_))
        Data.List(mapped.head, mapped.tail: _*)
      case map: Map[_, _] =>
        val pairs = map.toList.map { case (key, value) => (deriveData(key), deriveData(value)) }
        Data.Map(pairs.head, pairs.tail: _*)
      case set: Set[_] =>
        val derivedElements = set.toList.map(deriveData(_))
        Data.Set(derivedElements.head, derivedElements.tail: _*)
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

  def evaluateException(
      moduleName: String,
      functionName: String,
      values: List[Any]
  )(expected: Throwable => TestResult): ZIO[TypedMorphirRuntime, Throwable, TestResult] =
    runTest(moduleName, functionName, values).fold(
      throwable => expected(throwable),
      data => assertNever(s"Expected exception but test returned $data")
    )

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

  def testExceptionMultiple(label: String)(
      moduleName: String,
      functionName: String,
      values: List[Any]
  )(expected: Throwable => TestResult) =
    test(label) {
      evaluateException(moduleName, functionName, values)(expected)
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
  def alias(data: Data, alias: FQName) = {
    val concept = Concept.Alias(alias, data.shape)
    Data.Aliased(data, concept)
  }

  def opaqueIntShape: Concept.Enum = Concept.Enum(
    qn"Morphir/Examples/App:ExampleModule:OpaqueInt",
    List(
      Concept.Enum.Case(
        Label("Opaque"),
        List(
          (EnumLabel.Named("arg1"), Concept.Int32)
        )
      )
    )
  )
  def opaqueInt(i: Int): Data = Data.Case(
    List((EnumLabel.Named("arg1"), Data.Int(i))),
    "Opaque",
    opaqueIntShape
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
      suite("Aggregate")(
        testEvaluation("GroupBy")("aggregateTests", "aggregateGroupByTest")(
          Data.Map(
            (
              Data.String("k2_1"),
              Data.List(
                Data.Tuple(Data.String("k2_1"), Data.Int32(1)),
                Data.Tuple(Data.String("k2_1"), Data.Int32(2)),
                Data.Tuple(Data.String("k2_1"), Data.Int32(5)),
                Data.Tuple(Data.String("k2_1"), Data.Int32(6))
              )
            ),
            (
              Data.String("k2_2"),
              Data.List(
                Data.Tuple(Data.String("k2_2"), Data.Int32(3)),
                Data.Tuple(Data.String("k2_2"), Data.Int32(4)),
                Data.Tuple(Data.String("k2_2"), Data.Int32(7)),
                Data.Tuple(Data.String("k2_2"), Data.Int32(8))
              )
            )
          )
        ),
        testEvaluation("Map")("aggregateTests", "aggregateAggregateMapTest")(
          Data.List(
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(1.0)), Data.Float(10.0 / 1.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(2.0)), Data.Float(10.0 / 2.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(3.0)), Data.Float(10.0 / 3.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(4.0)), Data.Float(10.0 / 4.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(5.0)), Data.Float(26.0 / 5.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(6.0)), Data.Float(26.0 / 6.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(7.0)), Data.Float(26.0 / 7.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(8.0)), Data.Float(26.0 / 8.0))
          )
        ),
        testEvaluation("Map2")("aggregateTests", "aggregateAggregateMap2Test")(
          Data.List(
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(1.0)), Data.Float(10.0 * 4.0 / 1.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(2.0)), Data.Float(10.0 * 4.0 / 2.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(3.0)), Data.Float(10.0 * 4.0 / 3.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(4.0)), Data.Float(10.0 * 4.0 / 4.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(5.0)), Data.Float(26.0 * 8.0 / 5.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(6.0)), Data.Float(26.0 * 8.0 / 6.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(7.0)), Data.Float(26.0 * 8.0 / 7.0)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(8.0)), Data.Float(26.0 * 8.0 / 8.0))
          )
        ),
        testEvaluation("Map3")("aggregateTests", "aggregateAggregateMap3Test")(
          Data.List(
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(1.0)), Data.Float(10.0 * 4.0 / 1.0 + 1)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(2.0)), Data.Float(10.0 * 4.0 / 2.0 + 1)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(3.0)), Data.Float(10.0 * 4.0 / 3.0 + 1)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(4.0)), Data.Float(10.0 * 4.0 / 4.0 + 1)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(5.0)), Data.Float(26.0 * 8.0 / 5.0 + 5)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(6.0)), Data.Float(26.0 * 8.0 / 6.0 + 5)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(7.0)), Data.Float(26.0 * 8.0 / 7.0 + 5)),
            Data.Tuple(Data.Tuple(Data.String("k1_2"), Data.Float(8.0)), Data.Float(26.0 * 8.0 / 8.0 + 5))
          )
        ),
        testEvaluation("Map4")("aggregateTests", "aggregateAggregateMap4Test")(
          Data.List(
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(1.0)), Data.Float(10.0 * 4.0 / 1.0 + 1 + 2.5)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(2.0)), Data.Float(10.0 * 4.0 / 2.0 + 1 + 2.5)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(3.0)), Data.Float(10.0 * 4.0 / 3.0 + 1 + 2.5)),
            Data.Tuple(Data.Tuple(Data.String("k1_1"), Data.Float(4.0)), Data.Float(10.0 * 4.0 / 4.0 + 1 + 2.5))
          )
        ),
        testEvaluation("Count")("aggregateTests", "aggregateCountTest")(
          Data.List(
            Data.Float(4.0),
            Data.Float(5.0),
            Data.Float(6.0)
          )
        ),
        testEvaluation("SumOf")("aggregateTests", "aggregateSumOfTest")(
          Data.List(
            Data.Float(7.0),
            Data.Float(8.0),
            Data.Float(9.0)
          )
        ),
        testEvaluation("MinimumOf")("aggregateTests", "aggregateMinimumOfTest")(
          Data.List(
            Data.Float(2.0),
            Data.Float(3.0),
            Data.Float(4.0)
          )
        ),
        testEvaluation("MaximumOf")("aggregateTests", "aggregateMaximumOfTest")(
          Data.List(
            Data.Float(6.0),
            Data.Float(7.0),
            Data.Float(8.0)
          )
        ),
        testEvaluation("AverageOf")("aggregateTests", "aggregateAverageOfTest")(
          Data.List(
            Data.Float(3.0),
            Data.Float(4.0),
            Data.Float(5.0)
          )
        ),
        testEvaluation("WeightedAverageOf")("aggregateTests", "aggregateWeightedAverageOfTest")(
          Data.List(
            Data.Float(3.0),
            Data.Float(4.0),
            Data.Float(5.0)
          )
        ),
        testEvaluation("ByKey")("aggregateTests", "aggregateByKeyTest")(
          Data.List(
            Data.Float(3.0),
            Data.Float(3.0),
            Data.Float(3.0),
            Data.Float(2.0),
            Data.Float(2.0)
          )
        ),
        testEvaluation("WithFilter")("aggregateTests", "aggregateWithFilterTest")(
          Data.List(
            Data.Float(6.0),
            Data.Float(6.0),
            Data.Float(6.0),
            Data.Float(6.0),
            Data.Float(6.0),
            Data.Float(6.0)
          )
        )
      ),
      suite("Char")(
        testEval("isUpper true")("charTests", "charIsUpperTest", 'A')(Data.Boolean(true)),
        testEval("isUpper false")("charTests", "charIsUpperTest", 'w')(Data.Boolean(false)),
        testEval("isUpper false numeric")("charTests", "charIsUpperTest", '1')(Data.Boolean(false)),
        testEval("isUpper false symbol")("charTests", "charIsUpperTest", 'Σ')(
          Data.Boolean(false)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEval("isLower true")("charTests", "charIsLowerTest", 'w')(Data.Boolean(true)),
        testEval("isLower false")("charTests", "charIsLowerTest", 'A')(Data.Boolean(false)),
        testEval("isLower false numeric")("charTests", "charIsLowerTest", '0')(Data.Boolean(false)),
        testEval("isLower false symbol")("charTests", "charIsLowerTest", 'π')(
          Data.Boolean(false)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEval("isAlpha true lower")("charTests", "charIsAlphaTest", 'z')(Data.Boolean(true)),
        testEval("isAlpha true upper")("charTests", "charIsAlphaTest", 'A')(Data.Boolean(true)),
        testEval("isAlpha false")("charTests", "charIsAlphaTest", '1')(Data.Boolean(false)),
        testEval("isAlpha false symbol")("charTests", "charIsAlphaTest", 'π')(
          Data.Boolean(false)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEval("isAlpha false")("charTests", "charIsAlphaTest", '1')(Data.Boolean(false)),
        testEval("isAlphaNum true lower")("charTests", "charIsAlphaNumTest", 'z')(Data.Boolean(true)),
        testEval("isAlphaNum true upper")("charTests", "charIsAlphaNumTest", 'A')(Data.Boolean(true)),
        testEval("isAlphaNum true numeric")("charTests", "charIsAlphaNumTest", '1')(Data.Boolean(true)),
        testEval("isAlphaNum false symbol")("charTests", "charIsAlphaNumTest", 'π')(
          Data.Boolean(false)
        ) @@ ignore @@ TestAspect.tag("Not Implemented yet"),
        testEval("isDigit true")("charTests", "charIsDigitTest", '1')(Data.Boolean(true)),
        testEval("isDigit false")("charTests", "charIsDigitTest", 'A')(Data.Boolean(false)),
        testEval("isDigit false symbol")("charTests", "charIsDigitTest", 'π')(Data.Boolean(false)),
        testEval("isOctDigit true")("charTests", "charIsOctDigitTest", '1')(Data.Boolean(true)),
        testEval("isOctDigit false")("charTests", "charIsOctDigitTest", '8')(Data.Boolean(false)),
        testEval("isOctDigit false letter")("charTests", "charIsOctDigitTest", 'A')(Data.Boolean(false)),
        testEval("isOctDigit false symbol")("charTests", "charIsOctDigitTest", 'π')(Data.Boolean(false)),
        testEval("isHexDigit true")("charTests", "charIsHexDigitTest", '1')(Data.Boolean(true)),
        testEval("isHexDigit true upper case letter")("charTests", "charIsHexDigitTest", 'A')(Data.Boolean(true)),
        testEval("isHexDigit true lower case letter")("charTests", "charIsHexDigitTest", 'f')(Data.Boolean(true)),
        testEval("isHexDigit false")("charTests", "charIsHexDigitTest", 'g')(Data.Boolean(false)),
        testEval("isHexDigit false symbol")("charTests", "charIsHexDigitTest", 'π')(Data.Boolean(false)),
        testEval("toUpper")("charTests", "charToUpperTest", 'z')(Data.Char('Z')),
        testEval("toLower")("charTests", "charToLowerTest", 'Z')(Data.Char('z')),
        testEval("toLocaleUpper")("charTests", "charToLocaleUpperTest", 'z')(Data.Char('Z')),
        testEval("toLocaleLower")("charTests", "charToLocaleLowerTest", 'Z')(Data.Char('z'))
      ),
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
        },
        testEval("Implicit Constructor")("constructorTests", "implicitConstructorTest", "abcd")(
          Data.Record(
            FQName.fromString("Morphir.Examples.App:ConstructorTests:SomeRecord"),
            (Label("name"), Data.String("abcd")),
            (Label("number"), Data.Int(5))
          )
        )
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
        suite("conversion")(
          testEvaluation("fromFloat")("decimalTests", "decimalFromFloatTest")(Data.Decimal(1.2)),
          testEval("fromInt")("decimalTests", "decimalFromInt", 634)(Data.Decimal(634)),
          testEval("fromString good")("decimalTests", "decimalFromString", "523")(
            Data.Optional.Some(Data.Decimal(523))
          ),
          testEval("fromString bad")("decimalTests", "decimalFromString", "abcd")(
            Data.Optional.None(Concept.Decimal)
          ),
          testEvaluation("toFloat")("decimalTests", "decimalToFloatTest")(Data.Float(1.5)) @@ ignore @@ tag(
            "toFLoat does not have an Elm implementation but is listed in the IR... this is a bug"
          ),
          testEvaluation("toString")("decimalTests", "decimalToStringTest")(Data.String("-1"))
        ),
        suite("abs")(
          testEvaluation("positive value")("decimalTests", "decimalPositiveAbs")(Data.Decimal(1)),
          testEvaluation("negative value")("decimalTests", "decimalNegativeAbs")(Data.Decimal(100.243))
        ),
        testEvaluation("add")("decimalTests", "decimalAdd")(Data.Decimal(673.45)),
        testEvaluation("bps")("decimalTests", "decimalBps")(Data.Decimal(0.0463)),
//        testEvaluation("compare")("decimalTests", "decimalAdd")(Data.Decimal(673.45)),
        suite("shiftDecimal")(
          testEvalMultiple("shift left 2 places")(
            "decimalTests",
            "decimalShiftLeft",
            List(Data.Int(2), Data.Decimal(123.45))
          )(Data.Decimal(1.2345)),
          testEvalMultiple("shift left 3 places")(
            "decimalTests",
            "decimalShiftLeft",
            List(Data.Int(3), Data.Decimal(123.45))
          )(Data.Decimal(0.12345)),
          testEvalMultiple("shift left 4 places")(
            "decimalTests",
            "decimalShiftLeft",
            List(Data.Int(4), Data.Decimal(123.45))
          )(Data.Decimal(0.012345)),
          testEvalMultiple("shift right 2 places")(
            "decimalTests",
            "decimalShiftRight",
            List(Data.Int(2), Data.Decimal(12.345))
          )(Data.Decimal(1234.5)),
          testEvalMultiple("shift right 3 places")(
            "decimalTests",
            "decimalShiftRight",
            List(Data.Int(3), Data.Decimal(12.345))
          )(Data.Decimal(12345.0)),
          testEvalMultiple("shift right 4 places")(
            "decimalTests",
            "decimalShiftRight",
            List(Data.Int(4), Data.Decimal(12.345))
          )(Data.Decimal(123450.0))
        ),
        suite("creation")(
          testEval("hundred")("decimalTests", "decimalHundred", 123)(Data.Decimal(12300)),
          testEval("hundredth")("decimalTests", "decimalHundredth", 123)(Data.Decimal(BigDecimal("1.23"))),
          testEval("million")("decimalTests", "decimalMillion", 123)(Data.Decimal(123000000)),
          testEval("millionth")("decimalTests", "decimalMillionth", 123)(Data.Decimal(BigDecimal("0.000123"))),
          testEval("tenth")("decimalTests", "decimalTenth", 123)(Data.Decimal(BigDecimal("12.3"))),
          testEval("thousand")("decimalTests", "decimalThousand", 123)(Data.Decimal(123000)),
          testEval("thousandth")("decimalTests", "decimalThousandth", 123)(Data.Decimal(BigDecimal("0.123")))
        ),
        suite("div")(
          testEvaluation("div some")("decimalTests", "decimalGoodDiv")(Data.Optional.Some(Data.Decimal(1.8))),
          testEvaluation("div none")("decimalTests", "decimalBadDiv")(Data.Optional.None(Concept.Decimal)),
          testEvaluation("div with default")("decimalTests", "decimalDivWithDefault")(Data.Decimal(-7)),
          testEvaluation("div with default using default")("decimalTests", "decimalZeroDivWithDefault")(
            Data.Decimal(0)
          )
        ),
        suite("comparison")(
          testEvaluation("eq true")("decimalTests", "decimalTrueEq")(Data.Boolean(true)),
          testEvaluation("eq false")("decimalTests", "decimalFalseEq")(Data.Boolean(false)),
          testEvaluation("gt true")("decimalTests", "decimalTrueGt")(Data.Boolean(true)),
          testEvaluation("gt false")("decimalTests", "decimalFalseGt")(Data.Boolean(false)),
          testEvaluation("gte greater true")("decimalTests", "decimalTrueGte")(Data.Boolean(true)),
          testEvaluation("gte equals true")("decimalTests", "decimalTrueEqualGte")(Data.Boolean(true)),
          testEvaluation("gte false")("decimalTests", "decimalFalseGte")(Data.Boolean(false)),
          testEvaluation("lt true")("decimalTests", "decimalTrueLt")(Data.Boolean(true)),
          testEvaluation("lt false")("decimalTests", "decimalFalseLt")(Data.Boolean(false)),
          testEvaluation("lte less true")("decimalTests", "decimalTrueLte")(Data.Boolean(true)),
          testEvaluation("lte equal true")("decimalTests", "decimalTrueEqualLte")(Data.Boolean(true)),
          testEvaluation("lte false")("decimalTests", "decimalFalseLte")(Data.Boolean(false)),
          testEvaluation("neq true")("decimalTests", "decimalTrueNeq")(Data.Boolean(true)),
          testEvaluation("neq false")("decimalTests", "decimalFalseNeq")(Data.Boolean(false))
        ),
        testEvaluation("mul")("decimalTests", "decimalMul")(Data.Decimal(0.06927)),
        testEvaluation("neg")("decimalTests", "decimalNegate")(Data.Decimal(34.222)),
        testEvaluation("round down")("decimalTests", "decimalWholeRound")(Data.Decimal(322.0)),
        testEvaluation("round up")("decimalTests", "decimalNegativeRound")(Data.Decimal(-92.0)),
        testEvaluation("sub")("decimalTests", "decimalSub")(Data.Decimal(1.1)),
        testEvaluation("truncate")("decimalTests", "decimalTruncate")(Data.Decimal(1.0))
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
        testEvaluation("Map2")("listTests", "listMap2Test")(Data.List(
          Data.Int(6),
          Data.Int(8),
          Data.Int(10)
        )),
        testEvaluation("Map3")("listTests", "listMap3Test")(Data.List(
          Data.Int(9),
          Data.Int(12),
          Data.Int(15)
        )),
        testEvaluation("Map4")("listTests", "listMap4Test")(Data.List(
          Data.Int(12),
          Data.Int(16),
          Data.Int(20)
        )),
        testEvaluation("Map5")("listTests", "listMap5Test")(Data.List(
          Data.Int(15),
          Data.Int(20),
          Data.Int(25)
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
        testEvaluation("MapTest2")("listTests", "listMapTest2")(Data.List(
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
        suite("maximum")(
          testEvaluation("maximum returns value")("listTests", "listMaximumSomeTest")(
            Data.Optional.Some(Data.Int(3))
          ),
          testEvaluation("maximum returns none")("listTests", "listMaximumNoneTest")(
            Data.Optional.None(Concept.Int32)
          )
        ),
        suite("minimum")(
          testEvaluation("minimum returns value")("listTests", "listMinimumSomeTest")(
            Data.Optional.Some(Data.Int(-2))
          ),
          testEvaluation("minimum returns none")("listTests", "listMinimumNoneTest")(
            Data.Optional.None(Concept.Int32)
          )
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
        ),
        testEvaluation("intersperse")("listTests", "listIntersperseTest")(Data.List(
          Data.Int(2),
          Data.Int(1),
          Data.Int(3),
          Data.Int(1),
          Data.Int(4)
        )),
        testEvaluation("unzip")("listTests", "listUnzipTest")(Data.Tuple(
          Data.List(Data.Int(1), Data.Int(2)),
          Data.List(Data.String("a"), Data.String("b"))
        )),
        testEvaluation("innerJoin")("listTests", "listInnerJoinTest")(Data.List(
          Data.Tuple(
            Data.Tuple(Data.Int(2), Data.String("b")),
            Data.Tuple(Data.Int(2), Data.String("B"))
          )
        )),
        testEvaluation("leftJoin")("listTests", "listLeftJoinTest")(Data.List(
          Data.Tuple(
            Data.Tuple(Data.Int(1), Data.String("a")),
            Data.Optional.None(Concept.Tuple(List(Concept.Int32, Concept.String)))
          ),
          Data.Tuple(
            Data.Tuple(Data.Int(2), Data.String("b")),
            Data.Optional.Some(Data.Tuple(Data.Int(2), Data.String("B")))
          )
        )),
        suite("all")(
          testEval("predicate is true for all")("listTests", "listAllTest", List(1, 2, 3))(
            Data.Boolean(true)
          ),
          testEval("predicate is not true for all")("listTests", "listAllTest", List(2, 3, 4))(
            Data.Boolean(false)
          )
        ),
        suite("concatMap")(
          testEval("concatenates mapped result")("listTests", "listConcatMapTest", List(1, 2, 3))(
            Data.List(
              Data.Int(1),
              Data.Int(1),
              Data.Int(2),
              Data.Int(2),
              Data.Int(3),
              Data.Int(3)
            )
          ),
          testEval("produces empty list on empty list input")(
            "listTests",
            "listConcatMapTest",
            Data.List.empty(Concept.Int32)
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEval("produces flat output for a single result list")("listTests", "listConcatMapTest", List(3))(
            Data.List(Data.Int(3), Data.Int(3))
          )
        ),
        suite("drop")(
          testEvalMultiple("drops element from front of list")("listTests", "listDropTest", List(2, List(1, 2, 3)))(
            Data.List(Data.Int(3))
          ),
          testEvalMultiple("drops the entire list when the number dropped is gt the list length")(
            "listTests",
            "listDropTest",
            List(4, List(1, 2, 3))
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("returns an empty list when dropping from an empty list")(
            "listTests",
            "listDropTest",
            List(2, Data.List.empty(Concept.Int32))
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("does nothing when dropping 0 elements")("listTests", "listDropTest", List(0, List(1)))(
            Data.List(Data.Int(1))
          ),
          testEvalMultiple("does nothing when dropping a negative number of elements")(
            "listTests",
            "listDropTest",
            List(-5, List(1))
          )(
            Data.List(Data.Int(1))
          )
        ),
        suite("filterMap")(
          testEval("filters after mapping")("listTests", "listFilterMapTest", List(0, 1, 2))(
            Data.List(Data.Float(1.0), Data.Float(0.5))
          ),
          testEval("filters empty lists")("listTests", "listFilterMapTest", Data.List.empty(Concept.Int32))(
            Data.List.empty(Concept.Float)
          )
        ),
        suite("foldr")(
          testEval("folds")("listTests", "listFoldrTest", List(1, 2, 3))(
            Data.List(Data.Int32(1), Data.Int32(2), Data.Int32(3))
          ),
          testEval("folds empty lists")("listTests", "listFoldrTest", Data.List.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("sort")(
          testEval("sort list")("listTests", "listSortTest", List(3, 2, -2, 1, 0))(
            Data.List(Data.Int32(-2), Data.Int32(0), Data.Int32(1), Data.Int32(2), Data.Int32(3))
          ),
          testEval("sort same number")("listTests", "listSortTest", List(1, 1))(
            Data.List(Data.Int32(1), Data.Int32(1))
          ),
          testEval("sort single number")("listTests", "listSortTest", List(1))(
            Data.List(Data.Int32(1))
          ),
          testEval("sort empty list")("listTests", "listSortTest", Data.List.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("sortBy")(
          testEval("sortBy animal list")("listTests", "listSortByTest", List("mouse", "cat"))(
            Data.List(Data.String("cat"), Data.String("mouse"))
          ),
          testEval("sortBy same length list")("listTests", "listSortByTest", List("alice", "chuck", "bobby"))(
            Data.List(Data.String("alice"), Data.String("chuck"), Data.String("bobby"))
          ),
          testEval("sortBy single")("listTests", "listSortByTest", List("word"))(
            Data.List(Data.String("word"))
          ),
          testEval("sortBy empty list")("listTests", "listSortByTest", Data.List.empty(Concept.String))(
            Data.List.empty(Concept.String)
          )
        ),
        suite("sortWith")(
          testEval("sortWith backwards list")("listTests", "listSortWithTest", List(1, 2, 3, 4, 5))(
            Data.List(
              Data.Int(5),
              Data.Int(4),
              Data.Int(3),
              Data.Int(2),
              Data.Int(1)
            )
          ),
          testEval("sortWith single")("listTests", "listSortWithTest", List(-1))(
            Data.List(Data.Int(-1))
          ),
          testEval("sortWith empty list")("listTests", "listSortWithTest", Data.List.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("head")(
          testEval("head of a non-empty list is Just")("listTests", "listHeadTest", List(1, 2, 3))(
            Data.Optional.Some(Data.Int32(1))
          ),
          testEval("head of an empty list is Nothing")("listTests", "listHeadTest", Data.List.empty(Concept.Int32))(
            Data.Optional.None(Concept.Int32)
          )
        ),
        suite("indexedMap")(
          testEval("passes indices to the mapped function")("listTests", "listIndexedMapTest", List("a", "b", "c"))(
            Data.List(
              Data.Tuple(Data.Int(0), Data.String("a")),
              Data.Tuple(Data.Int(1), Data.String("b")),
              Data.Tuple(Data.Int(2), Data.String("c"))
            )
          ),
          testEval("maps empty lists")("listTests", "listIndexedMapTest", Data.List.empty(Concept.String))(
            Data.List.empty(Concept.Tuple(List(Concept.Int32, Concept.String)))
          )
        ),
        suite("member")(
          testEvalMultiple("finds a member of a list")("listTests", "listMemberTest", List(1, List(1, 2, 3)))(
            Data.Boolean(true)
          ),
          testEvalMultiple("doesn't find a member missing from a list")(
            "listTests",
            "listMemberTest",
            List(1, List(2, 3))
          )(
            Data.Boolean(false)
          ),
          testEvalMultiple("doesn't find a member of an empty list")(
            "listTests",
            "listMemberTest",
            List(1, Data.List.empty(Concept.Int32))
          )(
            Data.Boolean(false)
          )
        ),
        suite("range")(
          testEvalMultiple("creates a range")("listTests", "listRangeTest", List(1, 3))(
            Data.List(Data.Int(1), Data.Int(2), Data.Int(3))
          ),
          testEvalMultiple("creates a range including negative numbers")("listTests", "listRangeTest", List(-1, 2))(
            Data.List(Data.Int(-1), Data.Int(0), Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("creates a range with a single value")("listTests", "listRangeTest", List(1, 1))(
            Data.List(Data.Int(1))
          ),
          testEvalMultiple("creates an empty range for out of order arguments")(
            "listTests",
            "listRangeTest",
            List(2, 1)
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("creates an empty range for out of order negative arguments")(
            "listTests",
            "listRangeTest",
            List(-1, -2)
          )(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("repeat")(
          testEvalMultiple("creates a repeated list")("listTests", "listRepeatTest", List(3, 1))(
            Data.List(Data.Int(1), Data.Int(1), Data.Int(1))
          ),
          testEvalMultiple("creates an empty list for 0 repeats")("listTests", "listRepeatTest", List(0, 1))(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("creates an empty list for negative repeats")("listTests", "listRepeatTest", List(-1, 1))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("reverse")(
          testEval("reverses a list")("listTests", "listReverseTest", List(1, 2, 3))(
            Data.List(Data.Int(3), Data.Int(2), Data.Int(1))
          ),
          testEval("reverses an empty list")("listTests", "listReverseTest", Data.List.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("tail")(
          testEval("returns the tail of a list")("listTests", "listTailTest", List(1, 2, 3))(
            Data.Optional.Some(
              Data.List(
                Data.Int(2),
                Data.Int(3)
              )
            )
          ),
          testEval("returns the tail of a singleton list")("listTests", "listTailTest", List(1))(
            Data.Optional.Some(Data.List.empty(Concept.Int32))
          ),
          testEval("the tail of an empty list is Nothing")("listTests", "listTailTest", Data.List.empty(Concept.Int32))(
            Data.Optional.None(Concept.List(Concept.Int32))
          )
        ),
        suite("take")(
          testEvalMultiple("takes from a list")("listTests", "listTakeTest", List(2, List(1, 2, 3)))(
            Data.List(Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("taking 0 elements returns an empty list")(
            "listTests",
            "listTakeTest",
            List(0, List(1, 2, 3))
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("taking negative elements returns an empty list")(
            "listTests",
            "listTakeTest",
            List(-1, List(1, 2, 3))
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEvalMultiple("taking from an empty list returns an empty list")(
            "listTests",
            "listTakeTest",
            List(2, Data.List.empty(Concept.Int32))
          )(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("sum")(
          testEval("sum a list of ints returns sum of ints")("listTests", "listSumTest", List(1, 2))(
            Data.Int(3)
          ),
          testEvaluation("sum a list of floats returns sum of floats")("listTests", "listSumFloatTest")(
            Data.Float(3.0)
          ),
          testEval("sum a list of decimals returns sum of decimals")(
            "listTests",
            "listSumTest",
            Data.List(Data.Decimal(BigDecimal("1.0")), Data.Decimal(BigDecimal("2.0")))
          )(
            Data.Decimal(BigDecimal("3.0"))
          ),
          testEval("sum an empty list returns 0")(
            "listTests",
            "listSumTest",
            Data.List.empty(Concept.Int32)
          )(
            Data.Int(0)
          )
        ),
        suite("product")(
          testEval("multiply a list of ints returns product of ints")("listTests", "listProductTest", List(1, 2))(
            Data.Int(2)
          ),
          testEvaluation("sum a list of floats returns sum of floats")("listTests", "listProductFloatTest")(
            Data.Float(2.0)
          ),
          testEval("multiply a list of decimals returns product of decimals")(
            "listTests",
            "listProductTest",
            Data.List(Data.Decimal(BigDecimal("1.0")), Data.Decimal(BigDecimal("2.0")))
          )(
            Data.Decimal(BigDecimal("2.0"))
          ),
          testEval("multiply an empty list returns 0")(
            "listTests",
            "listProductTest",
            Data.List.empty(Concept.Int32)
          )(
            Data.Int(0)
          )
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
        suite("fromCalendarDate")(
          testEvalMultiple("fromCalendarDate valid")(
            "localDateTests",
            "fromCalendarDateTest",
            List(1900, java.time.Month.JANUARY, 20)
          )(
            Data.LocalDate(localDate)
          ),
          testEvalMultiple("fromCalendarDate clamped day, positive")(
            "localDateTests",
            "fromCalendarDateTest",
            List(1900, java.time.Month.JANUARY, 9999)
          )(
            Data.LocalDate(java.time.LocalDate.of(1900, 1, 31))
          ),
          testEvalMultiple("fromCalendarDate clamped day, negative")(
            "localDateTests",
            "fromCalendarDateTest",
            List(1900, java.time.Month.JANUARY, -9999)
          )(
            Data.LocalDate(java.time.LocalDate.of(1900, 1, 1))
          ),
          testEvalMultiple("fromCalendarDate clamped year, positive")(
            "localDateTests",
            "fromCalendarDateTest",
            List(java.time.Year.MAX_VALUE + 1, java.time.Month.JANUARY, 20)
          )(
            Data.LocalDate(java.time.LocalDate.of(java.time.Year.MAX_VALUE, 1, 20))
          ),
          testEvalMultiple("fromCalendarDate clamped year, negative")(
            "localDateTests",
            "fromCalendarDateTest",
            List(java.time.Year.MIN_VALUE - 1, java.time.Month.JANUARY, 20)
          )(
            Data.LocalDate(java.time.LocalDate.of(java.time.Year.MIN_VALUE, 1, 20))
          ),
          testEvalMultiple("fromCalendarDate clamped leap year day")(
            "localDateTests",
            "fromCalendarDateTest",
            List(1900, java.time.Month.FEBRUARY, 29)
          )(
            Data.LocalDate(java.time.LocalDate.of(1900, 2, 28))
          ),
          testEvalMultiple("fromCalendarDate valid leap year day")(
            "localDateTests",
            "fromCalendarDateTest",
            List(1904, java.time.Month.FEBRUARY, 29)
          )(
            Data.LocalDate(java.time.LocalDate.of(1904, 2, 29))
          )
        ),
        suite("Month enum support")(
          suite("as input")(
            testEvalMultiple("January")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.JANUARY, 20)
            )(
              Data.LocalDate(localDate)
            ),
            testEvalMultiple("February")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.FEBRUARY, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 2, 20))
            ),
            testEvalMultiple("March")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.MARCH, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 3, 20))
            ),
            testEvalMultiple("April")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.APRIL, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 4, 20))
            ),
            testEvalMultiple("May")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.MAY, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 5, 20))
            ),
            testEvalMultiple("June")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.JUNE, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 6, 20))
            ),
            testEvalMultiple("July")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.JULY, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 7, 20))
            ),
            testEvalMultiple("August")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.AUGUST, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 8, 20))
            ),
            testEvalMultiple("September")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.SEPTEMBER, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 9, 20))
            ),
            testEvalMultiple("October")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.OCTOBER, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 10, 20))
            ),
            testEvalMultiple("November")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.NOVEMBER, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 11, 20))
            ),
            testEvalMultiple("December")(
              "localDateTests",
              "fromCalendarDateTest",
              List(1900, java.time.Month.DECEMBER, 20)
            )(
              Data.LocalDate(java.time.LocalDate.of(1900, 12, 20))
            )
          ),
          suite("as output")(
            testEval("January")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 1, 20))(
              Data.Month(java.time.Month.JANUARY)
            ),
            testEval("February")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 2, 20))(
              Data.Month(java.time.Month.FEBRUARY)
            ),
            testEval("March")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 3, 20))(
              Data.Month(java.time.Month.MARCH)
            ),
            testEval("April")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 4, 20))(
              Data.Month(java.time.Month.APRIL)
            ),
            testEval("May")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 5, 20))(
              Data.Month(java.time.Month.MAY)
            ),
            testEval("June")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 6, 20))(
              Data.Month(java.time.Month.JUNE)
            ),
            testEval("July")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 7, 20))(
              Data.Month(java.time.Month.JULY)
            ),
            testEval("August")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 8, 20))(
              Data.Month(java.time.Month.AUGUST)
            ),
            testEval("September")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 9, 20))(
              Data.Month(java.time.Month.SEPTEMBER)
            ),
            testEval("October")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 10, 20))(
              Data.Month(java.time.Month.OCTOBER)
            ),
            testEval("November")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 11, 20))(
              Data.Month(java.time.Month.NOVEMBER)
            ),
            testEval("December")("localDateTests", "monthTest", java.time.LocalDate.of(1900, 12, 20))(
              Data.Month(java.time.Month.DECEMBER)
            )
          )
        ),
        suite("DayOfWeek enum support")(
          suite("as input")(
            testEval("Monday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.MONDAY)(Data.Int(1)),
            testEval("Tuesday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.TUESDAY)(Data.Int(2)),
            testEval("Wednesday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.WEDNESDAY)(Data.Int(3)),
            testEval("Thursday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.THURSDAY)(Data.Int(4)),
            testEval("Friday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.FRIDAY)(Data.Int(5)),
            testEval("Saturday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.SATURDAY)(Data.Int(6)),
            testEval("Sunday")("localDateTests", "dayOfWeekAsInputTest", java.time.DayOfWeek.SUNDAY)(Data.Int(7))
          ),
          suite("as output")(
            testEval("Monday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 15))(
              Data.DayOfWeek(java.time.DayOfWeek.MONDAY)
            ),
            testEval("Tuesday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 16))(
              Data.DayOfWeek(java.time.DayOfWeek.TUESDAY)
            ),
            testEval("Wednesday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 17))(
              Data.DayOfWeek(java.time.DayOfWeek.WEDNESDAY)
            ),
            testEval("Thursday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 18))(
              Data.DayOfWeek(java.time.DayOfWeek.THURSDAY)
            ),
            testEval("Friday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 19))(
              Data.DayOfWeek(java.time.DayOfWeek.FRIDAY)
            ),
            testEval("Saturday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 20))(
              Data.DayOfWeek(java.time.DayOfWeek.SATURDAY)
            ),
            testEval("Sunday")("localDateTests", "dayOfWeekTest", java.time.LocalDate.of(1900, 1, 21))(
              Data.DayOfWeek(java.time.DayOfWeek.SUNDAY)
            )
          )
        ),
        testEvalMultiple("addDays")("localDateTests", "addDaysTest", List(2, localDate))(
          Data.LocalDate(localDate.plusDays(2))
        ),
        testEvalMultiple("addWeeks")("localDateTests", "addWeeksTest", List(2, localDate))(
          Data.LocalDate(localDate.plusWeeks(2))
        ),
        testEvalMultiple("addYears")("localDateTests", "addYearsTest", List(2, localDate))(
          Data.LocalDate(localDate.plusYears(2))
        ),
        testEvalMultiple("diffInDays")("localDateTests", "diffInDaysTest", List(localDate, localDate.plusDays(999)))(
          Data.Int(999)
        ),
        testEvalMultiple("diffInWeeks")("localDateTests", "diffInWeeksTest", List(localDate, localDate.plusWeeks(2)))(
          Data.Int(2)
        ),
        testEvalMultiple("diffInMonths")(
          "localDateTests",
          "diffInMonthsTest",
          List(localDate, localDate.plusMonths(2))
        )(
          Data.Int(2)
        ),
        testEvalMultiple("diffInYears")("localDateTests", "diffInYearsTest", List(localDate, localDate.plusYears(2)))(
          Data.Int(2)
        ),
        testEvalMultiple("diffInYearsAdjacent")(
          "localDateTests",
          "diffInYearsTest",
          List(java.time.LocalDate.of(1900, 12, 31), java.time.LocalDate.of(1901, 1, 1))
        )(
          Data.Int(1)
        ),
        testEvalMultiple("diffInYearsSameYear")(
          "localDateTests",
          "diffInYearsTest",
          List(java.time.LocalDate.of(1900, 12, 31), java.time.LocalDate.of(1900, 1, 1))
        )(
          Data.Int(0)
        ),
        testEvalMultiple("addMonths")("localDateTests", "addMonthsTest", List(2, localDate))(
          Data.LocalDate(localDate.plusMonths(2))
        ),
        testEval("monthToInt")("localDateTests", "monthToIntTest", Data.Month(java.time.Month.JANUARY))(Data.Int(1)),
        testEval("isWeekend")("localDateTests", "isWeekendTest", localDate)(Data.Boolean(true)),
        testEval("isWeekday")("localDateTests", "isWeekdayTest", localDate)(Data.Boolean(false)),
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
        ),
        testEval("year")("localDateTests", "yearTest", localDate)(Data.Int(1900)),
        testEval("month")("localDateTests", "monthTest", localDate)(Data.Month(java.time.Month.JANUARY)),
        testEval("monthNumber")("localDateTests", "monthNumberTest", localDate)(Data.Int(1)),
        testEval("day")("localDateTests", "dayTest", localDate)(Data.Int(20)),
        testEval("dayOfWeek")("localDateTests", "dayOfWeekTest", localDate)(
          Data.DayOfWeek(java.time.DayOfWeek.SATURDAY)
        ),
        testEval("toISOString")("localDateTests", "toISOStringTest", localDate)(Data.String("1900-01-20"))
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
        testEvalMultiple("diffInHours")("localTimeTests", "diffInHoursTest", List(localTime, localTime.plusHours(2)))(
          Data.Int(-2)
        ),
        testEvalMultiple("diffInHours negative")(
          "localTimeTests",
          "diffInHoursTest",
          List(localTime, localTime.minusHours(2))
        )(
          Data.Int(2)
        ),
        testEvalMultiple("diffInMinutes")(
          "localTimeTests",
          "diffInMinutesTest",
          List(localTime, localTime.plusMinutes(2))
        )(
          Data.Int(-2)
        ),
        testEvalMultiple("diffInMinutes negative")(
          "localTimeTests",
          "diffInMinutesTest",
          List(localTime, localTime.minusMinutes(2))
        )(
          Data.Int(2)
        ),
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
        testEvaluation("Pi")("nativeReferenceTests", "nativeReferencePiTest")(Data.Float(scala.math.Pi)),
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
        testEvaluation("size")("setTests", "setSizeTest")(Data.Int(3)),
        suite("foldr")(
          testEval("folds left")("setTests", "setFoldrTest", Set(1, 2, 3))(
            Data.List(
              Data.Int(1),
              Data.Int(2),
              Data.Int(3)
            )
          ),
          testEval("iterates in desc sort order, not insertion order")("setTests", "setFoldrTest", Set(2, 1, 3))(
            Data.List(
              Data.Int(1),
              Data.Int(2),
              Data.Int(3)
            )
          ),
          testEval("folds empty sets")("setTests", "setFoldrTest", Data.Set.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("foldl")(
          testEval("folds left")("setTests", "setFoldlTest", Set(1, 2, 3))(
            Data.List(
              Data.Int(3),
              Data.Int(2),
              Data.Int(1)
            )
          ),
          testEval("iterates in asc sort order, not insertion order")("setTests", "setFoldlTest", Set(2, 1, 3))(
            Data.List(
              Data.Int(3),
              Data.Int(2),
              Data.Int(1)
            )
          ),
          testEval("folds empty sets")("setTests", "setFoldlTest", Data.Set.empty(Concept.Int32))(
            Data.List.empty(Concept.Int32)
          )
        ),
        suite("filter")(
          testEval("filter")("setTests", "setFilterTest", Set(-1, 1, 3, -100, 2))(
            Data.Set(
              Data.Int(1),
              Data.Int(2),
              Data.Int(3)
            )
          ),
          testEval("filters empty sets")("setTests", "setFilterTest", Data.Set.empty(Concept.Int32))(
            Data.Set.empty(Concept.Int32)
          )
        ),
        suite("insert")(
          testEvalMultiple("inserts into a set")("setTests", "setInsertTest", List(2, Set(1)))(
            Data.Set(Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("inserts into an empty set")(
            "setTests",
            "setInsertTest",
            List(1, Data.Set.empty(Concept.Int32))
          )(
            Data.Set(Data.Int(1))
          ),
          testEvalMultiple("leaves a set already containing the element unchanged")(
            "setTests",
            "setInsertTest",
            List(1, Set(1))
          )(
            Data.Set(Data.Int(1))
          )
        ),
        testEval("creates singleton sets")("setTests", "setSingletonTest", 1)(Data.Set(Data.Int(1))),
        suite("union")(
          testEvalMultiple("unions two sets")("setTests", "setUnionTest", List(Set(1), Set(2)))(
            Data.Set(Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("unions two sets with redundant elements")(
            "setTests",
            "setUnionTest",
            List(Set(1, 2), Set(2, 3))
          )(
            Data.Set(Data.Int(1), Data.Int(2), Data.Int(3))
          ),
          testEvalMultiple("unions when the first set is empty")(
            "setTests",
            "setUnionTest",
            List(Data.Set.empty(Concept.Int32), Set(1, 2))
          )(
            Data.Set(Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("unions when the second set is empty")(
            "setTests",
            "setUnionTest",
            List(Set(1, 2), Data.Set.empty(Concept.Int32))
          )(
            Data.Set(Data.Int(1), Data.Int(2))
          ),
          testEvalMultiple("unions when both sets are empty")(
            "setTests",
            "setUnionTest",
            List(Data.Set.empty(Concept.Int32), Data.Set.empty(Concept.Int32))
          )(
            Data.Set.empty(Concept.Int32)
          )
        ),
        suite("intersect")(
          testEvalMultiple("intersects two sets")("setTests", "setIntersectTest", List(Set(1, 2, 3), Set(2, 3, 4)))(
            Data.Set(Data.Int(2), Data.Int(3))
          ),
          testEvalMultiple("intersects two sets having no common elements")(
            "setTests",
            "setIntersectTest",
            List(Set(1, 2), Set(3, 4))
          )(
            Data.Set.empty(Concept.Int32)
          ),
          testEvalMultiple("intersects when the first set is empty")(
            "setTests",
            "setIntersectTest",
            List(Data.Set.empty(Concept.Int32), Set(1, 2))
          )(
            Data.Set.empty(Concept.Int32)
          ),
          testEvalMultiple("intersects when the second set is empty")(
            "setTests",
            "setIntersectTest",
            List(Set(1, 2), Data.Set.empty(Concept.Int32))
          )(
            Data.Set.empty(Concept.Int32)
          ),
          testEvalMultiple("intersects when both sets are empty")(
            "setTests",
            "setIntersectTest",
            List(Data.Set.empty(Concept.Int32), Data.Set.empty(Concept.Int32))
          )(
            Data.Set.empty(Concept.Int32)
          )
        ),
        suite("isEmpty")(
          testEval("false for non-empty sets")("setTests", "setIsEmptyTest", Set(1))(
            Data.Boolean(false)
          ),
          testEval("true for empty sets")("setTests", "setIsEmptyTest", Data.Set.empty(Concept.Int32))(
            Data.Boolean(true)
          )
        ),
        suite("map")(
          testEval("maps a function over a set")("setTests", "setMapTest", Set(1, 2, 3))(
            Data.Set(Data.Int(2), Data.Int(4), Data.Int(6))
          ),
          testEval("maps over empty sets")("setTests", "setMapTest", Data.Set.empty(Concept.Int32))(
            Data.Set.empty(Concept.Int32)
          )
        ),
        suite("partition")(
          testEval("partitions a set")("setTests", "setPartitionTest", Set(1, 2, 3))(
            Data.Tuple(
              Data.Set(Data.Int(1)),
              Data.Set(Data.Int(2), Data.Int(3))
            )
          ),
          testEval("partitions a set when all entries match")("setTests", "setPartitionTest", Set(0, 1))(
            Data.Tuple(
              Data.Set(Data.Int(0), Data.Int(1)),
              Data.Set.empty(Concept.Int32)
            )
          ),
          testEval("partitions a set when no entries match")("setTests", "setPartitionTest", Set(2, 3))(
            Data.Tuple(
              Data.Set.empty(Concept.Int32),
              Data.Set(Data.Int(2), Data.Int(3))
            )
          ),
          testEval("partitions an empty set")("setTests", "setPartitionTest", Data.Set.empty(Concept.Int32))(
            Data.Tuple(
              Data.Set.empty(Concept.Int32),
              Data.Set.empty(Concept.Int32)
            )
          )
        ),
        suite("remove")(
          testEvalMultiple("removes from a set")("setTests", "setRemoveTest", List(2, Set(1, 2, 3)))(
            Data.Set(Data.Int(1), Data.Int(3))
          ),
          testEvalMultiple("dosen't remove elements that are not in a set")(
            "setTests",
            "setRemoveTest",
            List(2, Set(1))
          )(
            Data.Set(Data.Int(1))
          ),
          testEvalMultiple("removes nothing from an empty set")(
            "setTests",
            "setRemoveTest",
            List(2, Data.Set.empty(Concept.Int32))
          )(
            Data.Set.empty(Concept.Int32)
          )
        )
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
        testEvalMultiple("repeat")(
          "stringTests",
          "stringRepeat",
          List(2, "Whomp")
        )(
          Data.String("WhompWhomp")
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
        testEvalMultiple("split sentence")(
          "stringTests",
          "stringSplit",
          List(" ", "This is a complete sentence.")
        )(
          Data.List(
            Data.String("This"),
            Data.String("is"),
            Data.String("a"),
            Data.String("complete"),
            Data.String("sentence.")
          )
        ),
        testEvalMultiple("split phrase")(
          "stringTests",
          "stringSplit",
          List("complete", "This is a complete sentence.")
        )(
          Data.List(
            Data.String("This is a "),
            Data.String(" sentence.")
          )
        ),
        testEvalMultiple("split letter")(
          "stringTests",
          "stringSplit",
          List("o", "foo")
        )(
          Data.List(
            Data.String("f")
          )
        ),
        testEvalMultiple("split no separator")(
          "stringTests",
          "stringSplit",
          List("", "foo")
        )(
          Data.List(
            Data.String("f"),
            Data.String("o"),
            Data.String("o")
          )
        ),
        testEvalMultiple("split no strings")(
          "stringTests",
          "stringSplit",
          List("", "")
        )(
          Data.List(
            Data.String("")
          )
        ),
        testEvalMultiple("split .")(
          "stringTests",
          "stringSplit",
          List(".", "1.2.3.4")
        )(
          Data.List(
            Data.String("1"),
            Data.String("2"),
            Data.String("3"),
            Data.String("4")
          )
        ),
        testEvalMultiple("split {")(
          "stringTests",
          "stringSplit",
          List("{", "{1}")
        )(
          Data.List(
            Data.String(""),
            Data.String("1}")
          )
        ),
        testEvalMultiple("split .{")(
          "stringTests",
          "stringSplit",
          List(".{", "ab.{c.{d")
        )(
          Data.List(
            Data.String("ab"),
            Data.String("c"),
            Data.String("d")
          )
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
        testEvaluation("toFloat")("StringTests", "stringGoodToFloatTest")(Data.Optional.Some(Data.Float(1.5))),
        testEvaluation("toFloat")("StringTests", "stringBadToFloatTest")(Data.Optional.None(Concept.Float)),
        testEvaluation("toInt")("StringTests", "stringToIntTest1")(Data.Optional.Some(Data.Int(25))),
        testEvaluation("toInt")("StringTests", "stringToIntTest2")(Data.Optional.None(Concept.Int32)),
        testEvaluation("isEmpty")("StringTests", "stringIsEmptyTest1")(Data.Boolean(true)),
        testEvaluation("isEmpty")("StringTests", "stringIsEmptyTest2")(Data.Boolean(false)),
        testEvaluation("fromChar")("StringTests", "stringFromCharTest")(Data.String("a")),
        testEvaluation("cons")("StringTests", "stringConsTest")(Data.String("abc")),
        testEval("uncons")("StringTests", "stringUnconsTest", "abc")(Data.Optional.Some(Data.Tuple(
          Data.Char('a'),
          Data.String("bc")
        ))),
        testEval("unconsSingleChar")("StringTests", "stringUnconsTest", "a")(Data.Optional.Some(Data.Tuple(
          Data.Char('a'),
          Data.String("")
        ))),
        testEval("unconsEmpty")("StringTests", "stringUnconsTest", "")(Data.Optional.None(Concept.Tuple(
          List(Concept.Char, Concept.String)
        ))),
        testEvaluation("toList")("StringTests", "stringToListTest")(Data.List(
          Data.Char('a'),
          Data.Char('b'),
          Data.Char('c')
        )),
        testEvaluation("fromList")("StringTests", "stringFromListTest")(Data.String("abc")),
        testEvaluation("fromListEmpty")("StringTests", "stringFromListEmptyTest")(Data.String("")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "1"))(Data.String("  1  ")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "11"))(Data.String("  11 ")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "121"))(Data.String(" 121 ")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "1234"))(Data.String(" 1234")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "12345"))(Data.String("12345")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, "123456"))(Data.String("123456")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(0, "123"))(Data.String("123")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(-5, "123"))(Data.String("123")),
        testEvalMultiple("pad")("StringTests", "stringPadTest", List(5, ""))(Data.String("     ")),
        testEvaluation("map")("StringTests", "stringMapTest")(Data.String("a.b.c")),
        testEvaluation("filter")("StringTests", "stringFilterTest")(Data.String("bc")),
        testEval("foldl")("StringTests", "stringFoldlTest", "UPPERCASE")(Data.Boolean(true)),
        testEval("foldl")("StringTests", "stringFoldlTest", "lowercase")(Data.Boolean(false)),
        testEval("foldl")("StringTests", "stringFoldlTest", "camelCase")(Data.Boolean(false)),
        testEval("foldl")("StringTests", "stringFoldlTest2", "time")(Data.String("emit")),
        testEval("foldr")("StringTests", "stringFoldrTest", "Hello, World")(Data.Int(2)),
        testEval("foldr")("StringTests", "stringFoldrTest", "HELLO, WORLD")(Data.Int(10)),
        testEval("foldr")("StringTests", "stringFoldrTest2", "time")(Data.String("time")),
        testEval("any")("StringTests", "stringAnyTest", "scala")(Data.Boolean(true)),
        testEval("any")("StringTests", "stringAnyTest", "elm")(Data.Boolean(false)),
        testEval("all")("StringTests", "stringAllTest", "aaa")(Data.Boolean(true)),
        testEval("all")("StringTests", "stringAllTest", "abc")(Data.Boolean(false))
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
        )(Data.Tuple(Data.Int(3), Data.String("Green"))),
        testEvalMultiple("Returns opaque types")(
          "typeCheckerTests",
          "returnOpaque",
          List(Data.Int(3))
        )(opaqueInt(3)),
        testEvalMultiple("Returns opaque types")(
          "typeCheckerTests",
          "acceptOpaque",
          List(opaqueInt(2))
        )(Data.Int(2)),
        testEvalMultiple("Aliased opaques also fine")(
          "typeCheckerTests",
          "aliasedOpaqueTest",
          List(opaqueInt(2))
        )(alias(opaqueInt(3), FQName.fromString("Morphir.Examples.App:TypeCheckerTests:AliasedOpaque")))
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
        )),
        suite("Partition")(
          testEvaluation("partitions")("dictionaryTests", "dictPartitionTest")(
            Data.Tuple(
              Data.Map(
                Data.String("Bob") -> Data.Int(1)
              ),
              Data.Map(
                Data.String("Waldo") -> Data.Int(0)
              )
            )
          ),
          testEvaluation("partitions an empty dict")("dictionaryTests", "dictPartitionEmptyTest")(
            Data.Tuple(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          ),
          testEvaluation("with partitioning result reversed")("dictionaryTests", "dictPartitionInversePredTest")(
            Data.Tuple(
              Data.Map(
                Data.String("Waldo") -> Data.Int(0)
              ),
              Data.Map(
                Data.String("Bob") -> Data.Int(1)
              )
            )
          ),
          testEvaluation("partitions when all entries match pred")("dictionaryTests", "dictPartitionAllMatchTest")(
            Data.Tuple(
              Data.Map(
                Data.String("Waldo") -> Data.Int(0),
                Data.String("Bob")   -> Data.Int(1)
              ),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          ),
          testEvaluation("partitions when no entries match pred")("dictionaryTests", "dictPartitionNoneMatchTest")(
            Data.Tuple(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map(
                Data.String("Bob")   -> Data.Int(1),
                Data.String("Waldo") -> Data.Int(0)
              )
            )
          ),
          testEvaluation("partitions with predicate based on key and value")(
            "dictionaryTests",
            "dictPartitionPredicateOperatesOnKeyAndValueTest"
          )(
            Data.Tuple(
              Data.Map(
                Data.String("Bob") -> Data.Int(1),
                Data.String("Lob") -> Data.Int(1)
              ),
              Data.Map(
                Data.String("Waldo") -> Data.Int(0),
                Data.String("Rob")   -> Data.Int(0)
              )
            )
          )
        ),
        suite("remove")(
          testEvalMultiple("removes entries")("dictionaryTests", "dictRemoveTest", List("a", Map("a" -> 1, "b" -> 2)))(
            Data.Map(Data.String("b") -> Data.Int(2))
          ),
          testEvalMultiple("no changes for missing entries")(
            "dictionaryTests",
            "dictRemoveTest",
            List("q", Map("a" -> 1, "b" -> 2))
          )(
            Data.Map(
              Data.String("a") -> Data.Int(1),
              Data.String("b") -> Data.Int(2)
            )
          ),
          testEvalMultiple("removes entries from empty dicts")(
            "dictionaryTests",
            "dictRemoveTest",
            List("a", Data.Map.empty(Concept.String, Concept.Int32))
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          )
        ),
        suite("diff")(
          testEvalMultiple("returns entries in the first dict but not the second")(
            "dictionaryTests",
            "dictDiffTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 2, "c" -> 3)
            )
          )(
            Data.Map(Data.String("a") -> Data.Int(1))
          ),
          testEvalMultiple("returns nothing when the first dict is empty")(
            "dictionaryTests",
            "dictDiffTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Map("b" -> 2, "c" -> 3)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("returns first dict when the second dict is empty")(
            "dictionaryTests",
            "dictDiffTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2))
          ),
          testEvalMultiple("returns nothing when both dicts are empty")(
            "dictionaryTests",
            "dictDiffTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("diff mutation test")(
            "dictionaryTests",
            "dictDiffMutateTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 2, "c" -> 3)
            )
          )(
            Data.List(
              Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2)),
              Data.Map(Data.String("b") -> Data.Int(2), Data.String("c") -> Data.Int(3))
            )
          )
        ),
        suite("intersect")(
          testEvalMultiple("returns entry from first dict when key is found in second dict")(
            "dictionaryTests",
            "dictIntersectTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 3, "c" -> 3)
            )
          )(
            Data.Map(Data.String("b") -> Data.Int(2))
          ),
          testEvalMultiple("returns nothing when the first dict is empty")(
            "dictionaryTests",
            "dictIntersectTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Map("b" -> 2, "c" -> 3)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("returns nothing when the second dict is empty")(
            "dictionaryTests",
            "dictIntersectTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("returns nothing when both dicts are empty")(
            "dictionaryTests",
            "dictIntersectTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("intersect mutation test")(
            "dictionaryTests",
            "dictIntersectMutateTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 3, "c" -> 3)
            )
          )(
            Data.List(
              Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2)),
              Data.Map(Data.String("b") -> Data.Int(3), Data.String("c") -> Data.Int(3))
            )
          )
        ),
        suite("union")(
          testEvalMultiple("returns entries in either dict, for collision: preference is given to first dict")(
            "dictionaryTests",
            "dictUnionTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 4, "c" -> 3)
            )
          )(
            Data.Map(
              Data.String("a") -> Data.Int(1),
              Data.String("b") -> Data.Int(2),
              Data.String("c") -> Data.Int(3)
            )
          ),
          testEvalMultiple("returns entries in the first dict when the second is empty")(
            "dictionaryTests",
            "dictUnionTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map(
              Data.String("a") -> Data.Int(1),
              Data.String("b") -> Data.Int(2)
            )
          ),
          testEvalMultiple("returns entries in the second dict when the first is empty")(
            "dictionaryTests",
            "dictUnionTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Map("b" -> 2, "c" -> 3)
            )
          )(
            Data.Map(
              Data.String("b") -> Data.Int(2),
              Data.String("c") -> Data.Int(3)
            )
          ),
          testEvalMultiple("returns nothing when both dicts are empty")(
            "dictionaryTests",
            "dictUnionTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("union mutation test")(
            "dictionaryTests",
            "dictUnionMutateTest",
            List(
              Map("a" -> 1, "b" -> 2),
              Map("b" -> 4, "c" -> 3)
            )
          )(
            Data.List(
              Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2)),
              Data.Map(Data.String("b") -> Data.Int(4), Data.String("c") -> Data.Int(3))
            )
          )
        ),
        suite("foldl")(
          testEval("folds left")("dictionaryTests", "dictFoldlTest", Map("a" -> 1, "b" -> 2))(
            Data.Int32(3)
          ),
          testEval("folds left alternate")("dictionaryTests", "dictFoldlTest2", Map("a" -> 1, "b" -> 2))(
            Data.List(Data.Int32(2), Data.Int32(1))
          ),
          testEval("folds left empty")(
            "dictionaryTests",
            "dictFoldlTest",
            Data.Map.empty(Concept.String, Concept.Int32)
          )(
            Data.Int32(0)
          ),
          testEval("folds left empty alternate")(
            "dictionaryTests",
            "dictFoldlTest2",
            Data.Map.empty(Concept.String, Concept.Int32)
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEval("foldl mutation test")("dictionaryTests", "dictFoldlMutateTest", Map("a" -> 1, "b" -> 2))(
            Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2))
          )
        ),
        suite("foldr")(
          testEval("folds right")("dictionaryTests", "dictFoldrTest", Map("a" -> 1, "b" -> 2))(
            Data.Int32(3)
          ),
          testEval("folds right alternate")("dictionaryTests", "dictFoldrTest2", Map("a" -> 1, "b" -> 2))(
            Data.List(Data.Int32(1), Data.Int32(2))
          ),
          testEval("folds right empty")(
            "dictionaryTests",
            "dictFoldrTest",
            Data.Map.empty(Concept.String, Concept.Int32)
          )(
            Data.Int32(0)
          ),
          testEval("folds right empty alternate")(
            "dictionaryTests",
            "dictFoldrTest2",
            Data.Map.empty(Concept.String, Concept.Int32)
          )(
            Data.List.empty(Concept.Int32)
          ),
          testEval("foldr mutation test")("dictionaryTests", "dictFoldlMutateTest", Map("a" -> 1, "b" -> 2))(
            Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2))
          )
        ),
        suite("map")(
          testEval("maps a function over a dictionary with positive values")(
            "dictionaryTests",
            "dictMapTest",
            Map("a" -> 1, "b" -> 2)
          )(
            Data.Map(
              Data.String("a") -> Data.Int(2),
              Data.String("b") -> Data.Int(4)
            )
          ),
          testEval("maps a function over a dictionary with negative values")(
            "dictionaryTests",
            "dictMapTest",
            Map("a" -> -1, "b" -> -2)
          )(
            Data.Map(
              Data.String("a") -> Data.Int(-2),
              Data.String("b") -> Data.Int(-4)
            )
          ),
          testEval("maps over empty dictionaries")(
            "dictionaryTests",
            "dictMapTest",
            Data.Map.empty(Concept.String, Concept.Int32)
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("map mutation test")(
            "dictionaryTests",
            "dictMapMutateTest",
            List(
              Map("a" -> 1, "b" -> 2)
            )
          )(
            Data.Map(Data.String("a") -> Data.Int(1), Data.String("b") -> Data.Int(2))
          )
        ),
        suite("merge")(
          testEvalMultiple("merges two dictionaries")(
            "dictionaryTests",
            "dictMergeTest",
            List(
              Map("Alice" -> 1, "Bob"  -> 1),
              Map("Bob"   -> 1, "Cedd" -> 1)
            )
          )(
            Data.Map(
              Data.String("Alice") -> Data.Int(2),
              Data.String("Bob")   -> Data.Int(3),
              Data.String("Cedd")  -> Data.Int(1)
            )
          ),
          testEvalMultiple("merges when the first dict is empty")(
            "dictionaryTests",
            "dictMergeTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Map("Bob" -> 1, "Cedd" -> 1)
            )
          )(
            Data.Map(
              Data.String("Bob")  -> Data.Int(1),
              Data.String("Cedd") -> Data.Int(1)
            )
          ),
          testEvalMultiple("merges when the second dict is empty")(
            "dictionaryTests",
            "dictMergeTest",
            List(
              Map("Alice" -> 1, "Bob" -> 1),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map(
              Data.String("Alice") -> Data.Int(2),
              Data.String("Bob")   -> Data.Int(2)
            )
          ),
          testEvalMultiple("merges when both dicts are empty")(
            "dictionaryTests",
            "dictMergeTest",
            List(
              Data.Map.empty(Concept.String, Concept.Int32),
              Data.Map.empty(Concept.String, Concept.Int32)
            )
          )(
            Data.Map.empty(Concept.String, Concept.Int32)
          ),
          testEvalMultiple("merges two dictionaries alternate")(
            "dictionaryTests",
            "dictMergeTest2",
            List(
              Map("Alice" -> "value1", "Bob"  -> "value2"),
              Map("Bob"   -> "value3", "Cedd" -> "value4")
            )
          )(
            Data.Map(
              Data.String("Alice") -> Data.String("value1"),
              Data.String("Bob")   -> Data.String("value2 and value3"),
              Data.String("Cedd")  -> Data.String("value4")
            )
          ),
          testEvalMultiple("merges two empty dictionaries alternate")(
            "dictionaryTests",
            "dictMergeTest2",
            List(
              Data.Map.empty(Concept.String, Concept.String),
              Data.Map.empty(Concept.String, Concept.String)
            )
          )(
            Data.Map.empty(Concept.String, Concept.String)
          ),
          testEvalMultiple("merges when the second dict is empty alternate")(
            "dictionaryTests",
            "dictMergeTest2",
            List(
              Map("Alice" -> "value1", "Bob" -> "value2"),
              Data.Map.empty(Concept.String, Concept.String)
            )
          )(
            Data.Map(
              Data.String("Alice") -> Data.String("value1"),
              Data.String("Bob")   -> Data.String("value2")
            )
          ),
          testEvalMultiple("merge mutation test")(
            "dictionaryTests",
            "dictMergeMutateTest",
            List(
              Map("Alice" -> 1, "Bob"  -> 1),
              Map("Bob"   -> 1, "Cedd" -> 1)
            )
          )(
            Data.List(
              Data.Map(Data.String("Alice") -> Data.Int(1), Data.String("Bob")  -> Data.Int(1)),
              Data.Map(Data.String("Bob")   -> Data.Int(1), Data.String("Cedd") -> Data.Int(1))
            )
          )
        )
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
        ),
        suite("andThen")(
          testEval("first output is Just, second is Nothing")("maybeTests", "maybeAndThen", 0)(
            Data.Optional.None(Concept.Float)
          ),
          testEval("both outputs are Just")("maybeTests", "maybeAndThen", 1)(Data.Optional.Some(Data.Float(1.0))),
          testEval("first output is Nothing")("maybeTests", "maybeAndThen", 2)(Data.Optional.None(Concept.Float))
        ),
        suite("hasValue Tests")(
          testEval("true for Just")("maybeTests", "maybeHasValueTest", Data.Optional.Some(Data.Int(1)))(
            Data.Boolean(true)
          ),
          testEval("false for Nothing")("maybeTests", "maybeHasValueTest", Data.Optional.None(Concept.Int32))(
            Data.Boolean(false)
          )
        ),
        suite("map2 Int Tests")(
          testEvalMultiple("both inputs are Just")(
            "maybeTests",
            "maybeMap2TestInt",
            List(Data.Optional.Some(Data.Int(1)), Data.Optional.Some(Data.Int(2)))
          )(
            Data.Optional.Some(Data.Int(3))
          )
        ),
        testEvalMultiple("Just and Nothing")(
          "maybeTests",
          "maybeMap2TestInt",
          List(Data.Optional.Some(Data.Int(1)), Data.Optional.None(Concept.Int32))
        )(
          Data.Optional.None(Concept.Int32)
        ),
        testEvalMultiple("Nothing and Just")(
          "maybeTests",
          "maybeMap2TestInt",
          List(Data.Optional.None(Concept.Int32), Data.Optional.Some(Data.Int(1)))
        )(
          Data.Optional.None(Concept.Int32)
        ),
        testEvalMultiple("Nothing and Nothing")(
          "maybeTests",
          "maybeMap2TestInt",
          List(Data.Optional.None(Concept.Int32), Data.Optional.None(Concept.Int32))
        )(
          Data.Optional.None(Concept.Int32)
        ),
        suite("map2 String Tests")(
          testEvalMultiple("both inputs are Just")(
            "maybeTests",
            "maybeMap2TestString",
            List(Data.Optional.Some(Data.String("Hello")), Data.Optional.Some(Data.String("World")))
          )(
            Data.String("HelloWorld")
          )
        ),
        testEvalMultiple("Just and Nothing")(
          "maybeTests",
          "maybeMap2TestString",
          List(Data.Optional.Some(Data.String("Hello")), Data.Optional.None(Concept.String))
        )(
          Data.String("Error")
        ),
        testEvalMultiple("Nothing and Just")(
          "maybeTests",
          "maybeMap2TestString",
          List(Data.Optional.None(Concept.String), Data.Optional.Some(Data.String("World")))
        )(
          Data.String("Error")
        ),
        testEvalMultiple("Nothing and Nothing")(
          "maybeTests",
          "maybeMap2TestString",
          List(Data.Optional.None(Concept.String), Data.Optional.None(Concept.String))
        )(
          Data.String("Error")
        ),
        suite("map3 String Tests")(
          testEvalMultiple("all inputs are Just")(
            "maybeTests",
            "maybeMap3TestString",
            List(
              Data.Optional.Some(Data.String("Hello")),
              Data.Optional.Some(Data.String("World")),
              Data.Optional.Some(Data.String("!!!"))
            )
          )(
            Data.String("HelloWorld!!!")
          )
        ),
        testEvalMultiple("Just, Just, Nothing")(
          "maybeTests",
          "maybeMap3TestString",
          List(
            Data.Optional.Some(Data.String("Hello")),
            Data.Optional.Some(Data.String("World")),
            Data.Optional.None(Concept.String)
          )
        )(
          Data.String("Error")
        ),
        testEvalMultiple("Nothing, Nothing, Nothing")(
          "maybeTests",
          "maybeMap3TestString",
          List(
            Data.Optional.None(Concept.String),
            Data.Optional.None(Concept.String),
            Data.Optional.None(Concept.String)
          )
        )(
          Data.String("Error")
        ),
        suite("map3 Int Tests")(
          testEvalMultiple("all inputs are Just")(
            "maybeTests",
            "maybeMap3TestInt",
            List(
              Data.Optional.Some(Data.Int(1)),
              Data.Optional.Some(Data.Int(2)),
              Data.Optional.Some(Data.Int(3))
            )
          )(
            Data.Optional.Some(Data.Int(6))
          )
        ),
        testEvalMultiple("Just, Just, Nothing")(
          "maybeTests",
          "maybeMap3TestInt",
          List(
            Data.Optional.Some(Data.Int(1)),
            Data.Optional.Some(Data.Int(2)),
            Data.Optional.None(Concept.Int32)
          )
        )(
          Data.Optional.None(Concept.Int32)
        ),
        testEvalMultiple("Nothing, Nothing, Nothing")(
          "maybeTests",
          "maybeMap3TestInt",
          List(
            Data.Optional.None(Concept.Int32),
            Data.Optional.None(Concept.Int32),
            Data.Optional.None(Concept.Int32)
          )
        )(
          Data.Optional.None(Concept.Int32)
        ),
        suite("map4 String Tests")(
          testEvalMultiple("all inputs are Just")(
            "maybeTests",
            "maybeMap4TestString",
            List(
              Data.Optional.Some(Data.String("Hello")),
              Data.Optional.Some(Data.String("World")),
              Data.Optional.Some(Data.String("!!!")),
              Data.Optional.Some(Data.String("!!!"))
            )
          )(
            Data.String("HelloWorld!!!!!!")
          )
        ),
        testEvalMultiple("Just, Just, Nothing, Just")(
          "maybeTests",
          "maybeMap4TestString",
          List(
            Data.Optional.Some(Data.String("Hello")),
            Data.Optional.Some(Data.String("World")),
            Data.Optional.None(Concept.String),
            Data.Optional.Some(Data.String("!!!"))
          )
        )(
          Data.String("Error")
        ),
        testEvalMultiple("Nothing, Nothing, Nothing, Nothing")(
          "maybeTests",
          "maybeMap4TestString",
          List(
            Data.Optional.None(Concept.String),
            Data.Optional.None(Concept.String),
            Data.Optional.None(Concept.String),
            Data.Optional.None(Concept.String)
          )
        )(
          Data.String("Error")
        ),
        suite("map4 Int Tests")(
          testEvalMultiple("all inputs are Just")(
            "maybeTests",
            "maybeMap4TestInt",
            List(
              Data.Optional.Some(Data.Int(1)),
              Data.Optional.Some(Data.Int(2)),
              Data.Optional.Some(Data.Int(3)),
              Data.Optional.Some(Data.Int(4))
            )
          )(
            Data.Optional.Some(Data.Int(10))
          )
        ),
        testEvalMultiple("Just, Just, Nothing, Just")(
          "maybeTests",
          "maybeMap4TestInt",
          List(
            Data.Optional.Some(Data.Int(1)),
            Data.Optional.Some(Data.Int(2)),
            Data.Optional.None(Concept.Int32),
            Data.Optional.Some(Data.Int(4))
          )
        )(
          Data.Optional.None(Concept.Int32)
        ),
        testEvalMultiple("Nothing, Nothing, Nothing, Nothing")(
          "maybeTests",
          "maybeMap4TestInt",
          List(
            Data.Optional.None(Concept.Int32),
            Data.Optional.None(Concept.Int32),
            Data.Optional.None(Concept.Int32),
            Data.Optional.None(Concept.Int32)
          )
        )(
          Data.Optional.None(Concept.Int32)
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
        )(Data.Result.Err.withOkConcept(Data.String("Undefined"), Concept.Int32)),
        suite("andThen")(
          testEval("both results are Ok")("resultTests", "resultAndThen", 1)(
            Data.Result.Ok(Data.Float(1.0), Concept.Result(Concept.String, Concept.Float))
          ),
          testEval("first result is Err")("resultTests", "resultAndThen", 2)(
            Data.Result.Err(Data.String("invalid"), Concept.Result(Concept.String, Concept.Float))
          ),
          testEval("first result is Ok, second is Err")("resultTests", "resultAndThen", 0)(
            Data.Result.Err(Data.String("undefined"), Concept.Result(Concept.String, Concept.Float))
          )
        )
      ),
      suite("SDK Comparable Tests")(
        testEvalMultiple("Compare Int")("sdkBasicsTests", "sdkCompareTest", List(1, 2))(Data.Order(-1)),
        testEvalMultiple("Compare Float")("sdkBasicsTests", "sdkCompareTest", List(2.0, 1.0))(Data.Order(1)),
        testEvalMultiple("Compare String")("sdkBasicsTests", "sdkCompareTest", List("Red", "Red"))(Data.Order(0)),
        testEvalMultiple("Compare Char")("sdkBasicsTests", "sdkCompareTest", List('r', 'b'))(Data.Order(1)),
        testEvalMultiple("Compare List")("sdkBasicsTests", "sdkCompareTest", List(List(1, 0), List(1, 2)))(
          Data.Order(-1)
        ),
        testEvalMultiple("Compare List Different Length")(
          "sdkBasicsTests",
          "sdkCompareTest",
          List(List(1, 0), List(1, 0, 0))
        )(Data.Order(-1)),
        testEvalMultiple("Compare List Different Length and Values")(
          "sdkBasicsTests",
          "sdkCompareTest",
          List(List(1, 1), List(1, 0, 0))
        )(Data.Order(1)),
        testEvalMultiple("Compare List of Tuples")(
          "sdkBasicsTests",
          "sdkCompareTest",
          List(
            List((1, "Blue"), (1, "Green")),
            List((1, "Blue"), (1, "An utter lack of any color, even black or white"))
          )
        )(Data.Order(1)),
        testEvalMultiple("Compare Tuple of Lists")(
          "sdkBasicsTests",
          "sdkCompareTest",
          List((List(1, 2), List("Red, Blue")), (List(1, 2), List("Red, Green")))
        )(Data.Order(-1)),
        testEvalMultiple("Compare Tuple")("sdkBasicsTests", "sdkCompareTest", List((0, 1, 2), (0, 2, 1)))(
          Data.Order(-1)
        ),
        testEval("Input LT")("sdkBasicsTests", "sdkOrderToStringTest", Data.Order(-1))(
          Data.String("LT")
        ),
        testEval("Input GT")("sdkBasicsTests", "sdkOrderToStringTest", Data.Order(1))(
          Data.String("GT")
        ),
        testEval("Input EQ")("sdkBasicsTests", "sdkOrderToStringTest", Data.Order(0))(
          Data.String("EQ")
        )
      ),
      suite("SDK Basics Tests")(
        suite("Append")(
          testEvaluation("String append")("sdkBasicsTests", "sdkAppendStringTest")(Data.String("aa-bb")),
          testEvaluation("List append")("sdkBasicsTests", "sdkAppendListTest")(Data.List(
            Data.Int(1),
            Data.Int(2),
            Data.Int(3),
            Data.Int(4)
          ))
        ),
        suite("Boolean")(
          suite("Equality")(
            testEvaluation("Equal 1")("sdkBasicsTests", "sdkEqualTest")(Data.Boolean(true)),
            testEvaluation("Equal 2")("sdkBasicsTests", "sdkEqualTest2")(Data.Boolean(true)),
            testEvaluation("Equal 3")("sdkBasicsTests", "sdkEqualTest3")(Data.Boolean(true)),
            testEvaluation("Equal 4")("sdkBasicsTests", "sdkEqualTest4")(Data.Boolean(true)),
            testEvaluation("Equal 4")("sdkBasicsTests", "sdkEqualTest5")(Data.Boolean(true)),
            testEvaluation("Equal 5")("sdkBasicsTests", "sdkEqualTest6")(Data.Boolean(true)),
            testEvaluation("Equal 6")("sdkBasicsTests", "sdkEqualTest7")(Data.Boolean(true))
          ),
          suite("Inequality")(
            testEvaluation("InEqual 1")("sdkBasicsTests", "sdkNotEqualTest")(Data.Boolean(true)),
            testEvaluation("InEqual 2")("sdkBasicsTests", "sdkNotEqualTest2")(Data.Boolean(true)),
            testEvaluation("InEqual 3")("sdkBasicsTests", "sdkNotEqualTest3")(Data.Boolean(true)),
            testEvaluation("InEqual 4")("sdkBasicsTests", "sdkNotEqualTest4")(Data.Boolean(true)),
            testEvaluation("InEqual 5")("sdkBasicsTests", "sdkNotEqualTest5")(Data.Boolean(true)),
            testEvaluation("InEqual 6")("sdkBasicsTests", "sdkNotEqualTest6")(Data.Boolean(true)),
            testEvaluation("InEqual 7")("sdkBasicsTests", "sdkNotEqualTest7")(Data.Boolean(true))
          ),
          suite("LessThan")(
            testEvalMultiple("Float")("sdkBasicsTests", "sdkLessThanTest", List(2.0, 4.0))(Data.Boolean(true)),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkLessThanTest", List('a', 'b'))(Data.Boolean(true)),
            testEvalMultiple("String")("sdkBasicsTests", "sdkLessThanTest", List("AA", "BB"))(
              Data.Boolean(true)
            ),
            testEvalMultiple("Tuple")("sdkBasicsTests", "sdkLessThanTest", List((1, 2), (2, 3)))(
              Data.Boolean(true)
            ),
            testEvalMultiple("List")("sdkBasicsTests", "sdkLessThanTest", List(List(1, 2), List(2, 3)))(
              Data.Boolean(true)
            ),
            testEvaluation("x < y - True")("sdkBasicsTests", "sdkLessThanTestIntTrue")(Data.Boolean(true)),
            testEvaluation("x < y - False")("sdkBasicsTests", "sdkLessThanTestIntFalse")(Data.Boolean(false))
          ),
          suite("LessThanOrEqual")(
            testEvalMultiple("Float")("sdkBasicsTests", "sdkLessThanOrEqualTest", List(2.0, 4.0))(
              Data.Boolean(true)
            ),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkLessThanOrEqualTest", List('a', 'b'))(
              Data.Boolean(true)
            ),
            testEvalMultiple("String")("sdkBasicsTests", "sdkLessThanOrEqualTest", List("AA", "BB"))(
              Data.Boolean(true)
            ),
            testEvalMultiple("Tuple")("sdkBasicsTests", "sdkLessThanOrEqualTest", List((1, 2), (2, 3)))(
              Data.Boolean(true)
            ),
            testEvalMultiple("List")(
              "sdkBasicsTests",
              "sdkLessThanOrEqualTest",
              List(List(1, 2), List(2, 3))
            )(Data.Boolean(true)),
            testEvaluation("x <= y - True A")("sdkBasicsTests", "sdkLessThanOrEqualTestIntTrue1")(Data.Boolean(true)),
            testEvaluation("x <= y - True B")("sdkBasicsTests", "sdkLessThanOrEqualTestIntTrue2")(Data.Boolean(true)),
            testEvaluation("x <= y - False")("sdkBasicsTests", "sdkLessThanOrEqualTestIntFalse")(Data.Boolean(false))
          ),
          suite("GreaterThan")(
            testEvalMultiple("Int")("sdkBasicsTests", "sdkGreaterThanTest", List(2, 4))(
              Data.Boolean(false)
            ),
            testEvalMultiple("Float")("sdkBasicsTests", "sdkGreaterThanTest", List(2.0, 4.0))(
              Data.Boolean(false)
            ),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkGreaterThanTest", List('a', 'b'))(
              Data.Boolean(false)
            ),
            testEvalMultiple("String")("sdkBasicsTests", "sdkGreaterThanTest", List("AA", "BB"))(
              Data.Boolean(false)
            ),
            testEvalMultiple("Tuple")("sdkBasicsTests", "sdkGreaterThanTest", List((1, 2), (2, 3)))(
              Data.Boolean(false)
            ),
            testEvalMultiple("List")("sdkBasicsTests", "sdkGreaterThanTest", List(List(1, 2), List(2, 3)))(
              Data.Boolean(false)
            ),
            testEvaluation("x > y - True")("sdkBasicsTests", "sdkGreaterThanTestIntTrue")(Data.Boolean(true)),
            testEvaluation("x > y - False")("sdkBasicsTests", "sdkGreaterThanTestIntFalse")(Data.Boolean(false))
          ),
          suite("GreaterThanOrEqual")(
            testEvalMultiple("Int")("sdkBasicsTests", "sdkGreaterThanOrEqualTest", List(2, 4))(
              Data.Boolean(false)
            ),
            testEvalMultiple("Float")("sdkBasicsTests", "sdkGreaterThanOrEqualTest", List(2.0, 4.0))(
              Data.Boolean(false)
            ),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkGreaterThanOrEqualTest", List('a', 'b'))(
              Data.Boolean(false)
            ),
            testEvalMultiple("String")(
              "sdkBasicsTests",
              "sdkGreaterThanOrEqualTest",
              List("AA", "BB")
            )(
              Data.Boolean(false)
            ),
            testEvalMultiple("Tuple")(
              "sdkBasicsTests",
              "sdkGreaterThanOrEqualTest",
              List((1, 2), (2, 3))
            )(
              Data.Boolean(false)
            ),
            testEvalMultiple("List")(
              "sdkBasicsTests",
              "sdkGreaterThanOrEqualTest",
              List(List(1, 2), List(2, 3))
            )(
              Data.Boolean(false)
            ),
            testEvaluation("x >= y - True A")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntTrue1")(
              Data.Boolean(true)
            ),
            testEvaluation("x >= y - True B")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntTrue2")(
              Data.Boolean(true)
            ),
            testEvaluation("x >= y - False")("sdkBasicsTests", "sdkGreaterThanOrEqualTestIntFalse")(Data.Boolean(false))
          ),
          suite("Operators")(
            testEvaluation("And")("sdkBasicsTests", "sdkAndTest")(Data.Boolean(false)),
            testEvaluation("Or")("sdkBasicsTests", "sdkOrTest")(Data.Boolean(true)),
            testEvalMultiple("Xor- true true")(
              "sdkBasicsTests",
              "basicsXorTest",
              List(Data.Boolean(true), Data.Boolean(true))
            )(
              Data.Boolean(false)
            ),
            testEvalMultiple("Xor- true false")(
              "sdkBasicsTests",
              "basicsXorTest",
              List(Data.Boolean(true), Data.Boolean(false))
            )(
              Data.Boolean(true)
            ),
            testEvalMultiple("Xor- false true")(
              "sdkBasicsTests",
              "basicsXorTest",
              List(Data.Boolean(false), Data.Boolean(true))
            )(
              Data.Boolean(true)
            ),
            testEvalMultiple("Xor- false false")(
              "sdkBasicsTests",
              "basicsXorTest",
              List(Data.Boolean(false), Data.Boolean(false))
            )(
              Data.Boolean(false)
            ),
            testEvaluation("Not")("sdkBasicsTests", "sdkNotTest")(Data.Boolean(false))
          )
        ),
        suite("Float")(
          testEvaluation("Plus")("sdkBasicsTests", "sdkAddFloatTest")(Data.Float(3.0)),
          testEvaluation("Plus overflow")("sdkBasicsTests", "sdkFloatOverflowTest")(Data.Float(3.0))
            @@ ignore @@ TestAspect.tag("Not Implemented yet"),
          testEvaluation("Minus")("sdkBasicsTests", "sdkSubtractFloatTest")(Data.Float(2.0)),
          testEvaluation("Multiply")("sdkBasicsTests", "sdkMultiplyFloatTest")(Data.Float(20.0)),
          testEvaluation("Divide")("sdkBasicsTests", "sdkDivideTest")(Data.Float(2.0)),
          testEvaluation("Divide by 0")("sdkBasicsTests", "sdkDivideByZeroTest")(Data.Float(Double.PositiveInfinity)),
          testEvalMultiple("Power")("sdkBasicsTests", "basicsPowerTest", List(4.0, 5.0))(Data.Float(1024)),
          testEvaluation("isNan")("sdkBasicsTests", "sdkIsNaNTest")(Data.Boolean(true)),
          testEvaluation("isInfinite")("sdkBasicsTests", "sdkIsInfiniteTest")(Data.Boolean(true)),
          testEvaluation("Eulers")("sdkBasicsTests", "sdkEulersNumberTest")(Data.Float(2.718281828459045)),
          testEvaluation("Pi")("sdkBasicsTests", "sdkPiTest")(Data.Float(3.141592653589793)),
          testEvaluation("cos")("sdkBasicsTests", "sdkCosTest")(Data.Float(0.5000000000000001)),
          testEvaluation("sin")("sdkBasicsTests", "sdkSinTest")(Data.Float(0.8660254037844386)),
          testEvaluation("tan")("sdkBasicsTests", "sdkTanTest")(Data.Float(0.9999999999999999)),
          testEvaluation("acos")("sdkBasicsTests", "sdkACosTest")(Data.Float(1.0471975511965979)),
          testEvaluation("asin")("sdkBasicsTests", "sdkASinTest")(Data.Float(0.5235987755982989)),
          testEvaluation("atan")("sdkBasicsTests", "sdkATanTest")(Data.Float(0.7853981633974483)),
          testEvaluation("atan2")("sdkBasicsTests", "sdkATan2Test")(Data.Float(0.7853981633974483)),
          testEvaluation("degrees")("sdkBasicsTests", "sdkDegreesTest")(Data.Float(3.141592653589793)),
          testEvaluation("radians")("sdkBasicsTests", "sdkRadiansTest")(Data.Float(3.141592653589793)),
          testEvaluation("turns")("sdkBasicsTests", "sdkTurnsTest")(Data.Float(3.141592653589793)),
          testEvaluation("toPolar")("sdkBasicsTests", "sdkToPolarTest")(Data.Tuple(
            Data.Float(5),
            Data.Float(0.9272952180016122)
          )),
          testEvaluation("fromPolar")("sdkBasicsTests", "sdkFromPolarTest")(Data.Tuple(
            Data.Float(1.2247448713915892),
            Data.Float(0.7071067811865475)
          )),
          testEval("Ceiling")("sdkBasicsTests", "basicsCeilingTest", 3.88)(Data.Int(4)),
          testEval("Ceiling whole number")("sdkBasicsTests", "basicsCeilingTest", 3.0)(Data.Int(3)),
          testEval("Floor")("sdkBasicsTests", "basicsFloorTest", 3.88)(Data.Int(3)),
          testEval("Floor whole number")("sdkBasicsTests", "basicsFloorTest", 3.0)(Data.Int(3)),
          testEval("Truncate")("sdkBasicsTests", "basicsTruncateTest", 1.2)(Data.Int(1)),
          testEval("Truncate 2")("sdkBasicsTests", "basicsTruncateTest", -1.2)(Data.Int(-1)),
          testEval("Truncate 3")("sdkBasicsTests", "basicsTruncateTest", .4)(Data.Int(0)),
          testEval("Truncate 4")("sdkBasicsTests", "basicsTruncateTest", -.4)(Data.Int(0)),
          testEval("Abs")("sdkBasicsTests", "basicsAbsTest", Data.Float(-5.0))(Data.Float(5.0)),
          testEval("Round up")("sdkBasicsTests", "basicsRoundTest", Data.Float(1.6))(Data.Int(2)),
          testEval("Round down")("sdkBasicsTests", "basicsRoundTest", Data.Float(1.4))(Data.Int(1))
        ),
        suite("Int")(
          testEvaluation("Plus")("sdkBasicsTests", "sdkAddTest")(Data.Int(3)),
          testEvaluation("Plus overflow")("sdkBasicsTests", "sdkIntOverflowTest")(Data.Int(3))
            @@ ignore @@ TestAspect.tag("Not Implemented yet"),
          testEvaluation("Minus")("sdkBasicsTests", "sdkSubtractTest")(Data.Int(2)),
          testEval("Plus(64)")("sdkBasicsTests", "sdkAddTest64", abStruct(1L, 2L))(
            Data.Int64(3)
          ) @@ ignore @@ TestAspect.tag("Not properly typed"),
          testEval("Minus(64)")("sdkBasicsTests", "sdkSubtractTest64", abStruct(4L, 2L))(
            Data.Int64(2)
          ) @@ ignore @@ TestAspect.tag("Not properly typed"),
          testEvaluation("Multiply")("sdkBasicsTests", "sdkMultiplyIntTest")(Data.Int(20)),
          testEvalMultiple("IntegerDivide")("sdkBasicsTests", "basicsIntegerDivideTest", List(12, 2))(
            Data.Int(6)
          ),
          testEvalMultiple("IntegerDivide 2")("sdkBasicsTests", "basicsIntegerDivideTest", List(12, 0))(
            Data.Int(0)
          ),
          testEvalMultiple("IntegerDivide 2")("sdkBasicsTests", "basicsIntegerDivideTest", List(-12, 7))(
            Data.Int(-1)
          ),
          testEvalMultiple("RemainderBy")("sdkBasicsTests", "basicsRemainderByTest", List(4, 21))(Data.Int(1)),
          testEvalMultiple("RemainderBy 2")("sdkBasicsTests", "basicsRemainderByTest", List(4, -21))(Data.Int(-1)),
          testEvalMultiple("RemainderBy 3")("sdkBasicsTests", "basicsRemainderByTest", List(0, 4))(
            Data.Int(0)
          ) @@ ignore @@ TestAspect.tag("remainderBy 0 throws"),
          testEvaluation("Negative int abs")("sdkBasicsTests", "sdkAbsTest")(Data.Int(3)),
          testEvaluation("Positive int abs")("sdkBasicsTests", "sdkAbsTest2")(Data.Int(3)),
          testEvaluation("Negate")("sdkBasicsTests", "sdkNegateTest")(Data.Int(-3)),
          testEvaluation("Negate")("sdkBasicsTests", "sdkNegateTest2")(Data.Int(3)),
          testEvaluation("Round")("sdkBasicsTests", "sdkRoundTest")(Data.Int(123)),
          testEvaluation("Round")("sdkBasicsTests", "sdkRoundTest2")(Data.Int(123)),
          testEvalMultiple("Power")("sdkBasicsTests", "basicsPowerTest", List(4, 5))(Data.Int(1024)),
          testEvaluation("ModBy")("sdkBasicsTests", "sdkModByTest")(Data.Int(2)),
          testEvaluation("Sqrt")("sdkBasicsTests", "sdkSqrtTest")(Data.Float(4.0)),
          testEvaluation("ToFloat")("sdkBasicsTests", "toFloatTest")(Data.Float(2.0))
        ),
        suite("Math")(
          suite("Clamp")(
            testEvalMultiple("Clamp greater than")("sdkBasicsTests", "basicsClampTest", List(100, 200, 1000))(
              Data.Int(200)
            ),
            testEvalMultiple("Clamp less than")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 50.0))(
              Data.Float(100.0)
            ),
            testEvalMultiple("Clamp as min")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 100.0))(
              Data.Float(100.0)
            ),
            testEvalMultiple("Clamp as max")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 200.0))(
              Data.Float(200.0)
            ),
            testEvalMultiple("Clamp in range")("sdkBasicsTests", "basicsClampTest", List(100.0, 200.0, 150.0))(
              Data.Float(150.0)
            )
          ),
          suite("Max")(
            testEvalMultiple("Int")("sdkBasicsTests", "sdkMaxTest", List(2, 4))(Data.Int(4)),
            testEvalMultiple("Float")("sdkBasicsTests", "sdkMaxTest", List(2.0, 4.0))(Data.Float(4.0)),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkMaxTest", List('a', 'b'))(Data.Char('b')),
            testEvalMultiple("String")("sdkBasicsTests", "sdkMaxTest", List("AA", "BB"))(Data.String("BB")),
            testEvalMultiple("Tuple")("sdkBasicsTests", "sdkMaxTest", List((1, 2), (2, 3)))(Data.Tuple(
              Data.Int(2),
              Data.Int(3)
            )),
            testEvalMultiple("List")("sdkBasicsTests", "sdkMaxTest", List(List(1, 2), List(2, 3)))(Data.List(
              Data.Int(2),
              Data.Int(3)
            ))
          ),
          suite("Min")(
            testEvalMultiple("Int")("sdkBasicsTests", "sdkMinTest", List(2, 4))(Data.Int(2)),
            testEvalMultiple("Float")("sdkBasicsTests", "sdkMinTest", List(2.0, 4.0))(Data.Float(2.0)),
            testEvalMultiple("Char")("sdkBasicsTests", "sdkMinTest", List('a', 'b'))(Data.Char('a')),
            testEvalMultiple("String")("sdkBasicsTests", "sdkMinTest", List("AA", "BB"))(Data.String("AA")),
            testEvalMultiple("Tuple")("sdkBasicsTests", "sdkMinTest", List((1, 2), (2, 3)))(Data.Tuple(
              Data.Int(1),
              Data.Int(2)
            )),
            testEvalMultiple("List")("sdkBasicsTests", "sdkMinTest", List(List(1, 2), List(2, 3)))(Data.List(
              Data.Int(1),
              Data.Int(2)
            ))
          ),
          testEval("Sqrt")("sdkBasicsTests", "basicsSqrtTest", Data.Float(9.0))(Data.Float(3.0)),
          testEvaluation("LogBase")("sdkBasicsTests", "sdkLogBaseTest")(Data.Float(2.0)),
          testEvaluation("LogBase2")("sdkBasicsTests", "sdkLogBaseTest2")(Data.Float(8.0))
        ),
        suite("break")(
          suite("Always")(
            testEval("Int")("sdkBasicsTests", "basicsAlwaysTest", 0)(Data.List(Data.Int(0))),
            testEval("Float")("sdkBasicsTests", "basicsAlwaysTest", 4.0)(Data.List(Data.Float(4.0))),
            testEval("Char")("sdkBasicsTests", "basicsAlwaysTest", Data.Char('z'))(Data.List(Data.Char('z'))),
            testEval("String")("sdkBasicsTests", "basicsAlwaysTest", Data.String("A"))(
              Data.List(Data.String("A"))
            )
          ),
          suite("Compose")(
            testEvaluation("ComposeLeft (<<)")("sdkBasicsTests", "sdkComposeLeftTest")(Data.Boolean(false)),
            testEvaluation("ComposeRight (>>)")("sdkBasicsTests", "sdkComposeRightTest")(Data.Int(603))
          ),
          suite("Identity")(
            testEval("Int")("sdkBasicsTests", "basicsIdentityTest", Data.Int(4))(Data.Int(4)),
            testEval("Float")("sdkBasicsTests", "basicsIdentityTest", Data.Float(-5.0))(Data.Float(-5.0)),
            testEval("Char")("sdkBasicsTests", "basicsIdentityTest", Data.Char('b'))(Data.Char('b')),
            testEval("String")("sdkBasicsTests", "basicsIdentityTest", Data.String("BB"))(Data.String("BB")),
            testEval("Tuple")(
              "sdkBasicsTests",
              "basicsIdentityTest",
              Data.Tuple(
                Data.Int(2),
                Data.Int(3)
              )
            )(Data.Tuple(
              Data.Int(2),
              Data.Int(3)
            )),
            testEval("List")("sdkBasicsTests", "basicsIdentityTest", Data.List(Data.Int(2), Data.Int(3)))(
              Data.List(Data.Int(2), Data.Int(3))
            )
          )
        )
      ),
      suite("Morphir Runtime Exception Tests")(
        testExceptionMultiple("FailedCoercion Test")(
          "exceptionTests",
          "sdkAddTest",
          List(Data.String("a"), Data.String("b"))
        ) {
          case TopLevelError(_, _, _) => assertTrue(true)
          case _                      => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("UnknownTypeMismatch Test")("exceptionTests", "decimalHundred", List(Data.String("a"))) {
          case TopLevelError(_, _, _: TypeError.UnknownTypeMismatch) => assertTrue(true)
          case _ => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("SizeMismatch Test")(
          "exceptionTests",
          "acceptTuple3",
          List(Data.Tuple(Data.Int(1), Data.Int(1)))
        ) {
          case TopLevelError(_, _, _: TypeError.SizeMismatch) => assertTrue(true)
          case _                                              => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("ValueLacksField Test")(
          "exceptionTests",
          "acceptXY",
          List(
            Data.Record(
              FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
              (Label("x"), Data.Int(1))
            )
          )
        ) {
          case TopLevelError(_, _, _: TypeError.ValueLacksField) => assertTrue(true)
          case _                                                 => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("ValueHasExtraField Test")(
          "exceptionTests",
          "acceptXY",
          List(
            Data.Record(
              FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
              (Label("x"), Data.Int(1)),
              (Label("y"), Data.Int(1)),
              (Label("z"), Data.Int(1))
            )
          )
        ) {
          case TopLevelError(_, _, _: TypeError.ValueHasExtraField) => assertTrue(true)
          case _ => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("ManyTypeErrors Test")(
          "exceptionTests",
          "acceptXY",
          List(
            Data.Record(
              FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
              (Label("a"), Data.Int(1)),
              (Label("b"), Data.Int(1))
            )
          )
        ) {
          case TopLevelError(_, _, _: TypeError.ManyTypeErrors) => assertTrue(true)
          case _                                                => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("LiteralTypeMismatch Test")(
          "exceptionTests",
          "acceptXYRecord",
          List(
            Data.Record(
              FQName.fromString("Morphir.Examples.App:ExceptionTests:XYRecord"),
              (
                Label("xy"),
                Data.Aliased.apply(
                  Data.String("test"),
                  Concept.Alias(FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"), Concept.String)
                )
              )
            )
          )
        ) {
          case TopLevelError(_, _, _: TypeError.LiteralTypeMismatch) => assertTrue(true)
          case _ => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("TypesMismatch Test")(
          "exceptionTests",
          "ignoreArgReturnString",
          List(
            Data.Record(
              FQName.fromString("Morphir.Examples.App:ExceptionTests:XYRecord"),
              (
                Label("xy"),
                Data.Aliased.apply(
                  Data.Record(
                    FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
                    (Label("x"), Data.Decimal(1.0)),
                    (Label("y"), Data.Decimal(1.0))
                  ),
                  Concept.Alias(
                    FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
                    Concept.Record(
                      FQName.fromString("Morphir.Examples.App:ExceptionTests:XY"),
                      List(
                        (Label("x"), Concept.Int32),
                        (Label("y"), Concept.Int32)
                      )
                    )
                  )
                )
              )
            )
          )
        ) {
          case TopLevelError(_, _, _: TypeError.TypesMismatch) => assertTrue(true)
          case _                                               => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("ResultTypeMismatch Test")(
          "exceptionTests",
          "stringAliasToString",
          List(
            Data.Aliased(
              Data.Unit,
              Concept.Alias(FQName.fromString("Morphir.Examples.App:ExceptionTests:StringAlias"), Concept.Unit)
            )
          )
        ) {
          case TopLevelError(_, _, _: ResultTypeMismatch) => assertTrue(true)
          case _                                          => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("UnmatchedPattern Test")(
          "exceptionTests",
          "nonExhaustiveCase",
          List(Data.Int(3))
        ) {
          case TopLevelError(_, _, _) => assertTrue(true)
          case _                      => assertNever(s"Unexpected exception type was thrown")
        },
        /* There are 2 different UnsupportedType errors possible.
           MorphirRuntimeError.UnsupportedType and MorphirErrorRuntime.TypeError.UnsupportedType
         */
        testExceptionMultiple("UnsupportedType Test")(
          "exceptionTests",
          "sdkAddTest",
          List(Data.String("a"))
        ) {
          case TopLevelError(_, _, _: BadReturnType) => assertTrue(true)
          case _                                     => assertNever(s"Unexpected exception type was thrown")
        },
        testExceptionMultiple("InferenceConflict Test")(
          "exceptionTests",
          "sdkAddTest",
          List(Data.Float(1), Data.Decimal(2))
        ) {
          case TopLevelError(_, _, _: TypeError.InferenceConflict) => assertTrue(true)
          case _ => assertNever(s"Unexpected exception type was thrown")
        },
        testExceptionMultiple("MissingDefinition Test")("exceptionTests", "notARealFunction", List(Data.Unit)) {
          case LookupError.MissingDefinition(_, _, _, _, _) => assertTrue(true)
          case _                                            => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("MissingModule Test")("notARealModule", "notARealFunction", List(Data.Unit)) {
          case LookupError.MissingModule(_, _, _, _) => assertTrue(true)
          case _                                     => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("MissingPackage Test")("", "", List(Data.Unit)) {
          case LookupError.MissingPackage(_, _, _) => assertTrue(true)
          case _                                   => assertNever("Unexpected exception type was thrown")
        },
        testExceptionMultiple("ImproperType Test")(
          "exceptionTests",
          "ignoreArgReturnString",
          List(Data.Int(1), Data.Int(1), Data.Int(1))
        ) {
          case TopLevelError(_, _, _: TypeError.ImproperType) => assertTrue(true)
          case _                                              => assertNever("Unexpected exception type was thrown")
        }
      )
    ).provideLayerShared(morphirRuntimeLayer)

  def abStruct(a: Long, b: Long) = Data.Struct(Label("a") -> Data.Int64(a), Label("b") -> Data.Int64(b))
}
