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
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.runtime.quick.EvaluatorQuick
import org.finos.morphir.runtime.ToMDMConcept.*

object DefaultsTestingSpec extends MorphirBaseSpec {
  val path =
    "examples/morphir-elm-projects/defaults-tests/morphir-ir.json"

  val testLayer: ZLayer[Any, Throwable, (Distribution, TypedMorphirRuntime)] = ZLayer(for {
    dist <- EvaluationLibrary.loadDistributionFromFileZIO(path)
  } yield (dist, MorphirRuntime.quick(dist)))

  def getRT =
    ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (_, rt) => ZIO.succeed(rt) }

  def getDist =
    ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, _) => ZIO.succeed(dist) }

  def getConcept(fqn: FQName) =
    ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, _) =>
      ZIO.fromEither(Type.reference(fqn, List()).concept(Distributions(dist), Map.empty))

    }

  def spec = suite("MDM Defaults Tests")(
    suite("Leaf Concepts")(
      test("Any") {
        val concept  = Concept.Any
        val actual   = concept.defaultData()
        val expected = Right(Data.Unit)
        assertTrue(actual == expected)
      },
      test("Boolean") {
        val concept  = Concept.Boolean
        val actual   = concept.defaultData()
        val expected = Right(Data.False)
        assertTrue(actual == expected)
      },
      test("Float") {
        val concept  = Concept.Float
        val actual   = concept.defaultData()
        val expected = Right(Data.Float(0.0))
        assertTrue(actual == expected)
      },
      test("Decimal") {
        val concept  = Concept.Decimal
        val actual   = concept.defaultData()
        val expected = Right(Data.Decimal(0))
        assertTrue(actual == expected)
      },
      test("Integer") {
        val concept  = Concept.Integer
        val actual   = concept.defaultData()
        val expected = Right(Data.Integer(0))
        assertTrue(actual == expected)
      },
      test("Int16") {
        val concept  = Concept.Int16
        val actual   = concept.defaultData()
        val expected = Right(Data.Int16(0))
        assertTrue(actual == expected)
      },
      test("Int32") {
        val concept  = Concept.Int32
        val actual   = concept.defaultData()
        val expected = Right(Data.Int32(0))
        assertTrue(actual == expected)
      },
      test("Int64") {
        val concept  = Concept.Int64
        val actual   = concept.defaultData()
        val expected = Right(Data.Int64(0))
        assertTrue(actual == expected)
      },
      test("String") {
        val concept  = Concept.String
        val actual   = concept.defaultData()
        val expected = Right(Data.String(""))
        assertTrue(actual == expected)
      },
      test("LocalDate") {
        val concept  = Concept.LocalDate
        val actual   = concept.defaultData()
        val expected = Right(Data.LocalDate(java.time.LocalDate.EPOCH))
        assertTrue(actual == expected)
      },
      test("Month") {
        val concept  = Concept.Month
        val actual   = concept.defaultData()
        val expected = Right(Data.Month(java.time.Month.JANUARY))
        assertTrue(actual == expected)
      },
      test("DayOfWeek") {
        val concept  = Concept.DayOfWeek
        val actual   = concept.defaultData()
        val expected = Right(Data.DayOfWeek(java.time.DayOfWeek.MONDAY))
        assertTrue(actual == expected)
      },
      test("LocalTime") {
        val concept  = Concept.LocalTime
        val actual   = concept.defaultData()
        val expected = Right(Data.LocalTime(java.time.LocalTime.MIDNIGHT))
        assertTrue(actual == expected)
      },
      test("Char") {
        val concept  = Concept.Char
        val actual   = concept.defaultData()
        val expected = Right(Data.Char('0'))
        assertTrue(actual == expected)
      },
      test("Order") {
        val concept  = Concept.Order
        val actual   = concept.defaultData()
        val expected = Right(Data.Order(0))
        assertTrue(actual == expected)
      },
      test("Unit") {
        val concept  = Concept.Unit
        val actual   = concept.defaultData()
        val expected = Right(Data.Unit)
        assertTrue(actual == expected)
      },
      test("Nothing") {
        val concept = Concept.Nothing
        val actual  = concept.defaultData()
        assertTrue(actual.isLeft)
      }
    ),
    suite("Nested Types")(
      test("List") {
        val inner    = Concept.Char
        val concept  = Concept.List(inner)
        val actual   = concept.defaultData()
        val expected = Right(Data.List.empty(inner))
        assertTrue(actual == expected)
      },
      test("Alias") {
        val inner    = Concept.Char
        val concept  = Concept.Alias(qn"Not:Real:Name", inner)
        val actual   = concept.defaultData()
        val expected = Right(Data.Aliased(Data.Char('0'), concept))
        assertTrue(actual == expected)
      },
      test("Map") {
        val key      = Concept.Char
        val value    = Concept.Int32
        val concept  = Concept.Map(key, value)
        val actual   = concept.defaultData()
        val expected = Right(Data.Map.empty(key, value))
        assertTrue(actual == expected)
      },
      test("Set") {
        val inner    = Concept.Char
        val concept  = Concept.Set(inner)
        val actual   = concept.defaultData()
        val expected = Right(Data.Set.empty(inner))
        assertTrue(actual == expected)
      },
      test("Tuple") {
        val first    = Concept.Char
        val second   = Concept.Unit
        val concept  = Concept.Tuple(List(first, second))
        val actual   = concept.defaultData()
        val expected = Right(Data.Tuple(List(Data.Char('0'), Data.Unit)))
        assertTrue(actual == expected)
      },
      test("Optional") {
        val inner    = Concept.Char
        val concept  = Concept.Optional(inner)
        val actual   = concept.defaultData()
        val expected = Right(Data.Optional.None(inner))
        assertTrue(actual == expected)
      },
      test("Result") {
        val err      = Concept.Char
        val ok       = Concept.Unit
        val concept  = Concept.Result(err, ok)
        val actual   = concept.defaultData()
        val expected = Right(Data.Result.Ok(Data.Unit, concept))
        assertTrue(actual == expected)
      }
    ),
    suite("From compiled code")(
      test("Record matches Default") {
        ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, rt) =>
          for {
            expected <- rt.evaluate(qn"Defaults:Defaults:expectedDefault")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            concept <- getConcept(qn"Defaults:Defaults:DefaultRecord")
            actual  <- ZIO.fromEither(concept.defaultData())
          } yield assertTrue(actual == expected)
        }
      },
      test("Record with nested complex field matches Default") {
        ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, rt) =>
          for {
            expected <- rt.evaluate(qn"Defaults:Defaults:largerExample")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            concept <- getConcept(qn"Defaults:Defaults:LargerRecord")
            actual  <- ZIO.fromEither(concept.defaultData())
          } yield assertTrue(actual == expected)
        }
      },
      test("Record with nested inner record matches Default") {
        ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, rt) =>
          for {
            expected <- rt.evaluate(qn"Defaults:Defaults:expectedNestedDefault")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            concept <- getConcept(qn"Defaults:Defaults:DefaultNestedRecord")
            actual  <- ZIO.fromEither(concept.defaultData())
          } yield assertTrue(actual == expected)
        }
      },
      test("Enum matches") {
        ZIO.serviceWithZIO[(Distribution, TypedMorphirRuntime)] { (dist, rt) =>
          for {
            expected <- rt.evaluate(qn"Defaults:Defaults:defaultUnion")
              .provideEnvironment(MorphirEnv.live)
              .toZIOWith(RTExecutionContext.typeChecked)
            concept <- getConcept(qn"Defaults:Defaults:NestedUnion")
            actual  <- ZIO.fromEither(concept.defaultData())
          } yield assertTrue(actual == expected)
        }
      }
    )
  ).provideLayerShared(testLayer)
}
