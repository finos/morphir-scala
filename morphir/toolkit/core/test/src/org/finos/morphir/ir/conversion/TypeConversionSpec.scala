package org.finos.morphir
package ir
package conversion

import org.finos.morphir.datamodel.{Concept, Label}
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.ir.Type.UType
import org.finos.morphir.testing.MorphirBaseSpec
import zio.Chunk
import zio.test._

object TypeConversionSpec extends MorphirBaseSpec {
  def spec = suite("TypeConversion Spec")(
    booleanSuite,
    listSuite,
    unitSuite,
    conceptSuite
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to convert a Scala Unit type to a Morphir Unit type") {
      val morphirType = ToMorphirType.summon[scala.Unit].withAttributesOf.morphirType
      assertTrue(morphirType == Type.unit)
    }
  )

  def booleanSuite = suite("Boolean")(
    test("Should be possible to convert a Scala Boolean type to a Morphir Boolean type") {
      val morphirType = ToMorphirType.summon[Boolean].withAttributesOf.morphirType
      assertTrue(morphirType == sdk.Basics.boolType)
    }
  )

  def listSuite = suite("List")(
    test("Should be possible to convert a Scala List[Boolean] type to a Morphir List[Boolean] type") {
      val morphirType = ToMorphirType.summon[scala.List[Boolean]].withAttributesOf.morphirType
      assertTrue(morphirType == sdk.List.listType(sdk.Basics.boolType))
    },
    test("Should be possible to convert a Scala List[Int] type to a Morphir List[Int] type") {
      val morphirType = ToMorphirType.summon[scala.List[Int]].withAttributesOf.morphirType
      assertTrue(morphirType == sdk.List.listType(sdk.Int.int32Type))
    }
  )

  def conceptSuite = suite("Concept")(
    test("Should be possible to convert a Concept Unit type to a Morphir Unit type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Unit).morphirType
      assertTrue(morphirType == Type.unit)
    },
    test("Should be possible to convert a Concept Boolean type to a Morphir Boolean type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Boolean).morphirType
      assertTrue(morphirType == sdk.Basics.boolType)
    },
    test("Should be possible to convert a Concept Byte type to a Morphir int8Type type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Byte).morphirType
      assertTrue(morphirType == sdk.Int.int8Type)
    },
    test("Should be possible to convert a Concept Decimal type to a Morphir Decimal type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Decimal).morphirType
      assertTrue(morphirType == sdk.Decimal.decimalType)
    },
    test("Should be possible to convert a Concept Integer type to a Morphir Integer type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Integer).morphirType
      assertTrue(morphirType == sdk.Basics.intType)
    },
    test("Should be possible to convert a Concept Int16 type to a Morphir Int16 type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Int16).morphirType
      assertTrue(morphirType == sdk.Int.int16Type)
    },
    test("Should be possible to convert a Concept Int32 type to a Morphir Integer type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Int32).morphirType
      assertTrue(morphirType == sdk.Int.int32Type)
    },
    test("Should be possible to convert a Concept String type to a Morphir String type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.String).morphirType
      assertTrue(morphirType == sdk.String.stringType)
    },
    test("Should be possible to convert a Concept LocalDate type to a Morphir LocalDate type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.LocalDate).morphirType
      assertTrue(morphirType == sdk.LocalDate.localDateType)
    },
    test("Should be possible to convert a Concept LocalTime type to a Morphir LocalTime type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.LocalTime).morphirType
      assertTrue(morphirType == sdk.LocalTime.localTimeType)
    },
    test("Should be possible to convert a Concept Month type to a Morphir Month type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Month).morphirType
      assertTrue(morphirType == sdk.Month.dateType)
    },
    test("Should be possible to convert a Concept Char type to a Morphir Char type") {
      val morphirType = ToMorphirType.summon[Concept].withAttributesOf(Concept.Char).morphirType
      assertTrue(morphirType == sdk.Char.charType)
    },
    suite("Concept.Option")(
      test("Should be possible to convert a Concept Option of String type to a Morphir Maybe type") {
        val concept     = Concept.Optional(Concept.String)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.Maybe.maybeType(sdk.String.stringType))
      },
      test("Should be possible to convert a nested Concept Option of String type to a Morphir Maybe type") {
        val concept     = Concept.Optional(Concept.Optional(Concept.String))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.Maybe.maybeType(sdk.Maybe.maybeType(sdk.String.stringType)))
      }
    ),
    suite("Concept.List")(
      test("Should be possible to convert a Concept List of Boolean type to a Morphir List[Boolean] type") {
        val concept     = Concept.List(Concept.Boolean)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.List.listType(sdk.Basics.boolType))
      },
      test("Should be possible to convert a Concept List of Integers type to a Morphir List[Int] type") {
        val concept     = Concept.List(Concept.Integer)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.List.listType(sdk.Basics.intType))
      },
      test("Should be possible to convert a Concept List of LocalDates type to a Morphir List[LocalDate] type") {
        val concept     = Concept.List(Concept.LocalDate)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.List.listType(sdk.LocalDate.localDateType))
      },
      test("Should be possible to convert a nested Concept List type to a Morphir List type") {
        val concept     = Concept.List(Concept.List(Concept.String))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.List.listType(sdk.List.listType(sdk.String.stringType)))
      }
    ),
    suite("Concept.Tuple")(
      test("Should be possible to convert a Concept Tuple type to a Morphir tuple type") {
        val conceptTuple = Concept.Tuple(List(Concept.Boolean, Concept.Unit, Concept.Byte))
        val morphirType  = ToMorphirType.summon[Concept].withAttributesOf(conceptTuple).morphirType
        assertTrue(morphirType == T.tuple(Chunk(sdk.Basics.boolType, Type.unit, sdk.Int.int8Type)))
      }
    ),
    suite("Concept.Record")(
      test("Should be possible to convert a Concept Record type to a Morphir record type") {
        val conceptRecord = Concept.Record(List(Label("1") -> Concept.String, Label("2") -> Concept.Char))
        val morphirType   = ToMorphirType.summon[Concept].withAttributesOf(conceptRecord).morphirType
        assertTrue(morphirType == T.record(Chunk("1" -> sdk.String.stringType, "2" -> sdk.Char.charType): _*))
      }
    ),
    suite("Concept.Map")(
      test("Should be possible to convert a simple Concept Map type to a Morphir Dict type") {
        val concept     = Concept.Map(Concept.String, Concept.LocalDate)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.Dict.dictType(sdk.String.stringType, sdk.LocalDate.localDateType))
      },
      test("Should be possible to convert a complex Concept Map type to a Morphir Dict type") {
        val concept     = Concept.Map(Concept.String, Concept.List(Concept.Integer))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.Dict.dictType(sdk.String.stringType, sdk.List.listType(sdk.Basics.intType)))
      },
      test("Should be possible to convert a nest Concept Map type to a Morphir Dict type") {
        val concept     = Concept.Map(Concept.String, Concept.Map(Concept.Integer, Concept.Decimal))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == sdk.Dict.dictType(
          sdk.String.stringType,
          sdk.Dict.dictType(sdk.Basics.intType, sdk.Decimal.decimalType)
        ))
      }
    ),
    suite("Concept.Alias")(
      test("Should be possible to convert a simple Concept Alias type to a Morphir type") {
        val concept     = Concept.Alias("SomeType", Concept.String)
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == T.reference("SomeType"))
      },
      test("Should be possible to convert a complex Concept Alias type to a Morphir type") {
        val concept     = Concept.Alias("SomeType", Concept.Map(Concept.String, Concept.List(Concept.Integer)))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == T.reference("SomeType"))
      },
      test("Should be possible to convert a nested Concept Alias type to a Morphir type") {
        val concept     = Concept.Alias("SomeType", Concept.Alias("otherAlias", Concept.String))
        val morphirType = ToMorphirType.summon[Concept].withAttributesOf(concept).morphirType
        assertTrue(morphirType == T.reference("SomeType"))
      }
    )
  )
}
