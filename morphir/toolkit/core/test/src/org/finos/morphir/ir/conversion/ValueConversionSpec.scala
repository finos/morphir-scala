package org.finos.morphir
package ir
package conversion

import org.finos.morphir.datamodel.{Concept, Data, Label}
import org.finos.morphir.ir.FQName.fqn
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.{Type => T, Value => V}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.Chunk
import zio.test._

import java.time.temporal.ChronoField

object ValueConversionSpec extends MorphirBaseSpec {
  object pn {
    val morphirIR: PartialName = root / "Morphir" % "IR"
  }

  def spec = suite("ValueConversion Spec")(
    booleanSuite,
    intSuite,
    listSuite,
    unitSuite,
    dataSuite
  )

  def unitSuite = suite("Unit")(
    test("Should be possible to convert a Scala Unit to a Morphir Unit") {
      val toValue     = ToMorphirValue.summon[scala.Unit].typed
      val inputValue  = ()
      val morphirUnit = toValue(inputValue)
      assertTrue(morphirUnit == V.unit(Type.unit))
    }
  )

  def booleanSuite = suite("Boolean")(
    test("Should be possible to convert a Scala Boolean to a Morphir Boolean") {
      val toValue           = ToMorphirValue.summon[Boolean].typed
      val trueValue         = true
      val falseValue        = false
      val morphirTrueValue  = toValue(trueValue)
      val morphirFalseValue = toValue(falseValue)
      assertTrue(morphirTrueValue == Lit.True, morphirFalseValue == Lit.False)
    }
  )

  def intSuite = suite("Int")(
    test("Should be possible to convert a Scala Int to a Morphir Int") {
      val toValue    = ToMorphirValue.summon[Int].typed
      val inputValue = 10
      val morphirInt = toValue(inputValue)
      assertTrue(morphirInt == V.intTyped(inputValue))
    }
  )

  def listSuite = suite("List")(
    test("Should be possible to convert a Scala List[Boolean] to a Morphir List[Boolean]") {
      val sut        = ToMorphirValue.summon[scala.List[Boolean]].typed
      val inputValue = List(true, false, true)
      val actual     = sut.toMorphirValue(inputValue)
      assertTrue(
        actual == V.list(sdk.List.listType(sdk.Basics.boolType), zio.Chunk(Lit.True, Lit.False, Lit.True))
      )
    },
    test("Should be possible to convert a Scala List[Int] to a Morphir List[Int]") {
      val sut        = ToMorphirValue.summon[scala.List[Int]].typed
      val inputValue = List(10, 20, 30)
      val actual     = sut.toMorphirValue(inputValue)
      assertTrue(
        actual == V.list(
          sdk.List.listType(sdk.Int.int32Type),
          zio.Chunk(V.intTyped(10), V.intTyped(20), V.intTyped(30))
        )
      )
    }
  )

  def dataSuite = suite("Data")(
    test("Should be possible to convert a Data Unit to a Morphir Unit") {
      val toValue     = ToMorphirValue.summon[Data].typed
      val inputValue  = Data.Unit
      val morphirUnit = toValue(inputValue)
      assertTrue(morphirUnit == V.unit(Type.unit))
    },
    test("Should be possible to convert a Data Boolean to a Morphir Boolean") {
      val toValue           = ToMorphirValue.summon[Data].typed
      val trueValue         = Data.Boolean(true)
      val falseValue        = Data.Boolean(false)
      val morphirTrueValue  = toValue(trueValue)
      val morphirFalseValue = toValue(falseValue)
      assertTrue(morphirTrueValue == Lit.True, morphirFalseValue == Lit.False)
    },
    test("Should be possible to convert a Data Byte to a Morphir Int8") {
      val toValue     = ToMorphirValue.summon[Data].typed
      val inputValue  = Data.Byte(java.lang.Byte.MAX_VALUE)
      val morphirInt8 = toValue(inputValue)
      assertTrue(morphirInt8 == V.apply(
        sdk.Int.int8Type,
        V.reference(sdk.Int.int8Type, FQName.fromString("Morphir.SDK:Int:toInt8")),
        V.intTyped(inputValue.value)
      ))
    },
    test("Should be possible to convert a Data Char to a Morphir Char") {
      val toValue     = ToMorphirValue.summon[Data].typed
      val inputValue  = Data.Char('a')
      val morphirChar = toValue(inputValue)
      assertTrue(morphirChar == V.literal(sdk.Char.charType, Lit.char(inputValue.value)))
    },
    test("Should be possible to convert a Data Decimal to a Morphir Decimal") {
      val toValue     = ToMorphirValue.summon[Data].typed
      val inputValue  = Data.Decimal(BigDecimal("2121212323232323232323.343432423"))
      val morphirChar = toValue(inputValue)
      assertTrue(morphirChar == V.decimal(sdk.Decimal.decimalType, inputValue.value))
    },
    test("Should be possible to convert a Data Int16 to a Morphir Int16") {
      val toValue      = ToMorphirValue.summon[Data].typed
      val inputValue   = Data.Int16(java.lang.Short.MAX_VALUE)
      val morphirInt16 = toValue(inputValue)
      assertTrue(morphirInt16 == V.apply(
        sdk.Int.int16Type,
        V.reference(sdk.Int.int16Type, FQName.fromString("Morphir.SDK:Int:toInt16")),
        V.intTyped(inputValue.value)
      ))
    },
    test("Should be possible to convert a Data Int32 to a Morphir Int") {
      val toValue      = ToMorphirValue.summon[Data].typed
      val inputValue   = Data.Int32(10)
      val morphirInt32 = toValue(inputValue)
      assertTrue(morphirInt32 == V.intTyped(inputValue.value))
    },
    test("Should be possible to convert a Data LocalDate to a Morphir LocalDate") {
      val toValue          = ToMorphirValue.summon[Data].typed
      val inputValue       = Data.LocalDate(java.time.LocalDate.of(2013, 9, 3))
      val morphirLocalDate = toValue(inputValue)
      assertTrue(morphirLocalDate == V.apply(
        sdk.LocalDate.localDateType,
        V.reference(sdk.LocalDate.localDateType, FQName.fromString("Morphir.SDK:LocalDate:fromParts")),
        V.intTyped(2013),
        V.intTyped(9),
        V.intTyped(3)
      ))
    },
    test("Should be possible to convert a Data LocalTime to a Morphir LocalTime") {
      val toValue          = ToMorphirValue.summon[Data].typed
      val inputValue       = Data.LocalTime(java.time.LocalTime.now)
      val morphirLocalTime = toValue(inputValue)
      assertTrue(morphirLocalTime == V.apply(
        sdk.LocalTime.localTimeType,
        V.reference(sdk.LocalTime.localTimeType, FQName.fromString("Morphir.SDK:LocalTime:fromMilliseconds")),
        V.intTyped(inputValue.value.get(ChronoField.MILLI_OF_DAY))
      ))
    },
    test("Should be possible to convert a Data String to a Morphir String") {
      val toValue       = ToMorphirValue.summon[Data].typed
      val inputValue    = Data.String("Some string")
      val morphirString = toValue(inputValue)
      assertTrue(morphirString == V.string(sdk.String.stringType, inputValue.value))
    },
    suite("Data.Optional")(
      test("Should be possible to convert a Data Optional Some to a Morphir Maybe Just") {
        val toValue               = ToMorphirValue.summon[Data].typed
        val inputValue            = Data.Optional.Some(Data.String("Optional string"))
        val morphirOptionalString = toValue(inputValue)
        val result = V.apply(
          sdk.Maybe.maybeType(sdk.String.stringType),
          V.constructor(
            FQName.fromString("Morphir.SDK:Maybe:just"),
            sdk.Maybe.maybeType(sdk.String.stringType)
          ), // TODO check
          V.string(sdk.String.stringType, "Optional string")
        )
        assertTrue(morphirOptionalString == result)
      },
      test("Should be possible to convert a Data Optional None to a Morphir Maybe Nothing") {
        val toValue               = ToMorphirValue.summon[Data].typed
        val inputValue            = Data.Optional.None(Concept.String)
        val morphirOptionalString = toValue(inputValue)
        val result =
          V.constructor(FQName.fromString("Morphir.SDK:Maybe:Nothing"), sdk.Maybe.maybeType(sdk.String.stringType))
        assertTrue(morphirOptionalString == result)
      }
    ),
    suite("Data.Aliased")(
      test("Should be possible to convert a Data.Int32 Aliased to it's Morphir value") {
        val toValue           = ToMorphirValue.summon[Data].typed
        val someAliasTypeName = pn.morphirIR % "someAlias"
        val expectedFQName    = fqn("Morphir", "IR", "someAlias")
        val inputValue        = Data.Aliased(Data.Int32(10), Concept.Alias(someAliasTypeName, Concept.Int32))
        val morphirInt32      = toValue(inputValue)
        val result            = Value.literal(T.reference(expectedFQName), Lit.int(10))
        assertTrue(morphirInt32 == result)
      },
      test("Should be possible to convert a Data.List Aliased to it's Morphir value") {
        val toValue = ToMorphirValue.summon[Data].typed
        val inputValue =
          Data.Aliased(
            Data.List(Data.True, Data.False, Data.True),
            Concept.Alias(pn.morphirIR % "Alias1", Concept.List(Concept.Boolean))
          )
        val actual = toValue(inputValue)
        val result = V.list(T.reference("Morphir:IR:Alias1"), zio.Chunk(Lit.True, Lit.False, Lit.True))
        assertTrue(actual == result)
      }
    ),
    suite("Data.Tuple")(
      test("Should be possible to convert a Data Tuple type to a Morphir tuple type") {
        val toValue      = ToMorphirValue.summon[Data].typed
        val inputValue   = Data.Tuple(List(Data.True, Data.Unit, Data.Byte(java.lang.Byte.MAX_VALUE)))
        val morphirTuple = toValue(inputValue)
        val result = V.tuple(
          T.tuple(Chunk(sdk.Basics.boolType, Type.unit, sdk.Int.int8Type)),
          Chunk(
            Lit.True,
            V.unit(Type.unit),
            V.apply(
              sdk.Int.int8Type,
              V.reference(sdk.Int.int8Type, FQName.fromString("Morphir.SDK:Int:toInt8")),
              V.intTyped(java.lang.Byte.MAX_VALUE)
            )
          )
        )
        assertTrue(morphirTuple == result)
      }
    ),
    suite("Data.List")(
      test("Should be possible to convert a empty Data List of Strings to a Morphir List type") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.List.empty(Concept.String)
        val actual     = toValue(inputValue)
        assertTrue(actual == V.list(sdk.List.listType(sdk.String.stringType), zio.Chunk.empty))
      },
      test("Should be possible to convert a Data List Boolean type to a Morphir List type") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.List(Data.True, Data.False, Data.True)
        val actual     = toValue(inputValue)
        assertTrue(actual == V.list(
          sdk.List.listType(sdk.Basics.boolType),
          zio.Chunk(Lit.True, Lit.False, Lit.True)
        ))
      },
      test("Should be possible to convert a Data List Int32 type to a Morphir List type") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.List(Data.Int32(10), Data.Int32(20), Data.Int32(30))
        val actual     = toValue(inputValue)
        assertTrue(actual == V.list(
          sdk.List.listType(sdk.Int.int32Type),
          zio.Chunk(V.intTyped(10), V.intTyped(20), V.intTyped(30))
        ))
      },
      test("Should be possible to convert a nested Data List Int32 type to a Morphir List type") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.List(Data.List(Data.Int32(10), Data.Int32(20), Data.Int32(30)))
        val actual     = toValue(inputValue)
        assertTrue(actual == V.list(
          sdk.List.listType(sdk.List.listType(sdk.Int.int32Type)),
          V.list(
            sdk.List.listType(sdk.Int.int32Type),
            zio.Chunk(V.intTyped(10), V.intTyped(20), V.intTyped(30))
          )
        ))
      }
    ),
    suite("Data.Record")(
      test("Should be possible to convert a Data record to a Morphir record") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val recordName = pn.morphirIR % "Test"
        val inputValue =
          Data.Record(recordName, List(Label("1") -> Data.String("Testing Record"), Label("2") -> Data.False))
        val actual = toValue(inputValue)
        val result = V.record(
          T.record(Chunk("1" -> sdk.String.stringType, "2" -> sdk.Basics.boolType): _*),
          "1" -> V.string(sdk.String.stringType, "Testing Record"),
          "2" -> Lit.False
        )
        assertTrue(actual == result)
      }
    ),
    suite("Data.Map")(
      test("Should be possible to convert a simple Data.Map to a Morphir Dict") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.Map(Data.String("Index 1") -> Data.Int32(3), Data.String("Index 2") -> Data.Int32(71))
        val actual     = toValue(inputValue)
        val shape      = sdk.Dict.dictType(sdk.String.stringType, sdk.Int.int32Type)
        val result = V.apply(
          shape,
          V.reference(shape, FQName.fromString("Morphir.SDK:Dict:fromList")),
          V.list(
            sdk.List.listType(T.tuple(Chunk(sdk.String.stringType, sdk.Int.int32Type))),
            zio.Chunk(
              V.tuple(
                T.tuple(Chunk(sdk.String.stringType, sdk.Int.int32Type)),
                Chunk(V.string(sdk.String.stringType, "Index 1"), V.intTyped(3))
              ),
              V.tuple(
                T.tuple(Chunk(sdk.String.stringType, sdk.Int.int32Type)),
                Chunk(V.string(sdk.String.stringType, "Index 2"), V.intTyped(71))
              )
            )
          )
        )
        assertTrue(actual == result)
      },
      test("Should be possible to convert a complex Data Map to a Morphir Dict") {
        val toValue = ToMorphirValue.summon[Data].typed
        val inputValue = Data.Map(
          Data.String("Index 1") -> Data.List(Data.Int32(9), Data.Int32(3)),
          Data.String("Index 2") -> Data.List(Data.Int32(2), Data.Int32(71))
        )
        val actual = toValue(inputValue)
        val shape  = sdk.Dict.dictType(sdk.String.stringType, sdk.List.listType(sdk.Int.int32Type))
        val result = V.apply(
          shape,
          V.reference(shape, FQName.fromString("Morphir.SDK:Dict:fromList")),
          V.list(
            sdk.List.listType(T.tuple(Chunk(sdk.String.stringType, sdk.List.listType(sdk.Int.int32Type)))),
            zio.Chunk(
              V.tuple(
                T.tuple(Chunk(sdk.String.stringType, sdk.List.listType(sdk.Int.int32Type))),
                Chunk(
                  V.string(sdk.String.stringType, "Index 1"),
                  V.list(
                    sdk.List.listType(sdk.Int.int32Type),
                    zio.Chunk(V.intTyped(9), V.intTyped(3))
                  )
                )
              ),
              V.tuple(
                T.tuple(Chunk(sdk.String.stringType, sdk.List.listType(sdk.Int.int32Type))),
                Chunk(
                  V.string(sdk.String.stringType, "Index 2"),
                  V.list(
                    sdk.List.listType(sdk.Int.int32Type),
                    zio.Chunk(V.intTyped(2), V.intTyped(71))
                  )
                )
              )
            )
          )
        )
        assertTrue(actual == result)
      },
      test("Should be possible to convert an empty Data Map to a Morphir Dict") {
        val toValue    = ToMorphirValue.summon[Data].typed
        val inputValue = Data.Map.empty(Concept.String, Concept.Int32)
        val actual     = toValue(inputValue)
        val shape      = sdk.Dict.dictType(sdk.String.stringType, sdk.Int.int32Type)
        val result = V.apply(
          shape,
          V.reference(shape, FQName.fromString("Morphir.SDK:Dict:fromList")),
          V.list(
            sdk.List.listType(T.tuple(Chunk(sdk.String.stringType, sdk.Int.int32Type))),
            zio.Chunk.empty
          )
        )
        assertTrue(actual == result)
      }
    )
  )
}
