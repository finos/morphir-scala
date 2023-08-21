package org.finos.morphir.datamodel
import org.finos.morphir.naming._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
object EnumWrapperSpec extends MorphirBaseSpec {
  case class MyBool(value: Boolean)
  case class MyByte(value: Byte)
  case class MyDecimal(value: BigDecimal)
  case class MyInteger(value: scala.BigInt)
  case class MyInt16(value: scala.Short)
  case class MyInt32(value: scala.Int)
  case class MyString(value: java.lang.String)
  case class MyLocalDate(value: java.time.LocalDate)
  case class MyMonth(value: java.time.Month)
  case class MyLocalTime(value: java.time.LocalTime)
  case class MyChar(value: scala.Char)
  object MyUnit

  given rootName: GlobalDatamodelContext with {
    override def value = root / "test" % "enumwrapper"
  }

  def enumMaker(label: String, data: Data, concept: Concept) =
    SingleEnumWrapper(label, concept, rootName.value)

  def spec = suite("EnumWrapperSpec")(
    test("Bool Deriver") {
      given CustomDeriver[MyBool] = Data.Boolean.deriveEnumWrapper("MyBoolLabel", _.value)
      val myBoolData              = Data.Boolean(true)
      val maker                   = enumMaker("MyBoolLabel", myBoolData, Concept.Boolean)
      val myBool                  = MyBool(true)
      val myBoolDeriver           = Deriver.gen[MyBool]
      assertTrue(
        myBoolDeriver.concept == maker.concept,
        myBoolDeriver.derive(myBool) == maker.construct(myBoolData)
      )
    },
    test("Byte Deriver") {
      given CustomDeriver[MyByte] = Data.Byte.deriveEnumWrapper("MyByteLabel", _.value)
      val myByteData              = Data.Byte(1)
      val maker                   = enumMaker("MyByteLabel", myByteData, Concept.Byte)
      val myByte                  = MyByte(1.toByte)
      val myByteDeriver           = Deriver.gen[MyByte]
      assertTrue(
        myByteDeriver.concept == maker.concept,
        myByteDeriver.derive(myByte) == maker.construct(myByteData)
      )
    },
    test("Decimal Deriver") {
      given CustomDeriver[MyDecimal] = Data.Decimal.deriveEnumWrapper("MyDecimalLabel", _.value)
      val myDecimalData              = Data.Decimal(BigDecimal(123))
      val maker                      = enumMaker("MyDecimalLabel", myDecimalData, Concept.Decimal)
      val myDecimal                  = MyDecimal(BigDecimal(123))
      val myDecimalDeriver           = Deriver.gen[MyDecimal]
      assertTrue(
        myDecimalDeriver.concept == maker.concept,
        myDecimalDeriver.derive(myDecimal) == maker.construct(myDecimalData)
      )
    },
    test("Integer Deriver") {
      given CustomDeriver[MyInteger] = Data.Integer.deriveEnumWrapper("MyIntegerLabel", _.value)
      val myIntegerData              = Data.Integer(123)
      val maker                      = enumMaker("MyIntegerLabel", myIntegerData, Concept.Integer)
      val myInteger                  = MyInteger(123)
      val myIntegerDeriver           = Deriver.gen[MyInteger]
      assertTrue(
        myIntegerDeriver.concept == maker.concept,
        myIntegerDeriver.derive(myInteger) == maker.construct(myIntegerData)
      )
    },
    test("Int16 Deriver") {
      given CustomDeriver[MyInt16] = Data.Int16.deriveEnumWrapper("MyInt16Label", _.value)
      val myInt16Data              = Data.Int16(123)
      val maker                    = enumMaker("MyInt16Label", myInt16Data, Concept.Int16)
      val myInt16                  = MyInt16(123)
      val myInt16Deriver           = Deriver.gen[MyInt16]
      assertTrue(
        myInt16Deriver.concept == maker.concept,
        myInt16Deriver.derive(myInt16) == maker.construct(myInt16Data)
      )
    },
    test("Int32 Deriver") {
      given CustomDeriver[MyInt32] = Data.Int32.deriveEnumWrapper("MyInt32Label", _.value)
      val myInt32Data              = Data.Int32(123)
      val maker                    = enumMaker("MyInt32Label", myInt32Data, Concept.Int32)
      val myInt32                  = MyInt32(123)
      val myInt32Deriver           = Deriver.gen[MyInt32]
      assertTrue(
        myInt32Deriver.concept == maker.concept,
        myInt32Deriver.derive(myInt32) == maker.construct(myInt32Data)
      )
    },
    test("Int64 Deriver") {
      given CustomDeriver[MyInt64] = Data.Int64.deriveEnumWrapper("MyInt64Label", _.value)
      val myInt32Data              = Data.Int64(123)
      val maker                    = enumMaker("MyInt64Label", myInt64Data, Concept.Int64)
      val myInt32                  = MyInt64(123)
      val myInt32Deriver           = Deriver.gen[MyInt64]
      assertTrue(
        myInt64Deriver.concept == maker.concept,
        myInt64Deriver.derive(myInt64) == maker.construct(myInt64Data)
      )
    },
    test("String Deriver") {
      given CustomDeriver[MyString] = Data.String.deriveEnumWrapper("MyStringLabel", _.value)
      val myStringData              = Data.String("123")
      val maker                     = enumMaker("MyStringLabel", myStringData, Concept.String)
      val myString                  = MyString("123")
      val myStringDeriver           = Deriver.gen[MyString]
      assertTrue(
        myStringDeriver.concept == maker.concept,
        myStringDeriver.derive(myString) == maker.construct(myStringData)
      )
    },
    test("LocalDate Deriver") {
      given CustomDeriver[MyLocalDate] = Data.LocalDate.deriveEnumWrapper("MyLocalDateLabel", _.value)
      val myLocalDateData              = Data.LocalDate(java.time.LocalDate.now)
      val maker                        = enumMaker("MyLocalDateLabel", myLocalDateData, Concept.LocalDate)
      val myLocalDate                  = MyLocalDate(java.time.LocalDate.now)
      val myLocalDateDeriver           = Deriver.gen[MyLocalDate]
      assertTrue(
        myLocalDateDeriver.concept == maker.concept,
        myLocalDateDeriver.derive(myLocalDate) == maker.construct(myLocalDateData)
      )
    },
    test("Char Deriver") {
      given CustomDeriver[MyChar] = Data.Char.deriveEnumWrapper("MyCharLabel", _.value)
      val myCharData              = Data.Char('a')
      val maker                   = enumMaker("MyCharLabel", myCharData, Concept.Char)
      val myChar                  = MyChar('a')
      val myCharDeriver           = Deriver.gen[MyChar]
      assertTrue(myCharDeriver.concept == maker.concept, myCharDeriver.derive(myChar) == maker.construct(myCharData))
    },
    test("Unit Deriver") {
      given CustomDeriver[MyUnit.type] = Data.Unit.deriveEnumWrapper("MyUnitLabel")
      val maker                        = UnitEnumWrapper("MyUnitLabel", rootName.value)
      val myUnit                       = MyUnit
      val myUnitDeriver                = Deriver.gen[MyUnit.type]
      assertTrue(myUnitDeriver.concept == maker.concept, myUnitDeriver.derive(myUnit) == maker.construct)
    }
  )
}
