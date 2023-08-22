package org.finos.morphir.datamodel

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.datamodel.*
import zio.test.*

object ToDataPrimitivesSpec extends MorphirBaseSpec {
  def spec = suite("ToDataPrimitives Spec")(
    test("Boolean-true") { assertTrue(Deriver.toData(true) == Data.Boolean(true)) },
    test("Boolean-false") { assertTrue(Deriver.toData(false) == Data.Boolean(false)) },
    test("Byte")(
      assertTrue(Deriver.toData(0xf.toByte) == Data.Byte(0xf.toByte))
    ),
    test("Decimal") {
      assertTrue(Deriver.toData(BigDecimal("123.456")) == Data.Decimal(BigDecimal("123.456")))
    },
    test("Integer") {
      assertTrue(Deriver.toData(BigInt("1234")) == Data.Integer(BigInt("1234")))
    },
    test("Int16") {
      assertTrue(Deriver.toData(123.toShort) == Data.Int16(123.toShort))
    },
    test("Int32") {
      assertTrue(Deriver.toData(123) == Data.Int32(123))
    },
    test("String") {
      assertTrue(Deriver.toData("something") == Data.String("something"))
    },
    test("LocalDate") {
      val dt = java.time.LocalDate.of(2023, 1, 2)
      assertTrue(Deriver.toData(dt) == Data.LocalDate(dt))
    },
    test("Month") {
      val mon = java.time.Month.MAY
      assertTrue(Deriver.toData(mon) == Data.Month(mon))
    },
    test("LocalTime") {
      val tm = java.time.LocalTime.of(1, 2, 3)
      assertTrue(Deriver.toData(tm) == Data.LocalTime(tm))
    },
    test("Char") { assertTrue(Deriver.toData('a') == Data.Char('a')) },
    test("Unit") { assertTrue(Deriver.toData(()) == Data.Unit) }
  )
}
