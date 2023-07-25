package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.Deriver
import org.finos.morphir.datamodel.{*, given}

class ToDataPrimitives extends munit.FunSuite {
  test("Boolean-true") { assertEquals(Deriver.toData(true), Data.Boolean(true)) }
  test("Boolean-false") { assertEquals(Deriver.toData(false), Data.Boolean(false)) }

  test("Byte")(
    assertEquals(Deriver.toData(0xf.toByte), Data.Byte(0xf.toByte))
  )
  test("Decimal") {
    assertEquals(Deriver.toData(BigDecimal("123.456")), Data.Decimal(BigDecimal("123.456")))
  }
  test("Integer") {
    assertEquals(Deriver.toData(BigInt("1234")), Data.Integer(BigInt("1234")))
  }
  test("Int16") {
    assertEquals(Deriver.toData(123.toShort), Data.Int16(123.toShort))
  }
  test("Int32") {
    assertEquals(Deriver.toData(123), Data.Int32(123))
  }
  test("String") {
    assertEquals(Deriver.toData("something"), Data.String("something"))
  }
  test("LocalDate") {
    val dt = java.time.LocalDate.of(2023, 1, 2)
    assertEquals(Deriver.toData(dt), Data.LocalDate(dt))
  }
  test("Month") {
    val mon = java.time.Month.MAY
    assertEquals(Deriver.toData(mon), Data.Month(mon))
  }
  test("LocalTime") {
    val tm = java.time.LocalTime.of(1, 2, 3)
    assertEquals(Deriver.toData(tm), Data.LocalTime(tm))
  }
  test("Char") { assertEquals(Deriver.toData('a'), Data.Char('a')) }
  test("Unit") { assertEquals(Deriver.toData(()), Data.Unit) }
}
