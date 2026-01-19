package org.finos.morphir.ir.json

import zio.test._
import zio.test.Assertion._
import zio.json._
import org.finos.morphir.ir.v4._
import org.finos.morphir.naming._

object MorphirJsonSupportV4Spec extends ZIOSpecDefault with MorphirJsonEncodingSupportV4
    with MorphirJsonDecodingSupportV4 {
  def spec = suite("MorphirJsonSupportV4Spec")(
    suite("Type Encoding/Decoding")(
      test("Unit Type round-trip") {
        val sut     = Type.Unit(TypeAttributes.empty)
        val json    = sut.toJson
        val decoded = json.fromJson[Type]
        assertTrue(decoded == Right(sut))
      },
      test("Variable Type round-trip") {
        val sut     = Type.Variable(TypeAttributes.empty, Name.fromString("foo"))
        val json    = sut.toJson
        val decoded = json.fromJson[Type]
        assertTrue(decoded == Right(sut))
      }
    ),
    suite("Value Encoding/Decoding")(
      test("Unit Value round-trip") {
        val sut     = Value.Unit(ValueAttributes.empty)
        val json    = sut.toJson
        val decoded = json.fromJson[Value]
        assertTrue(decoded == Right(sut))
      },
      test("Literal Value round-trip") {
        val sut     = Value.Literal(ValueAttributes.empty, Literal.BoolLiteral(true))
        val json    = sut.toJson
        val decoded = json.fromJson[Value]
        assertTrue(decoded == Right(sut))
      }
    )
  )
}
