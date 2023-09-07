package org.finos.morphir.testing
import zio.test._
import zio.json._
import zio.json.ast.Json

trait ZioJsonSpec { self: ZIOSpecDefault =>

  def jsonDecodingTest[A: JsonDecoder](label: String)(json: String)(expected: A) = test(label) {
    val decoded = json.fromJson[A]
    assertTrue(decoded == Right(expected))
  }

  def jsonEncodingTest[A: JsonEncoder](label: String)(value: A)(expected: String) = test(label) {
    val encoded = value.toJson
    assertTrue(encoded == expected)
  }

  def jsonEncodingTest[A: JsonEncoder](label: String)(value: A)(expected: Json) = test(label) {
    val encoded = value.toJsonAST
    assertTrue(encoded == Right(expected))
  }

  def jsonRoundtripTest[A: JsonEncoder: JsonDecoder](label: String)(value: A) = test(label) {
    val encoded = value.toJson
    val decoded = encoded.fromJson[A]
    assertTrue(decoded == Right(value))
  }
}
