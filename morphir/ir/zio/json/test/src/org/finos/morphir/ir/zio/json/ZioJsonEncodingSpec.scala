package org.finos.morphir.ir.zio.json

import org.finos.morphir.ir.codec.JsonFormatVersion
import org.finos.morphir.testing.MorphirBaseSpec
import zio.json._
import zio.test._
object ZioJsonEncodingSpec extends MorphirBaseSpec:
  def spec = suite("ZioJsonEncodingSpec")(
    jsonFormat2Specs,
    jsonFormat1Specs
  )

  def jsonFormat2Specs =
    import Encoders.V2.given

    suite("JsonFormat2")(
      test("JsonFormatVersion can be encoded") {
        val actual = JsonFormatVersion.V2.toJson
        assertTrue(actual == "2")
      },
      test("Unit can be encoded") {
        val unit    = ()
        val encoded = unit.toJson
        assertTrue(encoded == "[]")
      }
    )

  def jsonFormat1Specs =
    import Encoders.V1.given

    suite("JsonFormat1")(
      test("JsonFormatVersion can be encoded") {
        val actual = JsonFormatVersion.V1.toJson
        assertTrue(actual == "1")
      },
      test("Unit can be encoded") {
        val unit    = ()
        val encoded = unit.toJson
        assertTrue(encoded == "[]")
      }
    )
