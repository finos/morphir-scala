package org.finos.morphir.ir.zio.json

import org.finos.morphir.ir.Name
import org.finos.morphir.ir.Name.Name
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

    suite("When JsonFormatVersion == 2")(
      test("JsonFormatVersion can be encoded") {
        val actual = JsonFormatVersion.V2.toJson
        assertTrue(actual == "2")
      },
      suite("Unit")(
        test("Unit can be encoded") {
          val unit    = ()
          val encoded = unit.toJson
          assertTrue(encoded == "[]")
        }
      ),
      suite("Name")(
        // test("will encode an empty Name") {
        //   val actual   = Name.empty
        //   val expected = "[]"
        //   assertTrue(actual.toJson == expected)
        // }
        // test("will encode a single Name") {
        //   val actual   = Name("Hello")
        //   val expected = """["hello"]"""
        //   assertTrue(actual.toJson == expected)
        // },
        // test("will encode a Name") {
        //   val actual   = Name("HelloThere")
        //   val expected = """["hello","there"]"""
        //   assertTrue(actual.toJson == expected)
        // },
        // test("will encode a Name fromString") {
        //   val actual   = Name.fromString("Hello.There")
        //   val expected = """["hello","there"]"""
        //   assertTrue(actual.toJson == expected)
        // },
        // test("will encode a Name fromList") {
        //   val actual   = Name.fromList(List("This", "is", "a", "list"))
        //   val expected = """["this","is","a","list"]"""
        //   assertTrue(actual.toJson == expected)
        // }
      )
    )

  def jsonFormat1Specs =
    import Encoders.V1.given

    suite("When JsonFormatVersion == 1")(
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
