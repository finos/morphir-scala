package org.finos.morphir.datamodel.json.zio

import org.finos.morphir.datamodel._
import org.finos.morphir.datamodel.json.zio.codecs._
import org.scalacheck.Prop._
import zio.json._

class ZioJsonSuite extends munit.ScalaCheckSuite {

  checkBasicTypeEncodings()

  def checkBasicTypeEncodings() =
    BasicDataType.all.foreach { basicType =>
      test(s"BasicDataType ($basicType) should support encoding with zio-json") {
        val actual = basicType.toJson
        assertEquals(actual, "")
      }
    }
}
