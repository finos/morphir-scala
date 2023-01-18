package org.finos.morphir.ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.testing.MorphirBaseSpec
import upickle.AttributeTagged
object NamingReadersSpec extends MorphirBaseSpec {
  def spec = suite("NamingReaders Spec") {
    val sut = new NamingReaders with AttributeTagged {}
    suite("NameReader") {
      test("Should support reading Names")(
        expectAllEqual(
          """["hello"]"""         -> Name("hello"),
          """["hello","world"]""" -> Name("hello", "world"),
          """["a", "p", "i"]"""   -> Name("API")
        )(input => sut.read[Name](input)(sut.NameReader))
      )
    }
  }
}
