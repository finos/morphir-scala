package org.finos.morphir.ir

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import ujson.StringRenderer
import upickle.AttributeTagged
import upickle.core.Annotator
object NamingWritersSpec extends MorphirBaseSpec {
  def spec = suite("NamingWriters Spec") {
    val sut = new NamingWriters with AttributeTagged {}
    suite("NameWriter")(
      test("Should be able to write simple names") {
        expectAllEqual(
          Name("alpha") -> """["alpha"]""",
          Name("beta")  -> """["beta"]""",
          Name("gamma") -> """["gamma"]"""
        )(name => sut.NameWriter.write(StringRenderer(), name).toString)
      }
    )
  }
}
