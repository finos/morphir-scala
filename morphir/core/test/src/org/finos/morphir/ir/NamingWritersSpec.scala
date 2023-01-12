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
      tableTest("Writing simple names")("name")(
        Names.Name("alpha") -> """["alpha"]""",
        Names.Name("beta")  -> """["beta"]""",
        Names.Name("gamma") -> """["gamma"]"""
      )(name => sut.NameWriter.write(StringRenderer(), name).toString)
    ) +
      suite("PathWriter")(
        test("Should support writing Morphir IR Paths")(
          expectAllEqual(
            Paths.Path("alpha", "beta", "gamma")      -> """[["alpha"],["beta"],["gamma"]]""",
            Paths.Path("kebabs-rule", "do-you-agree") -> """[["kebabs","rule"],["do","you","agree"]]"""
          )(path => sut.PathWriter.write(StringRenderer(), path).toString)
        )
      )
  }
}
