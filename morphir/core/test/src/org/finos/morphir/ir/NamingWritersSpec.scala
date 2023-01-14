package org.finos.morphir.ir

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import ujson.StringRenderer
import upickle.AttributeTagged
import upickle.core.Annotator
object NamingWritersSpec extends MorphirBaseSpec {
  def spec = suite("NamingWriters Spec") {
    val sut = new NamingWriters with AttributeTagged {}
    suite("NameWriter")(
      tableTest("Writing simple names")("name")(
        Name.Name("alpha") -> """["alpha"]""",
        Name.Name("beta")  -> """["beta"]""",
        Name.Name("gamma") -> """["gamma"]"""
      )(name => sut.NameWriter.write(StringRenderer(), name).toString)
    ) +
      suite("PathWriter")(
        test("Should support writing Morphir IR Paths")(
          expectAllEqual(
            Path.Path("alpha", "beta", "gamma")      -> """[["alpha"],["beta"],["gamma"]]""",
            Path.Path("kebabs-rule", "do-you-agree") -> """[["kebabs","rule"],["do","you","agree"]]"""
          )(path => sut.PathWriter.write(StringRenderer(), path).toString)
        )
      ) +
      suite("PackageNameWriter")(
        test("Should support writing package names")(
          expectAllEqual(
            Package.PackageName("morphir", "sdk")          -> """[["morphir"],["sdk"]]""",
            Package.PackageName("Com", "MyCompany", "SDK") -> """[["com"],["my","company"],["s","d","k"]]"""
          )(pkg => sut.PackageNameWriter.write(StringRenderer(), pkg).toString)
        )
      )
  }
}
