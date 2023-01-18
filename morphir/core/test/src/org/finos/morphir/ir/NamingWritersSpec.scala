package org.finos.morphir.ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
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
        Name("alpha") -> """["alpha"]""",
        Name("beta")  -> """["beta"]""",
        Name("gamma") -> """["gamma"]"""
      )(name => sut.NameWriter.write(StringRenderer(), name).toString)
    ) +
      suite("PathWriter")(
        test("Should support writing Morphir IR Paths")(
          expectAllEqual(
            Path("alpha", "beta", "gamma")      -> """[["alpha"],["beta"],["gamma"]]""",
            Path("kebabs-rule", "do-you-agree") -> """[["kebabs","rule"],["do","you","agree"]]"""
          )(path => sut.PathWriter.write(StringRenderer(), path).toString)
        )
      ) +
      suite("PackageNameWriter")(
        test("Should support writing package names")(
          expectAllEqual(
            PackageName("morphir", "sdk")          -> """[["morphir"],["sdk"]]""",
            PackageName("Com", "MyCompany", "SDK") -> """[["com"],["my","company"],["s","d","k"]]"""
          )(pkg => sut.PackageNameWriter.write(StringRenderer(), pkg).toString)
        )
      ) +
      suite("QNameWriter")(
        test("Should support writing qualified names")(
          expectAllEqual(
            QName(Path("core", "macros"), Name("Errors")) -> """[[["core"],["macros"]],["errors"]]""",
            QName(Path("core-ir", "sdk"), Name("QName"))  -> """[[["core","ir"],["sdk"]],["q","name"]]"""
          )(qName => sut.QNameWriter.write(StringRenderer(), qName).toString)
        )
      ) +
      suite("FQNameWriter")(
        test("Should support writing fully qualified names")(
          expectAllEqual(
            FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName"))
              -> """[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]]"""
          )(fqName => sut.FQNameWriter.write(StringRenderer(), fqName).toString)
        )
      ) +
      suite("ModuleNameWriter")(
        test("Should support writing module names")(
          expectAllEqual(
            ModuleName(Path("core", "macros"), Name("Errors")) -> """[[["core"],["macros"]],["errors"]]""",
            ModuleName(Path("core-ir", "sdk"), Name("QName"))  -> """[[["core","ir"],["sdk"]],["q","name"]]"""
          )(moduleName => sut.ModuleNameWriter.write(StringRenderer(), moduleName).toString)
        )
      )
  }
}
