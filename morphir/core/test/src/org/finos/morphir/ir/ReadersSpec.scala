package org.finos.morphir.ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
import org.finos.morphir.testing.MorphirBaseSpec
import upickle.AttributeTagged

object ReadersSpec extends MorphirBaseSpec {
  def spec = suite("Readers Spec") {
    suite("NamingReaders Spec") {
      val sut = new NamingReaders with AttributeTagged {}
      suite("NameReader")(
        test("Should support reading Names")(
          expectAllEqual(
            """["hello"]"""         -> Name("hello"),
            """["hello","world"]""" -> Name("hello", "world"),
            """["a", "p", "i"]"""   -> Name("API")
          )(input => sut.read[Name](input)(sut.NameReader))
        )
      ) +
        suite("PathReader")(
          test("Should support reading Morphir IR Paths")(
            expectAllEqual(
              """[["alpha"],["beta"],["gamma"]]"""           -> Path("alpha", "beta", "gamma"),
              """[["kebabs","rule"],["do","you","agree"]]""" -> Path("kebabs-rule", "do-you-agree")
            )(input => sut.read[Path](input)(sut.PathReader))
          )
        ) +
        suite("PackageNameReader")(
          test("Should support reading package names")(
            expectAllEqual(
              """[["morphir"],["sdk"]]"""                    -> PackageName("morphir", "sdk"),
              """[["com"],["my","company"],["s","d","k"]]""" -> PackageName("Com", "MyCompany", "SDK")
            )(input => sut.read[PackageName](input)(sut.PackageNameReader))
          )
        ) +
        suite("QNameReader")(
          test("Should support reading qualified names")(
            expectAllEqual(
              """[[["core"],["macros"]],["errors"]]"""     -> QName(Path("core", "macros"), Name("Errors")),
              """[[["core","ir"],["sdk"]],["q","name"]]""" -> QName(Path("core-ir", "sdk"), Name("QName"))
            )(input => sut.read[QName](input)(sut.QNameReader))
          )
        ) +
        suite("FQNameReaderer")(
          test("Should support reading fully qualified names")(
            expectAllEqual(
              """[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]]""" ->
                FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName"))
            )(input => sut.read[FQName](input)(sut.FQNameReader))
          )
        ) +
        suite("ModuleNameReader")(
          test("Should support reading module names")(
            expectAllEqual(
              """[[["core"],["macros"]],["errors"]]"""     -> ModuleName(Path("core", "macros"), Name("Errors")),
              """[[["core","ir"],["sdk"]],["q","name"]]""" -> ModuleName(Path("core-ir", "sdk"), Name("QName"))
            )(input => sut.read[ModuleName](input)(sut.ModuleNameReader))
          )
        )
    }
  }
}
