package org.finos.morphir.ir

import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
import org.finos.morphir.ir.Type.Field
import org.finos.morphir.ir.Type.Type
import org.finos.morphir.ir.Type.Type.*
import org.finos.morphir.testing.MorphirBaseSpec
import upickle.AttributeTagged
import zio.test.*

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
    } +
      suite("IRTypeReaders Spec") {
        val sut = new IRTypeReaders with AttributeTagged {}
        suite("FieldTypeReader")(
          //   test("Should support reading Morphir IR Field")(
          //     expectAllEqual(
          //       """{"name":["key","1"],"tpe":["Unit",1]}""" -> Field[Int](Name("key1"), Unit[Int](1)),
          //       """{"name":["key","2"],"tpe":["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]]}""" ->
          //         Field[Int](Name("key2"), Tuple[Int](2, List(Variable[Int](2, Name("x")), Variable[Int](2, Name("y")))))
          //     )(input => sut.read[Field[Int]](input)(sut.FieldTypeReader(sut.IntReader)))
          //   )
          // ) +
          // suite("ExtensibleRecordTypeReader")(
          //   test("Should support reading Morphir IR Type ExtensibleRecord")(
          //     expectAllEqual(
          //       """["ExtensibleRecord",1,["ext","rec"],[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]""" ->
          //         ExtensibleRecord[Int](
          //           1,
          //           Name("ExtRec"),
          //           List(
          //             Field[Int](Name("key1"), Variable[Int](2, Name("x"))),
          //             Field[Int](Name("key2"), Variable[Int](2, Name("y")))
          //           )
          //         )
          //     )(input => sut.read[ExtensibleRecord[Int]](input)(sut.ExtensibleRecordTypeReader(sut.IntReader)))
          //   )
          // ) +
          // suite("FunctionTypeReader")(
          //   test("Should support reading Morphir IR Type Function")(
          //     expectAllEqual(
          //       """["Function",1,["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]],["Unit",3]]""" ->
          //         Function[Int](
          //           1,
          //           Tuple[Int](2, List(Variable[Int](2, Name("x")), Variable[Int](2, Name("y")))),
          //           Unit[Int](3)
          //         )
          //     )(input => sut.read[Function[Int]](input)(sut.FunctionTypeReader(sut.IntReader)))
          //   )
          // ) +
          // suite("RecordTypeReader")(
          //   test("Should support reading Morphir IR Type Record")(
          //     expectAllEqual(
          //       """["Record",1,[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]""" ->
          //         Record[Int](
          //           1,
          //           List(
          //             Field[Int](Name("key1"), Variable[Int](2, Name("x"))),
          //             Field[Int](Name("key2"), Variable[Int](2, Name("y")))
          //           )
          //         )
          //     )(input => sut.read[Record[Int]](input)(sut.RecordTypeReader(sut.IntReader)))
          //   )
          // ) +
          // suite("ReferenceTypeReader")(
          //   test("Should support reading Morphir IR Type Reference")(
          //     expectAllEqual(
          //       """["Reference",1,[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]],[["Unit",1],["Variable",2,["y"]]]]""" ->
          //         Reference[Int](
          //           1,
          //           FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName")),
          //           List(Unit[Int](1), Variable[Int](2, Name("y")))
          //         )
          //     )(input => sut.read[Reference[Int]](input)(sut.ReferenceTypeReader(sut.IntReader)))
          //   )
          // ) +
          // suite("TupleTypeReader")(
          //   test("Should support reading Morphir IR Type Tuple")(
          //     expectAllEqual(
          //       """["Tuple",1,[["Variable",1,["y"]]]]""" -> Tuple[Int](1, List(Variable[Int](1, Name("y")))),
          //       """["Tuple",2,[["Unit",2],["Variable",2,["y"]]]]""" ->
          //         Tuple[Int](2, List(Unit[Int](2), Variable[Int](2, Name("y"))))
          //     )(input => sut.read[Tuple[Int]](input)(sut.TupleTypeReader(sut.IntReader)))
          //   )
          // ) +
          suite("UnitTypeReader")(
            test("Should support reading Morphir IR Type Unit")(
              expectAllEqual(
                """["Unit",1]""" -> Unit[Int](1)
              )(input => sut.read[Unit[Int]](input)(sut.UnitTypeReader(sut.IntReader)))
            )
          ) +
            suite("VariableTypeReader")(
              test("Should support reading Morphir IR Type Variable")(
                expectAllEqual(
                  """["Variable",1,["y"]]""" -> Variable[Int](1, Name("y"))
                )(input => sut.read[Variable[Int]](input)(sut.VariableTypeReader(sut.IntReader)))
              )
            )
        )
      }
  }
}
