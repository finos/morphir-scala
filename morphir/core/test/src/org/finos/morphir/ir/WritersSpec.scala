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
import zio.test._
import ujson.StringRenderer
import upickle.AttributeTagged
import upickle.core.Annotator

object WritersSpec extends MorphirBaseSpec {
  def spec = suite("Writers Spec") {
    suite("NamingWriters Spec") {
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
    } +
      suite("IRTypeWriters Spec") {
        val sut = new IRTypeWriters with AttributeTagged {}
        suite("FieldTypeWriter")(
          test("Should support writing Morphir IR Field")(
            expectAllEqual(
              Field[Int]("key1", Unit[Int](1)) -> """{"key1":["Unit",1]}""",
              Field[Int]("key2", Tuple[Int](2, List(Variable[Int](2, Name("x")), Variable[Int](2, Name("y")))))
                -> """{"key2":["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]]}"""
            )(field => sut.FieldTypeWriter(sut.IntWriter).write(StringRenderer(), field).toString)
          )
        ) +
          suite("ExtensibleRecordTypeWriter")(
            test("Should support writing Morphir IR Type ExtensibleRecord")(
              expectAllEqual(
                ExtensibleRecord[Int](
                  1,
                  Name("ExtRec"),
                  List(Field[Int]("key1", Variable[Int](2, Name("x"))), Field[Int]("key2", Variable[Int](2, Name("y"))))
                )
                  -> """["ExtensibleRecord",1,["ext","rec"],[{"key1":["Variable",2,["x"]]},{"key2":["Variable",2,["y"]]}]]"""
              )(rec => sut.ExtensibleRecordTypeWriter(sut.IntWriter).write(StringRenderer(), rec).toString)
            )
          ) +
          suite("FunctionTypeWriter")(
            test("Should support writing Morphir IR Type Function")(
              expectAllEqual(
                Function[Int](
                  1,
                  Tuple[Int](2, List(Variable[Int](2, Name("x")), Variable[Int](2, Name("y")))),
                  Unit[Int](3)
                )
                  -> """["Function",1,["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]],["Unit",3]]"""
              )(func => sut.FunctionTypeWriter(sut.IntWriter).write(StringRenderer(), func).toString)
            )
          ) +
          suite("RecordTypeWriter")(
            test("Should support writing Morphir IR Type Record")(
              expectAllEqual(
                Record[Int](
                  1,
                  List(Field[Int]("key1", Variable[Int](2, Name("x"))), Field[Int]("key2", Variable[Int](2, Name("y"))))
                )
                  -> """["Record",1,[{"key1":["Variable",2,["x"]]},{"key2":["Variable",2,["y"]]}]]"""
              )(rec => sut.RecordTypeWriter(sut.IntWriter).write(StringRenderer(), rec).toString)
            )
          ) +
          suite("ReferenceTypeWriter")(
            test("Should support writing Morphir IR Type Reference")(
              expectAllEqual(
                Reference[Int](
                  1,
                  FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName")),
                  List(Unit[Int](1), Variable[Int](2, Name("y")))
                )
                  -> """["Reference",1,[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]],[["Unit",1],["Variable",2,["y"]]]]"""
              )(ref => sut.ReferenceTypeWriter(sut.IntWriter).write(StringRenderer(), ref).toString)
            )
          ) +
          suite("TupleTypeWriter")(
            test("Should support writing Morphir IR Type Tuple")(
              expectAllEqual(
                Tuple[Int](1, List(Variable[Int](1, Name("y")))) -> """["Tuple",1,[["Variable",1,["y"]]]]""",
                Tuple[Int](2, List(Unit[Int](2), Variable[Int](2, Name("y"))))
                  -> """["Tuple",2,[["Unit",2],["Variable",2,["y"]]]]"""
              )(tuple => sut.TupleTypeWriter(sut.IntWriter).write(StringRenderer(), tuple).toString)
            )
          ) +
          suite("UnitTypeWriter")(
            test("Should support writing Morphir IR Type Unit")(
              expectAllEqual(
                Unit[Int](1) -> """["Unit",1]"""
              )(unit => sut.UnitTypeWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          ) +
          suite("VariableTypeWriter")(
            test("Should support writing Morphir IR Type Variable")(
              expectAllEqual(
                Variable[Int](1, Name("y")) -> """["Variable",1,["y"]]"""
              )(unit => sut.VariableTypeWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          )
      }
  }
}
