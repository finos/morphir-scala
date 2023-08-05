package org.finos.morphir.ir
package json

import zio.json._
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.Module.{
  ModuleName,
  QualifiedModuleName,
  Definition => ModuleDefinition,
  Specification => ModuleSpecification
}
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.Type.{Type, Definition => TypeDefinition, Specification => TypeSpecification}
import org.finos.morphir.ir.Value.{Pattern, Value, Definition => ValueDefinition, Specification => ValueSpecification}
import org.finos.morphir.ir._
import org.finos.morphir.ir.json.MorphirJsonFileSupport._
import org.finos.morphir.ir.json.util.CustomAssert._
import zio.test.*

object MorphirJsonMorphirIRFileSpec extends ZIOSpecDefault {
  def spec = suite("Json Codec Suite")(
    suite("MorphirIRFile Version 1")(
      test("encoding") {
        val actual = MorphirIRFile(MorphirIRVersion.V1_0, library)
        val expected =
          """{"formatVersion":1,"distribution":["library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[{"name":[["org"],["src"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["unit",[]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["unit",[]]],[["name","2"],["unit",[]]]],"output":["unit",[]]}]]]}},{"name":[["org"],["test"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["unit",[]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["unit",[]]],[["name","2"],["unit",[]]]],"output":["unit",[]]}]]]}}]}]],{"modules":[{"name":[["org"],["src"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["unit",[]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],["unit",[]],["unit",[]]],[["name","2"],["unit",[]],["unit",[]]]],"outputType":["unit",[]],"body":["constructor",["unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]},{"name":[["org"],["test"]],"def":["private",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["unit",[]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],["unit",[]],["unit",[]]],[["name","2"],["unit",[]],["unit",[]]]],"outputType":["unit",[]],"body":["constructor",["unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]}]}]}"""
        assert(actual.toJson)(stringEqualTo(expected))
      },
      test("decoding") {
        val expected = MorphirIRFile(MorphirIRVersion.V1_0, library)
        val actual =
          """{"formatVersion":1,"distribution":["library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[{"name":[["org"],["src"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["unit",[]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["unit",[]]],[["name","2"],["unit",[]]]],"output":["unit",[]]}]]]}},{"name":[["org"],["test"]],"spec":{"types":[[["name"],["typeDoc1",["type_alias_specification",[["name","1"],["name","2"]],["unit",[]]]]]],"values":[[["name"],["valueDoc1",{"inputs":[[["name","1"],["unit",[]]],[["name","2"],["unit",[]]]],"output":["unit",[]]}]]]}}]}]],{"modules":[{"name":[["org"],["src"]],"def":["public",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["unit",[]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],["unit",[]],["unit",[]]],[["name","2"],["unit",[]],["unit",[]]]],"outputType":["unit",[]],"body":["constructor",["unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]},{"name":[["org"],["test"]],"def":["private",{"types":[[["name"],["private",["typeDoc1",["type_alias_definition",[["name","1"],["name","2"]],["unit",[]]]]]]],"values":[[["name"],["private",["valueDoc1",{"inputTypes":[[["name","1"],["unit",[]],["unit",[]]],[["name","2"],["unit",[]],["unit",[]]]],"outputType":["unit",[]],"body":["constructor",["unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}]]]]}]}]}]}"""
        assert(actual.fromJson[MorphirIRFile])(objectEqualTo(Right(expected)))
      }
    ),
    suite("MorphirIRFile Version 2")(
      test("encoding") {
        val actual = MorphirIRFile(MorphirIRVersion.V2_0, library)
        val expected =
          """{"formatVersion":2,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assert(actual.toJson)(stringEqualTo(expected))
      },
      test("decoding") {
        val expected = MorphirIRFile(MorphirIRVersion.V2_0, library)
        val actual =
          """{"formatVersion":2,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assert(actual.fromJson[MorphirIRFile])(objectEqualTo(Right(expected)))
      }
    ),
    suite("MorphirIRFile Version 3")(
      test("encoding") {
        val actual = MorphirIRFile(MorphirIRVersion.V3_0, library)
        val expected =
          """{"formatVersion":3,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assert(actual.toJson)(stringEqualTo(expected))
      },
      test("decoding") {
        val expected = MorphirIRFile(MorphirIRVersion.V3_0, library)
        val actual =
          """{"formatVersion":3,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assert(actual.fromJson[MorphirIRFile])(objectEqualTo(Right(expected)))
      }
    )
  )

  def library: Library = {
    val packageName = PackageName.fromString("morphir.SDK")
    val name        = Name.fromString("name")
    val name1       = Name.fromString("name1")
    val name2       = Name.fromString("name2")
    val modName1    = ModuleName.fromString("org.src")
    val modName2    = ModuleName.fromString("org.test")

    val specTypeMap = Map(
      name -> Documented(
        "typeDoc1",
        TypeSpecification.TypeAliasSpecification[scala.Unit](zio.Chunk(name1, name2), unit)
      )
    )
    val inputs       = zio.Chunk((name1, unit), (name2, unit))
    val specValueMap = Map(name -> Documented("valueDoc1", ValueSpecification[scala.Unit](inputs, unit)))

    val modSpec = ModuleSpecification[scala.Unit](specTypeMap, specValueMap)
    val pkgSpec = PackageSpecification[scala.Unit](Map(modName1 -> modSpec, modName2 -> modSpec))

    val inputParams = zio.Chunk(
      (name1, unit, unit),
      (name2, unit, unit)
    )
    val value    = Value.Constructor(unit, FQName.fromString("test:JavaHome:morphir"))
    val valueDef = ValueDefinition(inputParams, unit, value)

    val defValueMap =
      Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

    val defTypeMap = Map(
      name -> AccessControlled(
        AccessControlled.Access.Private,
        Documented(
          "typeDoc1",
          TypeDefinition.TypeAlias(zio.Chunk(name1, name2), unit)
        )
      )
    )

    val modDef = ModuleDefinition(defTypeMap, defValueMap)
    val dependencies = Map[PackageName, UPackageSpecification](
      PackageName.fromString("org.finos.morphir.ir") -> pkgSpec
    )
    val packageDef: PackageDefinition.Typed = PackageDefinition(
      Map(
        modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
        modName2 -> AccessControlled(AccessControlled.Access.Private, modDef)
      )
    )
    Library(packageName, dependencies, packageDef)
  }
}
