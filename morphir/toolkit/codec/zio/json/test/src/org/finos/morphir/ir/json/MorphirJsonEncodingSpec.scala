package org.finos.morphir
package ir
package json

import zio._
import zio.json._
import org.finos.morphir.ir.distribution.Distribution._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.Module.{
  Definition => ModuleDefinition,
  ModuleName,
  ModulePath,
  Specification => ModuleSpecification
}
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type.{Constructors, Definition => TypeDefinition, Specification => TypeSpecification, _}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Pattern, Specification => ValueSpecification, Value}
import org.finos.morphir.ir.json.MorphirJsonSupport._
import zio.test.{ZIOSpecDefault, _}

object MorphirJsonEncodingSpec extends ZIOSpecDefault {
  def spec = suite("Json Encoding Suite")(
    suite("Unit")(
      test("will encode a Unit") {
        val actual   = ()
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Name")(
      test("will encode an empty Name") {
        val actual   = Name.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a single Name") {
        val actual   = Name("Hello")
        val expected = """["hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name") {
        val actual   = Name("HelloThere")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromString") {
        val actual   = Name.fromString("Hello.There")
        val expected = """["hello","there"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Name fromList") {
        val actual   = Name.fromList(List("This", "is", "a", "list"))
        val expected = """["this","is","a","list"]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Path")(
      test("will encode an empty Path") {
        val actual   = Path.empty
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = Path.fromString("org")
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = Path.fromString("org.foo.bar")
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModulePath")(
      test("will encode an empty Path") {
        val actual   = ModulePath(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = ModulePath(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = ModulePath(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("PackageName")(
      test("will encode an empty Path") {
        val actual   = PackageName(Path.empty)
        val expected = "[]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple Path") {
        val actual   = PackageName(Path.fromString("org"))
        val expected = """[["org"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a Path") {
        val actual   = PackageName(Path.fromString("org.foo.bar"))
        val expected = """[["org"],["foo"],["bar"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModuleName")(
      test("will encode an empty ModuleName") {
        val actual   = ModuleName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a simple ModuleName") {
        val actual   = ModuleName(Path.fromString("org"), Name.fromString("SrcTest"))
        val expected = """[[["org"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a ModuleName") {
        val actual   = ModuleName(Path.fromString("src.test.scala"), Name.fromString("SrcTest"))
        val expected = """[[["src"],["test"],["scala"]],["src","test"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("QName")(
      test("will encode an empty QName") {
        val actual   = QName(Path.empty, Name.empty)
        val expected = "[[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a QName") {
        val actual   = QName.fromString("Proper.Path:name")
        val expected = """[[["proper"],["path"]],["name"]]"""
        assertTrue(actual.get.toJson == expected)
      }
    ),
    suite("FQName")(
      test("will encode an empty FQName") {
        val actual   = FQName(Path.empty, Path.empty, Name.empty)
        val expected = "[[],[],[]]"
        assertTrue(actual.toJson == expected)
      },
      test("will encode a FQName") {
        val actual   = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val expected = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Documented")(
      test("will encode Documented for Integer") {
        val actual   = Documented("This is an Integer 10", 10)
        val expected = """{"doc":"This is an Integer 10","value":10}"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Documented for String") {
        val actual   = Documented("This is a String", "Hello")
        val expected = """{"doc":"This is a String","value":"Hello"}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("AccessControlled")(
      test("will encode AccessControlled for private Integer") {
        val actual   = AccessControlled(AccessControlled.Access.Private, 10)
        val expected = """{"access":"Private","value":10}"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode AccessControlled for public String") {
        val actual   = AccessControlled(AccessControlled.Access.Public, "Hello")
        val expected = """{"access":"Public","value":"Hello"}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Field")(
      test("will encode Field for private Integer") {
        val actual   = Field(Name.fromString("Name"), AccessControlled(AccessControlled.Access.Private, 10))
        val expected = """{"name":["name"],"tpe":{"access":"Private","value":10}}"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Field for public String") {
        val actual =
          Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
        val expected = """{"name":["string"],"tpe":{"access":"Public","value":"Hello"}}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Literal")(
      test("will encode a BoolLiteral") {
        val actual   = BoolLiteral(true)
        val expected = """["BoolLiteral",true]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a CharLiteral") {
        val actual   = CharLiteral('x')
        val expected = """["CharLiteral","x"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a DecimalLiteral") {
        val actual   = DecimalLiteral(new java.math.BigDecimal("1.23456789"))
        val expected = """["DecimalLiteral","1.23456789"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a FloatLiteral") {
        val actual   = FloatLiteral(1.3232d)
        val expected = """["FloatLiteral",1.3232]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode a StringLiteral") {
        val actual   = StringLiteral("hello")
        val expected = """["StringLiteral","hello"]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode an WholeNumberLiteral") {
        val actual   = WholeNumberLiteral(321321L)
        val expected = """["WholeNumberLiteral",321321]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Type")(
      test("will encode TypeCase.Unit") {
        val actual   = unit[Int](1234)
        val expected = """["Unit",1234]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.VariableCase") {
        val actual   = variable[Int](1234, "x")
        val expected = """["Variable",1234,["x"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Field") {
        val actual   = Field(Name("someField"), variable[Int](1234, "x"))
        val expected = """{"name":["some","field"],"tpe":["Variable",1234,["x"]]}"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.RecordCase") {
        val var1   = Field(Name("first"), variable[Int](123, "f"))
        val var2   = Field(Name("second"), variable[Int](345, "g"))
        val actual = record(1, zio.Chunk(var1, var2))
        val expected =
          """["Record",1,[{"name":["first"],"tpe":["Variable",123,["f"]]},{"name":["second"],"tpe":["Variable",345,["g"]]}]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.ExtensibleRecordCase") {
        val var1   = Field(Name("first"), variable[Int](123, "f"))
        val var2   = Field(Name("second"), variable[Int](345, "g"))
        val actual = extensibleRecord(1, Name.fromString("someName"), zio.Chunk(var1, var2))
        val expected =
          """["ExtensibleRecord",1,["some","name"],[{"name":["first"],"tpe":["Variable",123,["f"]]},{"name":["second"],"tpe":["Variable",345,["g"]]}]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.TupleCase") {
        val var1     = variable[Int](123, "f")
        val var2     = variable[Int](345, "g")
        val actual   = tuple(1, var1, var2)
        val expected = """["Tuple",1,[["Variable",123,["f"]],["Variable",345,["g"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.ReferenceCase") {
        val var1   = variable[Int](123, "f")
        val var2   = variable[Int](345, "g")
        val actual = reference(1, FQName.fromString("test:JavaHome:morphir"), zio.Chunk(var1, var2))
        val expected =
          """["Reference",1,[[["test"]],[["java","home"]],["morphir"]],[["Variable",123,["f"]],["Variable",345,["g"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TypeCase.FunctionCase") {
        val var1   = variable[Int](123, "f")
        val var2   = variable[Int](345, "g")
        val actual = function(1, var1, var2)
        val expected =
          """["Function",1,["Variable",123,["f"]],["Variable",345,["g"]]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Constructors")(
      test("will encode empty Constructor") {
        val actual   = Constructors[Int](Map.empty)
        val expected = """[]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Constructors with one constructor") {
        val name     = Name.fromString("name")
        val actual   = Constructors[Int](Map((name, zio.Chunk((name, variable[Int](123, "f"))))))
        val expected = """[[["name"],[[["name"],["Variable",123,["f"]]]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Constructors") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val actual = Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        val expected =
          """[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("org.finos.morphir.ir.Type.Definition")(
      test("will encode TypeAlias") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = TypeDefinition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        val expected = """["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode CustomType") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = AccessControlled(
          AccessControlled.Access.Public,
          Constructors[Int](
            Map(
              (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
              (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
            )
          )
        )
        val actual = TypeDefinition.CustomType[Int](zio.Chunk(name1, name2), ctors)
        val expected =
          """["CustomTypeDefinition",[["name","1"],["name","2"]],{"access":"Public","value":[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]}]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("org.finos.morphir.ir.Type.Specification")(
      test("will encode TypeAliasSpecification") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val actual =
          TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        val expected = """["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode CustomTypeSpecification") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        val actual = TypeSpecification.CustomTypeSpecification[Int](zio.Chunk(name1, name2), ctors)
        val expected =
          """["CustomTypeSpecification",[["name","1"],["name","2"]],[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode OpaqueTypeSpecification") {
        val name1  = Name.fromString("name1")
        val name2  = Name.fromString("name2")
        val actual = TypeSpecification.OpaqueTypeSpecification(zio.Chunk(name1, name2))
        val expected =
          """["OpaqueTypeSpecification",[["name","1"],["name","2"]]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Pattern")(
      test("will encode AsPattern") {
        val actual   = Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        val expected = """["as_pattern",1,["wildcard_pattern",1],["wild","card"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode ConstructorPattern") {
        val patterns: zio.Chunk[Pattern[Int]] = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.EmptyListPattern[Int](2),
          Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        )
        val actual = Pattern.ConstructorPattern[Int](1, FQName.fromString("test:JavaHome:morphir"), patterns)
        val expected =
          """["constructor_pattern",1,[[["test"]],[["java","home"]],["morphir"]],[["wildcard_pattern",1],["empty_list_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode EmptyListPattern") {
        val actual   = Pattern.EmptyListPattern[Int](1)
        val expected = """["empty_list_pattern",1]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode LiteralPattern") {
        val actual   = Pattern.LiteralPattern[Int](1, StringLiteral("hello"))
        val expected = """["literal_pattern",1,["StringLiteral","hello"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode HeadTailPattern") {
        val actual = Pattern.HeadTailPattern[Int](1, Pattern.WildcardPattern[Int](1), Pattern.EmptyListPattern[Int](2))
        val expected = """["head_tail_pattern",1,["wildcard_pattern",1],["empty_list_pattern",2]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode TuplePattern") {
        val patterns: zio.Chunk[Pattern[Int]] = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.UnitPattern[Int](2),
          Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        )
        val actual = Pattern.TuplePattern[Int](1, patterns)
        val expected =
          """["tuple_pattern",1,[["wildcard_pattern",1],["unit_pattern",2],["as_pattern",1,["wildcard_pattern",1],["wild","card"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode UnitPattern") {
        val actual   = Pattern.UnitPattern[Int](1)
        val expected = """["unit_pattern",1]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode WildcardPattern") {
        val actual   = Pattern.WildcardPattern[Int](1)
        val expected = """["wildcard_pattern",1]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ValueDefinition")(
      test("will encode ValueDefinition") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](345, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val actual =
          ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), Value.Unit(1))
        val expected =
          """{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["unit",1]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ValueSpecification")(
      test("will encode ValueSpecification") {
        val inputs = zio.Chunk(
          (Name.fromString("name1"), variable[Int](345, "g")),
          (Name.fromString("name2"), variable[Int](678, "h"))
        )
        val actual = ValueSpecification[Int](inputs, variable[Int](111, "f"))
        val expected =
          """{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"outputs":["Variable",111,["f"]]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModuleSpecification")(
      test("will encode ModuleSpecification") {
        val name  = Name.fromString("name")
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int](345, "g")), (name2, variable[Int](678, "h")))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueSpecification[Int](inputs, variable[Int](111, "f"))))

        val actual = ModuleSpecification[Int](typeMap, valueMap)
        val expected =
          """{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"outputs":["Variable",111,["f"]]}}]]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("ModuleDefinition")(
      test("will encode ModuleDefinition") {
        val name  = Name.fromString("name")
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val inputParams = zio.Chunk(
          (name1, 1, variable[Int](345, "g")),
          (name2, 2, variable[Int](678, "h"))
        )
        val value    = Value.Constructor(1, FQName.fromString("test:JavaHome:morphir"))
        val valueDef = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), value)

        val valueMap =
          Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

        val typeMap = Map(
          name -> AccessControlled(
            AccessControlled.Access.Private,
            Documented(
              "typeDoc1",
              TypeDefinition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )
        val actual = ModuleDefinition[Int, Int](typeMap, valueMap)
        val expected =
          """{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("PackageSpecification")(
      test("will encode PackageSpecification") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2 = ModuleName(Path.fromString("org"), Name.fromString("test"))

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int](345, "g")), (name2, variable[Int](678, "h")))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueSpecification[Int](inputs, variable[Int](111, "f"))))

        val modSpec = ModuleSpecification[Int](typeMap, valueMap)
        val actual  = PackageSpecification[Int](Map(modName1 -> modSpec, modName2 -> modSpec))
        val expected =
          """{"modules":[[[[["org"]],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"outputs":["Variable",111,["f"]]}}]]}],[[[["org"]],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"outputs":["Variable",111,["f"]]}}]]}]]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("PackageDefinition")(
      test("will encode PackageDefinition") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2 = ModuleName(Path.fromString("org"), Name.fromString("test"))

        val inputParams = zio.Chunk(
          (name1, 1, variable[Int](345, "g")),
          (name2, 2, variable[Int](678, "h"))
        )
        val value    = Value.Constructor(1, FQName.fromString("test:JavaHome:morphir"))
        val valueDef = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), value)

        val valueMap =
          Map(name -> AccessControlled(AccessControlled.Access.Private, Documented("valueDoc1", valueDef)))

        val typeMap = Map(
          name -> AccessControlled(
            AccessControlled.Access.Private,
            Documented(
              "typeDoc1",
              TypeDefinition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )

        val modDef = ModuleDefinition[Int, Int](typeMap, valueMap)
        val actual = PackageDefinition[Int, Int](
          Map(
            modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
            modName2 -> AccessControlled(AccessControlled.Access.Public, modDef)
          )
        )

        val expected =
          """{"modules":[[[[["org"]],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[[["org"]],["test"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Value")(
      test("will encode Value - Apply") {
        val unitCase = Value.Unit(6)
        val actual   = Value.Apply[Unit, Int](3, unitCase, unitCase)
        val expected = """["apply",3,["unit",6],["unit",6]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Constructor") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = Value.Constructor(3, name)
        val expected = """["constructor",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Destructure") {
        val pattern  = Pattern.WildcardPattern[Int](1)
        val unitCase = Value.Unit(6)
        val actual   = Value.Destructure[Unit, Int](3, pattern, unitCase, unitCase)
        val expected = """["destructure",3,["wildcard_pattern",1],["unit",6],["unit",6]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Field") {
        val name     = Name("Hello")
        val unitCase = Value.Unit(6)
        val actual   = Value.Field[Unit, Int](3, unitCase, name)
        val expected = """["field",3,["unit",6],["hello"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - FieldFunction") {
        val actual   = Value.FieldFunction(3, Name("Hello"))
        val expected = """["field_function",3,["hello"]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - IfThenElse") {
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val unitCase          = Value.Unit(6)
        val actual            = Value.IfThenElse[Unit, Int](3, unitCase, fieldFunctionCase, unitCase)
        val expected          = """["if_then_else",3,["unit",6],["field_function",3,["hello"]],["unit",6]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Lambda") {
        val pattern           = Pattern.WildcardPattern[Int](1)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val actual            = Value.Lambda[Unit, Int](3, pattern, fieldFunctionCase)
        val expected          = """["lambda",3,["wildcard_pattern",1],["field_function",3,["hello"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - LetDefinition") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](444, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val literalCase     = Value.Literal(3, BoolLiteral(true))
        val valueDefinition = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), literalCase)

        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))

        val actual = Value.LetDefinition(3, Name("Hi"), valueDefinition, fieldFunctionCase)
        val expected =
          """["let_definition",3,["hi"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["literal",3,["BoolLiteral",true]]},["field_function",3,["hello"]]]"""
        assertTrue(actual.toJson == expected)
      },
      // test("will encode and decode Value - LetDefinition - with Any") {
      //   object Refs {
      //     val `morphir.SDK`            = PackageName.fromString("morphir.SDK")
      //     val `basics`                 = Module.ModulePath.fromString("basics")
      //     val `list`                   = Module.ModulePath.fromString("list")
      //     val `morphir.SDK.basics.int` = FQName(`morphir.SDK`, `basics`, Name("int"))
      //     val `morphir.SDK.list.list`  = FQName(`morphir.SDK`, `list`, Name("list"))
      //   }

      //   val `sdk.Int`                  = ir.Type.reference(Refs.`morphir.SDK.basics.int`)
      //   val `List[sdk.Int]`            = ir.Type.reference(Refs.`morphir.SDK.list.list`, `sdk.Int`)
      //   val `sdk.Int => List[sdk.Int]` = ir.Type.function(`sdk.Int`, `List[sdk.Int]`)

      //   // final case class LetDefinitionCase[+TA, +VA, +TypeRepr[+_], +Self](

      //   /*
      //   ValueCase.LetDefinitionCase[
      //     Any,UType,Type,
      //     ValueCase[Nothing,UType,ValueCase[Nothing,UType,Nothing]]
      //   ]
      //    */

      //   // ValueCase.ApplyCase[UType,ValueCase[Nothing,UType,Nothing]]
      //   val in =
      //     Value.Apply(
      //       `List[sdk.Int]`,
      //       Value.Variable(`sdk.Int => List[sdk.Int]`, Name("foo")),
      //       Value.Literal(`sdk.Int`, Literal.int(1))
      //     )

      //   // ValueCase.ListCase[UType,ValueCase.VariableCase[UType]]
      //   val body =
      //     Value.List(
      //       `List[sdk.Int]`,
      //       Chunk(Value.Variable(`sdk.Int`, Name("x")))
      //     )

      //   val actual =
      //     Value.LetDefinition(
      //       // o: List Int (minus the 'o' part)
      //       `List[sdk.Int]`,
      //       // foo: a -> List a
      //       // foo x = [x]
      //       Name("foo"),
      //       ValueDefinition(
      //         inputTypes = Chunk((Name("x"), `sdk.Int`, ir.Type.variable("a"))),
      //         outputType = ir.Type.reference(Refs.`morphir.SDK.list.list`, Chunk(ir.Type.variable("a"))),
      //         // ValueCase[Nothing, UType, ValueCase[Nothing, UType, Nothing]]
      //         body = body
      //       ),
      //       // in
      //       //   (foo 1)
      //       // ValueCase[Nothing, UType, Nothing]
      //       in
      //       // ValueCase.LiteralCase(`sdk.Int`, Literal.int(1))
      //     )

      //   // TODO Compare actual with expected which is this long string, the printline function actual.toJson is doing the right thing.
      //   // If you want to verify it, use try-morphir.html from https://github.com/finos/morphir and put in the following:
      //   /*
      //   module My.Test exposing (..)

      //   o: List Int
      //   o =
      //     let
      //       foo: a -> List a
      //       foo x = [x]
      //     in
      //       (foo 1)
      //    */
      //   val jsonValue    = actual.toJson
      //   val decodedValue = jsonValue.fromJson[Value[Any, UType]]

      //   assertTrue(decodedValue == Right(actual))
      // },
      test("will encode Value - LetRecursion") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](444, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val literalCase          = Value.Literal(3, BoolLiteral(true))
        val valueDefinitionCase1 = ValueDefinition[Int, Int](inputParams, variable[Int](333, "x"), literalCase)
        val valueDefinitionCase2 = ValueDefinition[Int, Int](inputParams, variable[Int](444, "y"), literalCase)
        val valueDefinitions =
          Map(Name.fromString("key1") -> valueDefinitionCase1, Name.fromString("key2") -> valueDefinitionCase1)

        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val actual            = Value.LetRecursion(3, valueDefinitions, fieldFunctionCase)
        val expected =
          """["let_recursion",3,[[["key","1"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",333,["x"]],"body":["literal",3,["BoolLiteral",true]]}],[["key","2"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",333,["x"]],"body":["literal",3,["BoolLiteral",true]]}]],["field_function",3,["hello"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - ListCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val actual            = Value.List(3, zio.Chunk[Value[Int, Int]](unitCase, fieldFunctionCase))
        val expected          = """["list",3,[["unit",6],["field_function",3,["hello"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Literal") {
        val literal  = BoolLiteral(true)
        val actual   = Value.Literal(3, literal)
        val expected = """["literal",3,["BoolLiteral",true]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - PatternMatch") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val patterns          = zio.Chunk((Pattern.WildcardPattern[Int](12), fieldFunctionCase))
        val actual            = Value.PatternMatch[Unit, Int](3, unitCase, patterns)
        val expected = """["pattern_match",3,["unit",6],[[["wildcard_pattern",12],["field_function",3,["hello"]]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Record") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase))
        val actual            = Value.Record[Unit, Int](3, fields)
        val expected          = """["record",3,[[["hello"],["field_function",3,["hello"]]],[["there"],["unit",6]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Reference") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = Value.Reference(3, name)
        val expected = """["reference",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Tuple") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val elements          = zio.Chunk(unitCase, fieldFunctionCase)
        val actual            = Value.Tuple[Unit, Int](3, elements)
        val expected          = """["tuple",3,[["unit",6],["field_function",3,["hello"]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - UpdateRecord") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase)).toMap
        val actual            = Value.UpdateRecord[Unit, Int](3, unitCase, fields)
        val expected =
          """["update_record",3,["unit",6],[[["hello"],["field_function",3,["hello"]]],[["there"],["unit",6]]]]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Unit") {
        val actual   = Value.Unit(6)
        val expected = """["unit",6]"""
        assertTrue(actual.toJson == expected)
      },
      test("will encode Value - Variable") {
        val actual   = Value.Variable(3, Name("hello"))
        val expected = """["variable",3,["hello"]]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("Distribution")(
      test("will encode Distribution.Library") {
        val packageName = PackageName.fromString("morphir.SDK")
        val name        = Name.fromString("name")
        val name1       = Name.fromString("name1")
        val name2       = Name.fromString("name2")
        val modName1    = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2    = ModuleName(Path.fromString("org"), Name.fromString("test"))

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
        val actual = Library(packageName, dependencies, packageDef)
        val expected =
          """["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[[["org"]],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"outputs":["Unit",[]]}}]]}],[[[["org"]],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"outputs":["Unit",[]]}}]]}]]}]],{"modules":[[[[["org"]],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[[["org"]],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]"""
        assertTrue(actual.toJson == expected)
      }
    ),
    suite("MorphirIRFile")(
      test("will encode Distribution.Library") {
        val packageName = PackageName.fromString("morphir.SDK")
        val name        = Name.fromString("name")
        val name1       = Name.fromString("name1")
        val name2       = Name.fromString("name2")
        val modName1    = ModuleName(Path.fromString("org"), Name.fromString("src"))
        val modName2    = ModuleName(Path.fromString("org"), Name.fromString("test"))

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
        val actual = MorphirIRFile(MorphirIRVersion.V2_0, Library(packageName, dependencies, packageDef))
        val expected =
          """{"formatVersion":2,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[[["org"]],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"outputs":["Unit",[]]}}]]}],[[[["org"]],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"outputs":["Unit",[]]}}]]}]]}]],{"modules":[[[[["org"]],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[[["org"]],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assertTrue(actual.toJson == expected)
      }
    )
  )
}
