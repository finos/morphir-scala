package org.finos.morphir.ir
package json

import org.finos.morphir.naming._
import zio.json._
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.distribution.Distribution._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal._
import org.finos.morphir.ir.Module.{Definition => ModuleDefinition, Specification => ModuleSpecification}
import org.finos.morphir.ir.PackageModule.{
  Definition => PackageDefinition,
  Specification => PackageSpecification,
  USpecification => UPackageSpecification
}
import org.finos.morphir.ir.Type._
import org.finos.morphir.ir.Type.{Definition => TypeDefinition, Specification => TypeSpecification, Type}
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Pattern, Specification => ValueSpecification, Value}
import org.finos.morphir.ir._
import org.finos.morphir.ir.json.MorphirJsonSupport._
import org.finos.morphir.ir.json.util.CustomAssert._
import zio.test._

object MorphirJsonDecodingSpec extends ZIOSpecDefault {
  def spec = suite("Json Decoding Suite")(
    suite("Unit")(
      test("will decode a Unit") {
        val actual   = """[]"""
        val expected = ()
        assert(actual.fromJson[scala.Unit])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Name")(
      test("will decode an empty Name") {
        val actual   = "[]"
        val expected = Name.empty
        assert(actual.fromJson[Name])(objectEqualTo(Right(expected)))
      },
      test("will decode a single Name") {
        val actual   = """["hello"]"""
        val expected = Name("Hello")
        assert(actual.fromJson[Name])(objectEqualTo(Right(expected)))
      },
      test("will decode a Name") {
        val actual   = """["hello","there"]"""
        val expected = Name("HelloThere")
        assert(actual.fromJson[Name])(objectEqualTo(Right(expected)))
      },
      test("will decode a Name fromString") {
        val actual   = """["hello","there"]"""
        val expected = Name.fromString("Hello.There")
        assert(actual.fromJson[Name])(objectEqualTo(Right(expected)))
      },
      test("will decode a Name fromList") {
        val actual   = """["this","is","a","list"]"""
        val expected = Name.fromList(List("this", "is", "a", "list"))
        assert(actual.fromJson[Name])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Path")(
      test("will decode an empty Path") {
        val actual   = "[]"
        val expected = Path.empty
        assert(actual.fromJson[Path])(objectEqualTo(Right(expected)))
      },
      test("will decode a simple Path") {
        val actual   = """[["org"]]"""
        val expected = Path.fromString("org")
        assert(actual.fromJson[Path])(objectEqualTo(Right(expected)))
      },
      test("will decode a Path") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = Path.fromString("org.foo.bar")
        assert(actual.fromJson[Path])(objectEqualTo(Right(expected)))
      }
    ),
    suite("ModulePath")(
      test("will decode an empty ModulePath") {
        val actual   = "[]"
        val expected = ModuleName(Path.empty)
        assert(actual.fromJson[ModuleName])(objectEqualTo(Right(expected)))
      },
      test("will decode a simple ModulePath") {
        val actual   = """[["org"]]"""
        val expected = ModuleName(Path.fromString("org"))
        assert(actual.fromJson[ModuleName])(objectEqualTo(Right(expected)))
      },
      test("will decode a ModulePath") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = ModuleName(Path.fromString("org.foo.bar"))
        assert(actual.fromJson[ModuleName])(objectEqualTo(Right(expected)))
      }
    ),
    suite("PackageName")(
      test("will decode an empty PackageName") {
        val actual   = "[]"
        val expected = PackageName(Path.empty)
        assert(actual.fromJson[PackageName])(objectEqualTo(Right(expected)))
      },
      test("will decode a simple PackageName") {
        val actual   = """[["org"]]"""
        val expected = PackageName(Path.fromString("org"))
        assert(actual.fromJson[PackageName])(objectEqualTo(Right(expected)))
      },
      test("will decode a PackageName") {
        val actual   = """[["org"],["foo"],["bar"]]"""
        val expected = PackageName(Path.fromString("org.foo.bar"))
        assert(actual.fromJson[PackageName])(objectEqualTo(Right(expected)))
      }
    ),
    suite("QualifiedModuleName")(
      test("will decode an empty QualifiedModuleName") {
        val actual   = "[[],[]]"
        val expected = QualifiedModuleName.empty
        assert(actual.fromJson[QualifiedModuleName])(objectEqualTo(Right(expected)))
      },
      test("will decode a simple QualifiedModuleName") {
        val actual   = """[[["org"]],[["src","test"]]]"""
        val expected = QualifiedModuleName(PackageName.fromString("org"), ModuleName.fromString("SrcTest"))
        assert(actual.fromJson[QualifiedModuleName])(objectEqualTo(Right(expected)))
      },
      test("will decode a QualifiedModuleName") {
        val actual   = """[[["src"],["test"],["scala"]],[["src","test"]]]"""
        val expected = QualifiedModuleName(PackageName.fromString("src.test.scala"), ModuleName.fromString("SrcTest"))
        assert(actual.fromJson[QualifiedModuleName])(objectEqualTo(Right(expected)))
      }
    ),
    suite("QName")(
      test("will decode an empty QName") {
        val actual   = "[[],[]]"
        val expected = QName(Path.empty, Name.empty)
        assert(actual.fromJson[QName])(objectEqualTo(Right(expected)))
      },
      test("will decode a QName") {
        val actual   = """[[["proper"],["path"]],["name"]]"""
        val expected = QName.fromString("Proper.Path:name").get
        assert(actual.fromJson[QName])(objectEqualTo(Right(expected)))
      }
    ),
    suite("FQName")(
      test("will decode an empty FQName") {
        val actual   = "[[],[],[]]"
        val expected = FQName.empty
        assert(actual.fromJson[FQName])(objectEqualTo(Right(expected)))
      },
      test("will decode a FQName") {
        val actual   = """[[["com"],["example"]],[["java","home"]],["morphir"]]"""
        val expected = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        assert(actual.fromJson[FQName])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Documented")(
      test("will decode Documented for Integer") {
        val actual   = """{"doc":"This is an Integer 10","value":10}"""
        val expected = Documented("This is an Integer 10", 10)
        assert(actual.fromJson[Documented[Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Documented for String") {
        val actual   = """{"doc":"This is a String","value":"Hello"}"""
        val expected = Documented("This is a String", "Hello")
        assert(actual.fromJson[Documented[String]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("AccessControlled")(
      test("will decode AccessControlled for private Integer") {
        val actual   = """{"access":"Private","value":10}"""
        val expected = AccessControlled(AccessControlled.Access.Private, 10)
        assert(actual.fromJson[AccessControlled[Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode AccessControlled for public String") {
        val actual   = """{"access":"Public","value":"Hello"}"""
        val expected = AccessControlled(AccessControlled.Access.Public, "Hello")
        assert(actual.fromJson[AccessControlled[String]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Field")(
      test("will decode Field for private Integer") {
        val actual   = """{"name":["name"],"tpe":{"access":"Private","value":10}}"""
        val expected = Field(Name.fromString("Name"), AccessControlled(AccessControlled.Access.Private, 10))
        assert(actual.fromJson[Field[AccessControlled[Int]]])(objectEqualTo(Right(expected)))
      },
      test("will decode Field for public String") {
        val actual = """{"name":["string"],"tpe":{"access":"Public","value":"Hello"}}"""
        val expected =
          Field(Name.fromString("String"), AccessControlled(AccessControlled.Access.Public, "Hello"))
        assert(actual.fromJson[Field[AccessControlled[String]]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Literal")(
      test("will decode a BoolLiteral") {
        val actual   = """["BoolLiteral",true]"""
        val expected = BoolLiteral(true)
        assert(actual.fromJson[BoolLiteral])(objectEqualTo(Right(expected)))
      },
      test("will decode a CharLiteral") {
        val actual   = """["CharLiteral","x"]"""
        val expected = CharLiteral('x')
        assert(actual.fromJson[CharLiteral])(objectEqualTo(Right(expected)))
      },
      test("will decode a DecimalLiteral") {
        val actual   = """["DecimalLiteral","1.23456789"]"""
        val expected = DecimalLiteral(new java.math.BigDecimal("1.23456789"))
        assert(actual.fromJson[DecimalLiteral])(objectEqualTo(Right(expected)))
      },
      test("will decode a FloatLiteral") {
        val actual   = """["FloatLiteral",1.3232]"""
        val expected = FloatLiteral(1.3232d)

        assert(actual.fromJson[FloatLiteral])(objectEqualTo(Right(expected)))
      },
      test("will decode a StringLiteral") {
        val actual   = """["StringLiteral","hello"]"""
        val expected = StringLiteral("hello")
        assert(actual.fromJson[StringLiteral])(objectEqualTo(Right(expected)))
      },
      test("will decode an WholeNumberLiteral") {
        val actual   = """["WholeNumberLiteral",321321]"""
        val expected = WholeNumberLiteral(321321L)
        assert(actual.fromJson[WholeNumberLiteral])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Type")(
      test("will decode Type.Unit") {
        val actual   = """["Unit",1234]"""
        val expected = Type.Unit[Int](1234)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Variable") {
        val actual   = """["Variable",1234,["x"]]"""
        val expected = variable[Int](1234, "x")
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Record") {
        val var1 = Field(Name("first"), variable[Int](123, "f"))
        val var2 = Field(Name("second"), variable[Int](345, "g"))
        val actual =
          """["Record",1,[{"name":["first"],"tpe":["Variable",123,["f"]]},{"name":["second"],"tpe":["Variable",345,["g"]]}]]"""
        val expected = record(1, fields(var1, var2))
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.ExtensibleRecord") {
        val var1 = Field(Name("first"), variable[Int](123, "f"))
        val var2 = Field(Name("second"), variable[Int](345, "g"))
        val actual =
          """["ExtensibleRecord",1,["some","name"],[{"name":["first"],"tpe":["Variable",123,["f"]]},{"name":["second"],"tpe":["Variable",345,["g"]]}]]"""
        val expected = Type.ExtensibleRecord(1, Name.fromString("someName"), var1, var2)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Tuple") {
        val var1     = variable[Int](123, "f")
        val var2     = variable[Int](345, "g")
        val actual   = """["Tuple",1,[["Variable",123,["f"]],["Variable",345,["g"]]]]"""
        val expected = Type.Tuple(1, var1, var2)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Reference") {
        val var1 = variable[Int](123, "f")
        val var2 = variable[Int](345, "g")
        val actual =
          """["Reference",1,[[["test"]],[["java","home"]],["morphir"]],[["Variable",123,["f"]],["Variable",345,["g"]]]]"""
        val expected = Type.Reference(1, FQName.fromString("test:JavaHome:morphir"), var1, var2)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      },
      test("will decode Type.Function") {
        val var1 = variable[Int](123, "f")
        val var2 = variable[Int](345, "g")
        val actual =
          """["Function",1,["Variable",123,["f"]],["Variable",345,["g"]]]"""
        val expected = function(1, var1, var2)
        assertTrue(
          actual.fromJson[Type[Int]] == Right(expected)
        )
      }
    ),
    suite("Constructors")(
      test("will decode empty Constructor") {
        val actual   = """[]"""
        val expected = org.finos.morphir.ir.Type.Constructors[Int](Map.empty)
        assert(actual.fromJson[org.finos.morphir.ir.Type.Constructors[Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Constructors with one constructor") {
        val name   = Name.fromString("name")
        val actual = """[[["name"],[[["name"],["Variable",123,["f"]]]]]]"""
        val expected =
          org.finos.morphir.ir.Type.Constructors[Int](Map((name, zio.Chunk((name, variable[Int](123, "f"))))))
        assert(actual.fromJson[org.finos.morphir.ir.Type.Constructors[Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Constructors") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val actual =
          """[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]"""
        val expected = org.finos.morphir.ir.Type.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        assert(actual.fromJson[org.finos.morphir.ir.Type.Constructors[Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("TypeDefinition")(
      test("will decode TypeAlias") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
        val expected = TypeDefinition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        assertTrue(
          actual.fromJson[TypeDefinition.TypeAlias[Int]] == Right(expected),
          actual.fromJson[TypeDefinition[Int]] == Right(expected)
        )
      },
      test("will decode CustomType") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = AccessControlled(
          AccessControlled.Access.Public,
          org.finos.morphir.ir.Type.Constructors[Int](
            Map(
              (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
              (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
            )
          )
        )
        val actual =
          """["CustomTypeDefinition",[["name","1"],["name","2"]],{"access":"Public","value":[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]}]"""
        val expected = TypeDefinition.CustomType[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeDefinition.CustomType[Int]] == Right(expected),
          actual.fromJson[TypeDefinition[Int]] == Right(expected)
        )
      }
    ),
    suite("TypeSpecification")(
      test("will decode TypeAliasSpecification") {
        val name1  = Name.fromString("name1")
        val name2  = Name.fromString("name2")
        val actual = """["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
        val expected =
          TypeSpecification
            .TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
        assertTrue(
          actual.fromJson[TypeSpecification.TypeAliasSpecification[Int]] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      },
      test("will decode CustomTypeSpecification") {
        val name1 = Name.fromString("name1")
        val name2 = Name.fromString("name2")
        val name3 = Name.fromString("name3")
        val name4 = Name.fromString("name4")
        val ctors = org.finos.morphir.ir.Type.Constructors[Int](
          Map(
            (name1, zio.Chunk((name1, variable[Int](123, "f")), (name2, variable[Int](345, "g")))),
            (name2, zio.Chunk((name3, variable[Int](678, "h")), (name4, variable[Int](789, "i"))))
          )
        )
        val actual =
          """["CustomTypeSpecification",[["name","1"],["name","2"]],[[["name","1"],[[["name","1"],["Variable",123,["f"]]],[["name","2"],["Variable",345,["g"]]]]],[["name","2"],[[["name","3"],["Variable",678,["h"]]],[["name","4"],["Variable",789,["i"]]]]]]]"""
        val expected = TypeSpecification.CustomTypeSpecification[Int](zio.Chunk(name1, name2), ctors)
        assertTrue(
          actual.fromJson[TypeSpecification.CustomTypeSpecification[Int]] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      },
      test("will decode OpaqueTypeSpecification") {
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val actual   = """["OpaqueTypeSpecification",[["name","1"],["name","2"]]]"""
        val expected = TypeSpecification.OpaqueTypeSpecification(zio.Chunk(name1, name2))
        assertTrue(
          actual.fromJson[TypeSpecification.OpaqueTypeSpecification] == Right(expected),
          actual.fromJson[TypeSpecification[Int]] == Right(expected)
        )
      }
    ),
    suite("ValueDefinition")(
      test("will decode ValueDefinition") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](345, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val actual =
          """{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Unit",1]}"""
        val expected =
          ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), Value.Unit(1))
        assert(actual.fromJson[ValueDefinition[Int, Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("ValueSpecification")(
      test("will decode ValueSpecification") {
        val inputs = zio.Chunk(
          (Name.fromString("name1"), variable[Int](345, "g")),
          (Name.fromString("name2"), variable[Int](678, "h"))
        )
        val actual =
          """{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"output":["Variable",111,["f"]]}"""
        val expected = ValueSpecification[Int](inputs, variable[Int](111, "f"))
        assert(actual.fromJson[ValueSpecification[Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Pattern")(
      test("will decode AsPattern") {
        val actual   = """["AsPattern",1,["WildcardPattern",1],["wild","card"]]"""
        val expected = Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        assertTrue(
          actual.fromJson[Pattern.AsPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode ConstructorPattern") {
        val patterns: zio.Chunk[Pattern[Int]] = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.EmptyListPattern[Int](2),
          Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        )
        val actual =
          """["ConstructorPattern",1,[[["test"]],[["java","home"]],["morphir"]],[["WildcardPattern",1],["EmptyListPattern",2],["AsPattern",1,["WildcardPattern",1],["wild","card"]]]]"""
        val expected = Pattern.ConstructorPattern[Int](1, FQName.fromString("test:JavaHome:morphir"), patterns)
        assertTrue(
          actual.fromJson[Pattern.ConstructorPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode EmptyListPattern") {
        val actual   = """["EmptyListPattern",1]"""
        val expected = Pattern.EmptyListPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.EmptyListPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode LiteralPattern") {
        val actual   = """["LiteralPattern",1,["StringLiteral","hello"]]"""
        val expected = Pattern.LiteralPattern[Int](1, StringLiteral("hello"))
        assert(actual.fromJson[Pattern.LiteralPattern[Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode HeadTailPattern") {
        val actual = """["HeadTailPattern",1,["WildcardPattern",1],["EmptyListPattern",2]]"""
        val expected =
          Pattern.HeadTailPattern[Int](1, Pattern.WildcardPattern[Int](1), Pattern.EmptyListPattern[Int](2))
        assertTrue(
          actual.fromJson[Pattern.HeadTailPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode TuplePattern") {
        val patterns: zio.Chunk[Pattern[Int]] = zio.Chunk(
          Pattern.WildcardPattern[Int](1),
          Pattern.UnitPattern[Int](2),
          Pattern.AsPattern[Int](1, Pattern.WildcardPattern[Int](1), Name.fromString("wildCard"))
        )
        val actual =
          """["TuplePattern",1,[["WildcardPattern",1],["UnitPattern",2],["AsPattern",1,["WildcardPattern",1],["wild","card"]]]]"""
        val expected = Pattern.TuplePattern[Int](1, patterns)
        assertTrue(
          actual.fromJson[Pattern.TuplePattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode UnitPattern") {
        val actual   = """["UnitPattern",1]"""
        val expected = Pattern.UnitPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.UnitPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      },
      test("will decode WildcardPattern") {
        val actual   = """["WildcardPattern",1]"""
        val expected = Pattern.WildcardPattern[Int](1)
        assertTrue(
          actual.fromJson[Pattern.WildcardPattern[Int]] == Right(expected),
          actual.fromJson[Pattern[Int]] == Right(expected)
        )
      }
    ),
    suite("ModuleSpecification")(
      test("will decode ModuleSpecification") {
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

        val actual =
          """{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"output":["Variable",111,["f"]]}}]]}"""
        val expected = ModuleSpecification[Int](typeMap, valueMap)
        assert(actual.fromJson[ModuleSpecification[Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("PackageSpecification")(
      test("will decode PackageSpecification") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName.fromString("org.src")
        val modName2 = ModuleName.fromString("org.test")

        val typeMap = Map(
          name -> Documented(
            "typeDoc1",
            TypeSpecification.TypeAliasSpecification[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
          )
        )
        val inputs = zio.Chunk((name1, variable[Int](345, "g")), (name2, variable[Int](678, "h")))
        val valueMap =
          Map(name -> Documented("valueDoc1", ValueSpecification[Int](inputs, variable[Int](111, "f"))))

        val modSpec  = ModuleSpecification[Int](typeMap, valueMap)
        val expected = PackageSpecification[Int](Map(modName1 -> modSpec, modName2 -> modSpec))
        val actual =
          """{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"output":["Variable",111,["f"]]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",345,["g"]]],[["name","2"],["Variable",678,["h"]]]],"output":["Variable",111,["f"]]}}]]}]]}"""
        assert(actual.fromJson[PackageSpecification[Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("ModuleDefinition")(
      test("will decode ModuleDefinition") {
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
              org.finos.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )

        val expected = ModuleDefinition[Int, Int](typeMap, valueMap)
        val actual =
          """{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}"""
        assert(actual.fromJson[ModuleDefinition[Int, Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("PackageDefinition")(
      test("will decode PackageDefinition") {
        val name     = Name.fromString("name")
        val name1    = Name.fromString("name1")
        val name2    = Name.fromString("name2")
        val modName1 = ModuleName.fromString("org.src")
        val modName2 = ModuleName.fromString("org.test")

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
              org.finos.morphir.ir.Type.Definition.TypeAlias[Int](zio.Chunk(name1, name2), variable[Int](345, "g"))
            )
          )
        )

        val modDef = ModuleDefinition[Int, Int](typeMap, valueMap)
        val actual =
          """{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",345,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Constructor",1,[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}"""
        val expected = PackageDefinition[Int, Int](
          Map(
            modName1 -> AccessControlled(AccessControlled.Access.Public, modDef),
            modName2 -> AccessControlled(AccessControlled.Access.Public, modDef)
          )
        )

        assert(actual.fromJson[PackageDefinition[Int, Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Value")(
      test("will decode Value - ApplyCase") {
        val unitCase = Value.Unit(6)
        val actual   = """["Apply",3,["Unit",6],["Unit",6]]"""
        val expected = Value.Apply(3, unitCase, unitCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - ConstructorCase") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = """["Constructor",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        val expected = Value.Constructor(3, name)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - DestructureCase") {
        val pattern  = Pattern.WildcardPattern[Int](1)
        val unitCase = Value.Unit(6)
        val actual   = """["Destructure",3,["WildcardPattern",1],["Unit",6],["Unit",6]]"""
        val expected = Value.Destructure(3, pattern, unitCase, unitCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - FieldCase") {
        val name     = Name("Hello")
        val unitCase = Value.Unit(6)
        val actual   = """["Field",3,["Unit",6],["hello"]]"""
        val expected = Value.Field(3, unitCase, name)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - FieldFunctionCase") {
        val actual   = """["FieldFunction",3,["hello"]]"""
        val expected = Value.FieldFunction(3, Name("Hello"))
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - IfThenElseCase") {
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val unitCase          = Value.Unit(6)
        val actual            = """["IfThenElse",3,["Unit",6],["FieldFunction",3,["hello"]],["Unit",6]]"""
        val expected          = Value.IfThenElse(3, unitCase, fieldFunctionCase, unitCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - LambdaCase") {
        val pattern           = Pattern.WildcardPattern[Int](1)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val actual            = """["Lambda",3,["WildcardPattern",1],["FieldFunction",3,["hello"]]]"""
        val expected          = Value.Lambda(3, pattern, fieldFunctionCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - LetDefinitionCase") {
        val inputParams = zio.Chunk(
          (Name.fromString("name1"), 1, variable[Int](444, "g")),
          (Name.fromString("name2"), 2, variable[Int](678, "h"))
        )
        val literalCase     = Value.Literal(3, BoolLiteral(true))
        val valueDefinition = ValueDefinition[Int, Int](inputParams, variable[Int](345, "g"), literalCase)

        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))

        val actual =
          """["LetDefinition",3,["hi"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",345,["g"]],"body":["Literal",3,["BoolLiteral",true]]},["FieldFunction",3,["hello"]]]"""
        val expected =
          Value.LetDefinition(3, Name("Hi"), valueDefinition, fieldFunctionCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - LetRecursionCase") {
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
        val actual =
          """["LetRecursion",3,[[["key","1"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",333,["x"]],"body":["Literal",3,["BoolLiteral",true]]}],[["key","2"],{"inputTypes":[[["name","1"],1,["Variable",444,["g"]]],[["name","2"],2,["Variable",678,["h"]]]],"outputType":["Variable",333,["x"]],"body":["Literal",3,["BoolLiteral",true]]}]],["FieldFunction",3,["hello"]]]"""
        val expected = Value.LetRecursion(3, valueDefinitions, fieldFunctionCase)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - ListCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val actual            = """["List",3,[["Unit",6],["FieldFunction",3,["hello"]]]]"""
        val expected          = Value.List(3, zio.Chunk[Value[Int, Int]](unitCase, fieldFunctionCase))
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - LiteralCase") {
        val literal  = BoolLiteral(true)
        val actual   = """["Literal",3,["BoolLiteral",true]]"""
        val expected = Value.Literal(3, literal)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - PatternMatchCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val patterns          = zio.Chunk((Pattern.WildcardPattern[Int](12), fieldFunctionCase))
        val actual   = """["PatternMatch",3,["Unit",6],[[["WildcardPattern",12],["FieldFunction",3,["hello"]]]]]"""
        val expected = Value.PatternMatch(3, unitCase, patterns)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - RecordCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase))
        val actual            = """["Record",3,[[["hello"],["FieldFunction",3,["hello"]]],[["there"],["Unit",6]]]]"""
        val expected          = Value.Record(3, fields)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - ReferenceCase") {
        val name     = FQName.fromString("Com.Example;JavaHome;morphir", ";")
        val actual   = """["Reference",3,[[["com"],["example"]],[["java","home"]],["morphir"]]]"""
        val expected = Value.Reference(3, name)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - TupleCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val elements          = zio.Chunk(unitCase, fieldFunctionCase)
        val actual            = """["Tuple",3,[["Unit",6],["FieldFunction",3,["hello"]]]]"""
        val expected          = Value.Tuple(3, elements)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - UpdateRecordCase") {
        val unitCase          = Value.Unit(6)
        val fieldFunctionCase = Value.FieldFunction(3, Name("Hello"))
        val fields            = zio.Chunk((Name("hello"), fieldFunctionCase), (Name("there"), unitCase)).toMap
        val actual =
          """["UpdateRecord",3,["Unit",6],[[["hello"],["FieldFunction",3,["hello"]]],[["there"],["Unit",6]]]]"""
        val expected = Value.UpdateRecord(3, unitCase, fields)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - UnitCase") {
        val actual   = """["Unit",6]"""
        val expected = Value.Unit(6)
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      },
      test("will decode Value - VariableCase") {
        val actual   = """["Variable",3,["hello"]]"""
        val expected = Value.Variable(3, Name("hello"))
        assert(actual.fromJson[Value[Int, Int]])(objectEqualTo(Right(expected)))
      }
    ),
    suite("Distribution")(
      test("will decode Distribution.Library") {
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
        val expected = Library(packageName, dependencies, packageDef)
        val actual =
          """["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"output":["Unit",[]]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"output":["Unit",[]]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["Constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["Constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]"""
        assert(actual.fromJson[Library])(objectEqualTo(Right(expected))) &&
        assert(actual.fromJson[Distribution])(objectEqualTo(Right(expected)))

      },
      test("will decode Distribution.Bundle") {
        val packageName  = PackageName.fromString("morphir.SDK")
        val packageName2 = PackageName.fromString("morphir.SDK.copy")
        val name         = Name.fromString("name")
        val name1        = Name.fromString("name1")
        val name2        = Name.fromString("name2")
        val modName1     = ModuleName.fromString("org.src")
        val modName2     = ModuleName.fromString("org.test")

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
        val bundle1  = toBundle(packageName, dependencies, packageDef)
        val bundle2  = toBundle(packageName2, dependencies, packageDef)
        val expected = bundle1.insert(bundle2)
        val actual =
          """["Bundle",[[[["morphir"],["s","d","k"]],{"dependencies":[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],"packageDef":{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}}],[[["morphir"],["s","d","k"],["copy"]],{"dependencies":[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",{}]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",{}]],[["name","2"],["Unit",{}]]],"output":["Unit",{}]}}]]}]]}]],"packageDef":{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",{}]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",{}],["Unit",{}]],[["name","2"],["Unit",{}],["Unit",{}]]],"outputType":["Unit",{}],"body":["Constructor",["Unit",{}],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}}]]]"""
        assert(actual.fromJson[Bundle])(objectEqualTo(Right(expected))) &&
        assert(actual.fromJson[Distribution])(objectEqualTo(Right(expected)))
      }
    ),
    suite("MorphirIRFile")(
      test("will decode code Distribution.Library from MorphirIRFile") {
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
        val expected = MorphirIRFile(MorphirIRVersion.V3_0, Library(packageName, dependencies, packageDef))
        val actual =
          """{"formatVersion":3,"distribution":["Library",[["morphir"],["s","d","k"]],[[[["org"],["finos"],["morphir"],["ir"]],{"modules":[[[["org"],["src"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"output":["Unit",[]]}}]]}],[[["org"],["test"]],{"types":[[["name"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","1"],["name","2"]],["Unit",[]]]}]],"values":[[["name"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Unit",[]]],[["name","2"],["Unit",[]]]],"output":["Unit",[]]}}]]}]]}]],{"modules":[[[["org"],["src"]],{"access":"Public","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["Constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}],[[["org"],["test"]],{"access":"Private","value":{"types":[[["name"],{"access":"Private","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"],["name","2"]],["Unit",[]]]}}]],"values":[[["name"],{"access":"Private","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],["Unit",[]],["Unit",[]]],[["name","2"],["Unit",[]],["Unit",[]]]],"outputType":["Unit",[]],"body":["Constructor",["Unit",[]],[[["test"]],[["java","home"]],["morphir"]]]}}}]]}}]]}]}"""
        assert(actual.fromJson[MorphirIRFile])(objectEqualTo(Right(expected)))
      }
    )
  )
}
