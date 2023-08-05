package org.finos.morphir.ir

import org.finos.morphir.ir.Documented.Documented
import org.finos.morphir.ir.Name.Name
import org.finos.morphir.ir.QName.QName
import org.finos.morphir.ir.FQName.FQName
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Literal.Literal.*
import org.finos.morphir.ir.Module.{ModuleName, ModulePath}
import org.finos.morphir.ir.Path.Path
import org.finos.morphir.ir.Package.PackageName
import org.finos.morphir.ir.Module as M
import org.finos.morphir.ir.Package as P
import org.finos.morphir.ir.Type as T
import org.finos.morphir.ir.Value as V
import org.finos.morphir.testing.MorphirBaseSpec
import zio.Chunk
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
      suite("Literal Spec") {
        val sut = new LiteralWriters with AttributeTagged {}
        test("Should support writing BoolLiteral")(
          expectAllEqual(
            BoolLiteral(true)  -> """["BoolLiteral",true]""",
            BoolLiteral(false) -> """["BoolLiteral",false]"""
          )(field => sut.BoolLiteralWriter.write(StringRenderer(), field).toString)
        ) +
          test("Should support writing CharLiteral")(
            expectAllEqual(
              CharLiteral('x') -> """["CharLiteral","x"]""",
              CharLiteral(' ') -> """["CharLiteral"," "]"""
            )(field => sut.CharLiteralWriter.write(StringRenderer(), field).toString)
          ) +
          test("Should support writing DecimalLiteral")(
            expectAllEqual(
              DecimalLiteral(new java.math.BigDecimal("0.0"))        -> """["DecimalLiteral","0.0"]""",
              DecimalLiteral(new java.math.BigDecimal("1.23456789")) -> """["DecimalLiteral","1.23456789"]""",
              DecimalLiteral(new java.math.BigDecimal("-111.22222")) -> """["DecimalLiteral","-111.22222"]"""
            )(field => sut.DecimalLiteralWriter.write(StringRenderer(), field).toString)
          ) +
          test("Should support writing FloatLiteral")(
            expectAllEqual(
              FloatLiteral(0.0)     -> """["FloatLiteral",0]""",
              FloatLiteral(1.3232d) -> """["FloatLiteral",1.3232]""",
              FloatLiteral(-1.323d) -> """["FloatLiteral",-1.323]"""
            )(field => sut.FloatLiteralWriter.write(StringRenderer(), field).toString)
          ) +
          test("Should support writing StringLiteral")(
            expectAllEqual(
              StringLiteral("Hello") -> """["StringLiteral","Hello"]""",
              StringLiteral("there") -> """["StringLiteral","there"]"""
            )(field => sut.StringLiteralWriter.write(StringRenderer(), field).toString)
          ) +
          test("Should support writing StringLiteral")(
            expectAllEqual(
              WholeNumberLiteral(0)        -> """["WholeNumberLiteral",0]""",
              WholeNumberLiteral(321321L)  -> """["WholeNumberLiteral",321321]""",
              WholeNumberLiteral(-321321L) -> """["WholeNumberLiteral",-321321]"""
            )(field => sut.WholeNumberLiteralWriter.write(StringRenderer(), field).toString)
          )
      } +
      suite("Type Writers Spec") {
        val sut = new IRTypeWriters with AttributeTagged {}
        suite("FieldTypeWriter")(
          test("Should support writing Morphir IR Field")(
            expectAllEqual(
              T.Field[Int](Name("key1"), T.Type.Unit[Int](1)) -> """{"name":["key","1"],"tpe":["Unit",1]}""",
              T.Field[Int](
                Name("key2"),
                T.Type.Tuple[Int](2, List(T.Type.Variable[Int](2, Name("x")), T.Type.Variable[Int](2, Name("y"))))
              )
                -> """{"name":["key","2"],"tpe":["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]]}"""
            )(field => sut.FieldTypeWriter(sut.IntWriter).write(StringRenderer(), field).toString)
          )
        ) +
          suite("ExtensibleRecordTypeWriter")(
            test("Should support writing Morphir IR Type ExtensibleRecord")(
              expectAllEqual(
                T.Type.ExtensibleRecord[Int](
                  1,
                  Name("ExtRec"),
                  List(
                    T.Field[Int](Name("key1"), T.Type.Variable[Int](2, Name("x"))),
                    T.Field[Int](Name("key2"), T.Type.Variable[Int](2, Name("y")))
                  )
                )
                  -> """["ExtensibleRecord",1,["ext","rec"],[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]"""
              )(rec => sut.ExtensibleRecordTypeWriter(sut.IntWriter).write(StringRenderer(), rec).toString)
            )
          ) +
          suite("FunctionTypeWriter")(
            test("Should support writing Morphir IR Type Function")(
              expectAllEqual(
                T.Type.Function[Int](
                  1,
                  T.Type.Tuple[Int](2, List(T.Type.Variable[Int](2, Name("x")), T.Type.Variable[Int](2, Name("y")))),
                  T.Type.Unit[Int](3)
                )
                  -> """["Function",1,["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]],["Unit",3]]"""
              )(func => sut.FunctionTypeWriter(sut.IntWriter).write(StringRenderer(), func).toString)
            )
          ) +
          suite("RecordTypeWriter")(
            test("Should support writing Morphir IR Type Record")(
              expectAllEqual(
                T.Type.Record[Int](
                  1,
                  List(
                    T.Field[Int](Name("key1"), T.Type.Variable[Int](2, Name("x"))),
                    T.Field[Int](Name("key2"), T.Type.Variable[Int](2, Name("y")))
                  )
                )
                  -> """["Record",1,[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]"""
              )(rec => sut.RecordTypeWriter(sut.IntWriter).write(StringRenderer(), rec).toString)
            )
          ) +
          suite("ReferenceTypeWriter")(
            test("Should support writing Morphir IR Type Reference")(
              expectAllEqual(
                T.Type.Reference[Int](
                  1,
                  FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName")),
                  List(T.Type.Unit[Int](1), T.Type.Variable[Int](2, Name("y")))
                )
                  -> """["Reference",1,[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]],[["Unit",1],["Variable",2,["y"]]]]"""
              )(ref => sut.ReferenceTypeWriter(sut.IntWriter).write(StringRenderer(), ref).toString)
            )
          ) +
          suite("TupleTypeWriter")(
            test("Should support writing Morphir IR Type Tuple")(
              expectAllEqual(
                T.Type
                  .Tuple[Int](1, List(T.Type.Variable[Int](1, Name("y")))) -> """["Tuple",1,[["Variable",1,["y"]]]]""",
                T.Type.Tuple[Int](2, List(T.Type.Unit[Int](2), T.Type.Variable[Int](2, Name("y"))))
                  -> """["Tuple",2,[["Unit",2],["Variable",2,["y"]]]]"""
              )(tuple => sut.TupleTypeWriter(sut.IntWriter).write(StringRenderer(), tuple).toString)
            )
          ) +
          suite("UnitTypeWriter")(
            test("Should support writing Morphir IR Type Unit")(
              expectAllEqual(
                T.Type.Unit[Int](1) -> """["Unit",1]"""
              )(unit => sut.UnitTypeWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          ) +
          suite("VariableTypeWriter")(
            test("Should support writing Morphir IR Type Variable")(
              expectAllEqual(
                T.Type.Variable[Int](1, Name("y")) -> """["Variable",1,["y"]]"""
              )(unit => sut.VariableTypeWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          ) +
          suite("TypeWriter")(
            test("Should support writing any Morphir IR Type")(
              expectAllEqual(
                T.Type.ExtensibleRecord[Int](
                  1,
                  Name("ExtRec"),
                  List(
                    T.Field[Int](Name("key1"), T.Type.Variable[Int](2, Name("x"))),
                    T.Field[Int](Name("key2"), T.Type.Variable[Int](2, Name("y")))
                  )
                )
                  -> """["ExtensibleRecord",1,["ext","rec"],[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]""",
                T.Type.Function[Int](
                  1,
                  T.Type.Tuple[Int](2, List(T.Type.Variable[Int](2, Name("x")), T.Type.Variable[Int](2, Name("y")))),
                  T.Type.Unit[Int](3)
                )
                  -> """["Function",1,["Tuple",2,[["Variable",2,["x"]],["Variable",2,["y"]]]],["Unit",3]]""",
                T.Type.Record[Int](
                  1,
                  List(
                    T.Field[Int](Name("key1"), T.Type.Variable[Int](2, Name("x"))),
                    T.Field[Int](Name("key2"), T.Type.Variable[Int](2, Name("y")))
                  )
                )
                  -> """["Record",1,[{"name":["key","1"],"tpe":["Variable",2,["x"]]},{"name":["key","2"],"tpe":["Variable",2,["y"]]}]]""",
                T.Type.Reference[Int](
                  1,
                  FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName")),
                  List(T.Type.Unit[Int](1), T.Type.Variable[Int](2, Name("y")))
                )
                  -> """["Reference",1,[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name"]],[["Unit",1],["Variable",2,["y"]]]]""",
                T.Type
                  .Tuple[Int](1, List(T.Type.Variable[Int](1, Name("y")))) -> """["Tuple",1,[["Variable",1,["y"]]]]""",
                T.Type.Unit[Int](1)                                        -> """["Unit",1]""",
                T.Type.Variable[Int](1, Name("y"))                         -> """["Variable",1,["y"]]"""
              )(unit => sut.TypeWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          ) +
          suite("Constructors") {
            val name            = Name("name")
            val ConstructorArg1 = T.ConstructorArg(name, T.Type.Variable[Int](123, Name("f")))
            val ConstructorArg2 =
              T.ConstructorArg(
                name,
                T.Type.Tuple[Int](2, List(T.Type.Unit[Int](2), T.Type.Variable[Int](2, Name("yy"))))
              )

            test("should support Constructors") {
              expectAllEqual(
                T.Constructors[Int](Map.empty) -> """[]""",
                T.Constructors[Int](Map(name -> T.Constructor[Int](name, T.ConstructorArgs(List(ConstructorArg1)))))
                  -> """[[["name"],[[["name"],["Variable",123,["f"]]]]]]""",
                T.Constructors[Int](
                  Map(name -> T.Constructor[Int](name, T.ConstructorArgs(List(ConstructorArg1, ConstructorArg2))))
                ) ->
                  """[[["name"],[[["name"],["Variable",123,["f"]]],[["name"],["Tuple",2,[["Unit",2],["Variable",2,["yy"]]]]]]]]"""
              )(unit => sut.ConstructorsWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            }
          } +
          suite("AccessControlled")(
            test("should support AccessControlled") {
              expectAllEqual(
                AccessControlled.privateAccess(10) -> """{"access":"Private","value":10}""",
                AccessControlled.publicAccess(333) -> """{"access":"Public","value":333}"""
              )(unit => sut.AccessControlledWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            }
          ) +
          suite("TypeDefinition") {
            val name            = Name("name")
            val varG            = T.Type.Variable[Int](345, Name("g"))
            val ConstructorArg1 = T.ConstructorArg(name, varG)
            val c =
              T.Constructors[Int](Map(name -> T.Constructor[Int](name, T.ConstructorArgs(List(ConstructorArg1)))))
            val ctors = AccessControlled.publicAccess(c)

            suite("TypeAliasDefinitionWriter")(
              test("should support TypeAliasDefinition") {
                expectAllEqual(
                  T.Definition.TypeAliasDefinition[Int](Chunk(name), T.Type.Unit[Int](1))
                    -> """["TypeAliasDefinition",[["name"]],["Unit",1]]""",
                  T.Definition.TypeAliasDefinition[Int](Chunk(Name("name1"), Name("name2")), varG)
                    -> """["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
                )(unit => sut.TypeAliasDefinitionWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
              }
            ) +
              suite("TypeAliasDefinitionWriter")(
                test("should support TypeAliasDefinition") {
                  expectAllEqual(
                    T.Definition.CustomTypeDefinition[Int](Chunk(name), ctors)
                      -> """["CustomTypeDefinition",[["name"]],{"access":"Public","value":[[["name"],[[["name"],["Variable",345,["g"]]]]]]}]"""
                  )(unit => sut.CustomTypeDefinitionWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                }
              ) +
              suite("TypeDefinitionWriter")(
                test("should support any TypeDefinition") {
                  expectAllEqual(
                    T.Definition.TypeAliasDefinition[Int](Chunk(Name("name1"), Name("name2")), varG)
                      -> """["TypeAliasDefinition",[["name","1"],["name","2"]],["Variable",345,["g"]]]""",
                    T.Definition.CustomTypeDefinition[Int](Chunk(name), ctors)
                      -> """["CustomTypeDefinition",[["name"]],{"access":"Public","value":[[["name"],[[["name"],["Variable",345,["g"]]]]]]}]"""
                  )(unit => sut.TypeDefinitionWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                }
              )
          } +
          suite("TypeSpecification") {
            val name            = Name("name")
            val varG            = T.Type.Variable[Int](345, Name("g"))
            val ConstructorArg1 = T.ConstructorArg(name, varG)
            val constructors =
              T.Constructors[Int](Map(name -> T.Constructor[Int](name, T.ConstructorArgs(List(ConstructorArg1)))))
            val fqName1 = FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName1"))
            val fqName2 = FQName(PackageName("org", "finos"), ModulePath(Name("morphir"), Name("ir")), Name("FQName2"))
            val derType = T.Specification.Properties.DerivedType[Int](varG, fqName1, fqName2)

            suite("TypeAliasSpecificationWriter")(
              test("should support TypeAliasSpecification") {
                expectAllEqual(
                  T.Specification.TypeAliasSpecification[Int](List(name), T.Type.Unit[Int](1))
                    -> """["TypeAliasSpecification",[["name"]],["Unit",1]]""",
                  T.Specification.TypeAliasSpecification[Int](List(Name("name1"), Name("name2")), varG)
                    -> """["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]"""
                )(unit => sut.TypeAliasSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
              }
            ) +
              suite("CustomTypeSpecificationWriter")(
                test("should support CustomTypeSpecification") {
                  expectAllEqual(
                    T.Specification.CustomTypeSpecification[Int](List(name), constructors)
                      -> """["CustomTypeSpecification",[["name"]],[[["name"],[[["name"],["Variable",345,["g"]]]]]]]"""
                  )(unit => sut.CustomTypeSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                }
              ) +
              suite("OpaqueTypeSpecificationWriter")(
                test("should support OpaqueTypeSpecification") {
                  expectAllEqual(
                    T.Specification.OpaqueTypeSpecification(List(Name("a"), Name("b")))
                      -> """["OpaqueTypeSpecification",[["a"],["b"]]]"""
                  )(unit => sut.OpaqueTypeSpecificationWriter.write(StringRenderer(), unit).toString)
                }
              ) +
              suite("DerivedTypeSpecificationWriter")(
                test("should support DerivedTypeSpecification") {
                  expectAllEqual(
                    T.Specification.DerivedTypeSpecification[Int](List(Name("a"), Name("b")), derType)
                      -> """["DerivedTypeSpecification",[["a"],["b"]],{"baseType":["Variable",345,["g"]],"fromBaseType":[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name","1"]],"toBaseType":[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name","2"]]}]"""
                  )(unit => sut.DerivedTypeSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                }
              ) +
              suite("TypeSpecificationWriter")(
                test("should support any Type Specification") {
                  expectAllEqual(
                    T.Specification.TypeAliasSpecification[Int](List(Name("name1"), Name("name2")), varG)
                      -> """["TypeAliasSpecification",[["name","1"],["name","2"]],["Variable",345,["g"]]]""",
                    T.Specification.CustomTypeSpecification[Int](List(name), constructors)
                      -> """["CustomTypeSpecification",[["name"]],[[["name"],[[["name"],["Variable",345,["g"]]]]]]]""",
                    T.Specification.OpaqueTypeSpecification(List(Name("a"), Name("b")))
                      -> """["OpaqueTypeSpecification",[["a"],["b"]]]""",
                    T.Specification.DerivedTypeSpecification[Int](List(Name("a"), Name("b")), derType)
                      -> """["DerivedTypeSpecification",[["a"],["b"]],{"baseType":["Variable",345,["g"]],"fromBaseType":[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name","1"]],"toBaseType":[[["org"],["finos"]],[["morphir"],["ir"]],["f","q","name","2"]]}]"""
                  )(unit => sut.TypeSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                }
              )
          }
      } +
      suite("Value Writers Spec") {
        val sut                  = new IRWriters with AttributeTagged {}
        val name1                = Name("name1")
        val name2                = Name("name2")
        val fqName               = FQName.fromString("test:JavaHome:morphir")
        val varG                 = T.Type.Variable[Int](1, Name("g"))
        val varH                 = T.Type.Variable[Int](2, Name("h"))
        val valDefParam1         = V.Parameter[Int, Int](name1, 1, varG)
        val valDefParam2         = V.Parameter[Int, Int](name2, 2, varH)
        val valueDef             = V.Definition[Int, Int](Chunk(valDefParam1, valDefParam2), varG, V.Variable(1, name1))
        val valueDef2            = V.Definition[Int, Int](Chunk(valDefParam1, valDefParam2), varH, V.Variable(1, name2))
        val valSpecParam1        = V.SpecParameter[Int](name1, varG)
        val valSpecParam2        = V.SpecParameter[Int](name2, varH)
        val valueSpec            = V.Specification[Int](Chunk(valSpecParam1, valSpecParam2), varG)
        val asPattern            = V.Pattern.AsPattern[Int](1, V.Pattern.WildcardPattern[Int](1), name1)
        val emptyListPattern     = V.Pattern.EmptyListPattern[Int](1)
        val wildcardPattern      = V.Pattern.WildcardPattern[Int](2)
        val patterns             = Chunk(wildcardPattern, emptyListPattern, asPattern)
        val constructorPattern   = V.Pattern.ConstructorPattern[Int](1, fqName, patterns)
        val literalPattern       = V.Pattern.LiteralPattern[Int](1, StringLiteral("hello"))
        val headTailPattern      = V.Pattern.HeadTailPattern[Int](1, wildcardPattern, emptyListPattern)
        val tuplePattern         = V.Pattern.TuplePattern[Int](1, patterns)
        val unitPattern          = V.Pattern.UnitPattern[Int](1)
        val constructorValue     = V.Constructor(3, fqName)
        val fieldFunctionValue   = V.FieldFunction(3, Name("Hello"))
        val applyValue           = V.Apply[Int, Int](3, constructorValue, fieldFunctionValue)
        val destructureValue     = V.Destructure[Int, Int](3, wildcardPattern, constructorValue, fieldFunctionValue)
        val fieldValue           = V.Field[Int, Int](3, constructorValue, name1)
        val literalValue         = V.Literal(3, BoolLiteral(true))
        val IfThenElseValue      = V.IfThenElse(3, literalValue, fieldFunctionValue, fieldValue)
        val lambdaValue          = V.Lambda[Int, Int](3, wildcardPattern, fieldFunctionValue)
        val letDefinitionValue   = V.LetDefinition(3, Name("Hi"), valueDef, fieldFunctionValue)
        val letRecursionValue    = V.LetRecursion(3, Map(name1 -> valueDef, name2 -> valueDef2), fieldFunctionValue)
        val listValue            = V.List(3, Chunk[V.Value[Int, Int]](lambdaValue, fieldFunctionValue, literalValue))
        val patternsPatternMatch = Chunk((wildcardPattern, literalValue), (emptyListPattern, fieldFunctionValue))
        val patternMatchValue    = V.PatternMatch(3, IfThenElseValue, patternsPatternMatch)
        val recordValue          = V.Record(3, Chunk((Name("literal"), literalValue), (Name("lambda"), lambdaValue)))
        val referenceValue       = V.Reference(3, fqName)
        val tupleValue           = Value.Tuple(3, Chunk(lambdaValue, referenceValue))
        val unitValue            = V.Unit(6)
        val updateRecordValue    = V.UpdateRecord(3, recordValue, Map(Name("lambda") -> fieldFunctionValue))
        val variableValue        = V.Variable(3, Name("x"))
        val typeDef              = T.Definition.TypeAliasDefinition[Int](Chunk(name1), varG)
        val typeSpec             = T.Specification.TypeAliasSpecification[Int](List(name2), varG)
        val moduleSpec = M.Specification(
          Map(name1 -> Documented("typeDoc1", typeSpec)),
          Map(name1 -> Documented("valueDoc1", valueSpec))
        )
        val moduleDef = M.Definition(
          Map(name1 -> AccessControlled.publicAccess(Documented("typeDoc1", typeDef))),
          Map(name1 -> AccessControlled.publicAccess(Documented("valueDoc1", valueDef)))
        )
        val moduleName  = ModuleName(Path("path"), Name("name"))
        val packageSpec = P.Specification(Map(moduleName -> moduleSpec))
        val packageDef  = P.Definition(Map(moduleName -> AccessControlled.publicAccess(moduleDef)))

        suite("Specifications") {
          suite("ValueSpecificationWriter")(
            test("Should support writing Morphir IR Value Specification")(
              expectAllEqual(
                valueSpec -> """{"inputs":[[["name","1"],["Variable",1,["g"]]],[["name","2"],["Variable",2,["h"]]]],"output":["Variable",1,["g"]]}"""
              )(unit => sut.ValueSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
            )
          ) +
            suite("ModuleSpecificationWriter")(
              test("Should support writing Morphir IR Module Specification")(
                expectAllEqual(
                  moduleSpec -> """{"types":[[["name","1"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","2"]],["Variable",1,["g"]]]}]],"values":[[["name","1"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",1,["g"]]],[["name","2"],["Variable",2,["h"]]]],"output":["Variable",1,["g"]]}}]]}"""
                )(unit => sut.ModuleSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
              )
            ) +
            suite("PackageSpecificationWriter")(
              test("Should support writing Morphir IR Package Specification")(
                expectAllEqual(
                  packageSpec -> """{"modules":[[[[["path"]],["name"]],{"types":[[["name","1"],{"doc":"typeDoc1","value":["TypeAliasSpecification",[["name","2"]],["Variable",1,["g"]]]}]],"values":[[["name","1"],{"doc":"valueDoc1","value":{"inputs":[[["name","1"],["Variable",1,["g"]]],[["name","2"],["Variable",2,["h"]]]],"output":["Variable",1,["g"]]}}]]}]]}"""
                )(unit => sut.PackageSpecificationWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
              )
            )
        } +
          suite("Definitions") {
            suite("ValueDefinitionWriter")(
              test("Should support writing Morphir IR Value Definition")(
                expectAllEqual(
                  valueDef -> """{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]}"""
                )(unit =>
                  sut.ValueDefinitionWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                )
              )
            ) +
              suite("ModuleDefinitionWriter")(
                test("Should support writing Morphir IR Module Definition")(
                  expectAllEqual(
                    moduleDef -> """{"types":[[["name","1"],{"access":"Public","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"]],["Variable",1,["g"]]]}}]],"values":[[["name","1"],{"access":"Public","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]}}}]]}"""
                  )(unit =>
                    sut.ModuleDefinitionWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              ) +
              suite("PackageDefinitionWriter")(
                test("Should support writing Morphir IR Package Definition")(
                  expectAllEqual(
                    packageDef -> """{"modules":[[[[["path"]],["name"]],{"access":"Public","value":{"types":[[["name","1"],{"access":"Public","value":{"doc":"typeDoc1","value":["TypeAliasDefinition",[["name","1"]],["Variable",1,["g"]]]}}]],"values":[[["name","1"],{"access":"Public","value":{"doc":"valueDoc1","value":{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]}}}]]}}]]}"""
                  )(unit =>
                    sut.PackageDefinitionWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              )
          } +
          suite("Pattern") {
            suite("AsPatternWriter") {
              test("Should support writing Morphir IR AsPattern")(
                expectAllEqual(
                  asPattern -> """["as_pattern",1,["wildcard_pattern",1],["name","1"]]"""
                )(unit => sut.AsPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
              )
            } +
              suite("ConstructorPatternWriter") {
                test("Should support writing Morphir IR ConstructorPattern")(
                  expectAllEqual(
                    constructorPattern -> """["constructor_pattern",1,[[["test"]],[["java","home"]],["morphir"]],[["wildcard_pattern",2],["empty_list_pattern",1],["as_pattern",1,["wildcard_pattern",1],["name","1"]]]]"""
                  )(unit => sut.ConstructorPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("EmptyListPatternWriter") {
                test("Should support writing Morphir IR EmptyListPattern")(
                  expectAllEqual(
                    emptyListPattern -> """["empty_list_pattern",1]"""
                  )(unit => sut.EmptyListPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("LiteralPatternWriter") {
                test("Should support writing Morphir IR LiteralPattern")(
                  expectAllEqual(
                    literalPattern -> """["literal_pattern",1,["StringLiteral","hello"]]"""
                  )(unit => sut.LiteralPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("HeadTailPatternWriter") {
                test("Should support writing Morphir IR HeadTailPattern")(
                  expectAllEqual(
                    headTailPattern -> """["head_tail_pattern",1,["wildcard_pattern",2],["empty_list_pattern",1]]"""
                  )(unit => sut.HeadTailPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("TuplePatternWriter") {
                test("Should support writing Morphir IR TuplePattern")(
                  expectAllEqual(
                    tuplePattern -> """["tuple_pattern",1,[["wildcard_pattern",2],["empty_list_pattern",1],["as_pattern",1,["wildcard_pattern",1],["name","1"]]]]"""
                  )(unit => sut.TuplePatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("UnitPatterWriter") {
                test("Should support writing Morphir IR UnitPattern")(
                  expectAllEqual(
                    unitPattern -> """["unit_pattern",1]"""
                  )(unit => sut.UnitPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("WildcardPatternWriter") {
                test("Should support writing Morphir IR WildcardPattern")(
                  expectAllEqual(
                    wildcardPattern -> """["wildcard_pattern",2]"""
                  )(unit => sut.WildcardPatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("PatternWriter") {
                test("Should support writing any Morphir IR Pattern")(
                  expectAllEqual(
                    asPattern -> """["as_pattern",1,["wildcard_pattern",1],["name","1"]]""",
                    constructorPattern -> """["constructor_pattern",1,[[["test"]],[["java","home"]],["morphir"]],[["wildcard_pattern",2],["empty_list_pattern",1],["as_pattern",1,["wildcard_pattern",1],["name","1"]]]]""",
                    emptyListPattern -> """["empty_list_pattern",1]""",
                    literalPattern   -> """["literal_pattern",1,["StringLiteral","hello"]]""",
                    headTailPattern  -> """["head_tail_pattern",1,["wildcard_pattern",2],["empty_list_pattern",1]]""",
                    tuplePattern -> """["tuple_pattern",1,[["wildcard_pattern",2],["empty_list_pattern",1],["as_pattern",1,["wildcard_pattern",1],["name","1"]]]]""",
                    unitPattern     -> """["unit_pattern",1]""",
                    wildcardPattern -> """["wildcard_pattern",2]"""
                  )(unit => sut.PatternWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              }
          } +
          suite("Value") {
            suite("ApplyValueWriter") {
              test("Should support writing Morphir IR ApplyValue")(
                expectAllEqual(
                  V.Apply(1, V.Unit(2), V.Unit(3)) -> """["apply",1,["unit",2],["unit",3]]""",
                  applyValue -> """["apply",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["field_function",3,["hello"]]]"""
                )(unit => sut.ApplyValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
              )
            } +
              suite("ConstructorValueWriter") {
                test("Should support writing Morphir IR ConstructorValue")(
                  expectAllEqual(
                    constructorValue -> """["constructor",3,[[["test"]],[["java","home"]],["morphir"]]]"""
                  )(unit => sut.ConstructorValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("DestructureValueWriter") {
                test("Should support writing Morphir IR DestructureValue")(
                  expectAllEqual(
                    destructureValue -> """["destructure",3,["wildcard_pattern",2],["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["field_function",3,["hello"]]]"""
                  )(unit =>
                    sut.DestructureValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("FieldValueWriter") {
                test("Should support writing Morphir IR FieldValue")(
                  expectAllEqual(
                    fieldValue -> """["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]"""
                  )(unit => sut.FieldValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("FieldFunctionValueWriter") {
                test("Should support writing Morphir IR FieldFunction")(
                  expectAllEqual(
                    fieldFunctionValue -> """["field_function",3,["hello"]]"""
                  )(unit => sut.FieldFunctionValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("IfThenElseValueWriter") {
                test("Should support writing Morphir IR IfThenElseValue")(
                  expectAllEqual(
                    IfThenElseValue -> """["if_then_else",3,["literal",3,["BoolLiteral",true]],["field_function",3,["hello"]],["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]]"""
                  )(unit =>
                    sut.IfThenElseValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("LambdaValueWriter") {
                test("Should support writing Morphir IR LambdaValue")(
                  expectAllEqual(
                    lambdaValue -> """["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]"""
                  )(unit => sut.LambdaValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("LetDefinitionValueWriter") {
                test("Should support writing Morphir IR LetDefinitionValue")(
                  expectAllEqual(
                    letDefinitionValue -> """["let_definition",3,["hi"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]},["field_function",3,["hello"]]]"""
                  )(unit =>
                    sut.LetDefinitionValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("LetRecursionValueWriter") {
                test("Should support writing Morphir IR LetRecursion")(
                  expectAllEqual(
                    letRecursionValue -> """["let_recursion",3,[[["name","1"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]}],[["name","2"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",2,["h"]],"body":["variable",1,["name","2"]]}]],["field_function",3,["hello"]]]"""
                  )(unit =>
                    sut.LetRecursionValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("ListValueWriter") {
                test("Should support writing Morphir IR ListValue")(
                  expectAllEqual(
                    listValue -> """["list",3,[["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]],["field_function",3,["hello"]],["literal",3,["BoolLiteral",true]]]]"""
                  )(unit => sut.ListValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("LiteralValueWriter") {
                test("Should support writing Morphir IR LiteralValue")(
                  expectAllEqual(
                    literalValue -> """["literal",3,["BoolLiteral",true]]"""
                  )(unit => sut.LiteralValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("PatternMatchValueWriter") {
                test("Should support writing Morphir IR PatternMatchValue")(
                  expectAllEqual(
                    patternMatchValue -> """["pattern_match",3,["if_then_else",3,["literal",3,["BoolLiteral",true]],["field_function",3,["hello"]],["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]],[[["wildcard_pattern",2],["literal",3,["BoolLiteral",true]]],[["empty_list_pattern",1],["field_function",3,["hello"]]]]]"""
                  )(unit =>
                    sut.PatternMatchValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("RecordValueWriter") {
                test("Should support writing Morphir IR RecordValue")(
                  expectAllEqual(
                    recordValue -> """["record",3,[[["literal"],["literal",3,["BoolLiteral",true]]],[["lambda"],["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]]]]"""
                  )(unit => sut.RecordValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("ReferenceValueWriter") {
                test("Should support writing Morphir IR ReferenceValue")(
                  expectAllEqual(
                    referenceValue -> """["reference",3,[[["test"]],[["java","home"]],["morphir"]]]"""
                  )(unit => sut.ReferenceValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("TupleValueWriter") {
                test("Should support writing Morphir IR TupleValue")(
                  expectAllEqual(
                    tupleValue -> """["tuple",3,[["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]],["reference",3,[[["test"]],[["java","home"]],["morphir"]]]]]"""
                  )(unit => sut.TupleValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("UpdateRecordValueWriter") {
                test("Should support writing Morphir IR UpdateRecordValue")(
                  expectAllEqual(
                    updateRecordValue -> """["update_record",3,["record",3,[[["literal"],["literal",3,["BoolLiteral",true]]],[["lambda"],["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]]]],[[["lambda"],["field_function",3,["hello"]]]]]"""
                  )(unit =>
                    sut.UpdateRecordValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString
                  )
                )
              } +
              suite("UnitValueWriter") {
                test("Should support writing Morphir IR UnitValue")(
                  expectAllEqual(
                    unitValue -> """["unit",6]"""
                  )(unit => sut.UnitValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("VariableValueWriter") {
                test("Should support writing Morphir IR VariableValue")(
                  expectAllEqual(
                    variableValue -> """["variable",3,["x"]]"""
                  )(unit => sut.VariableValueWriter(sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              } +
              suite("ValueWriter") {
                test("Should support writing any Morphir IR Value")(
                  expectAllEqual(
                    applyValue -> """["apply",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["field_function",3,["hello"]]]""",
                    constructorValue -> """["constructor",3,[[["test"]],[["java","home"]],["morphir"]]]""",
                    destructureValue -> """["destructure",3,["wildcard_pattern",2],["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["field_function",3,["hello"]]]""",
                    fieldValue -> """["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]""",
                    fieldFunctionValue -> """["field_function",3,["hello"]]""",
                    IfThenElseValue -> """["if_then_else",3,["literal",3,["BoolLiteral",true]],["field_function",3,["hello"]],["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]]""",
                    lambdaValue -> """["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]""",
                    letDefinitionValue -> """["let_definition",3,["hi"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]},["field_function",3,["hello"]]]""",
                    letRecursionValue -> """["let_recursion",3,[[["name","1"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",1,["g"]],"body":["variable",1,["name","1"]]}],[["name","2"],{"inputTypes":[[["name","1"],1,["Variable",1,["g"]]],[["name","2"],2,["Variable",2,["h"]]]],"outputType":["Variable",2,["h"]],"body":["variable",1,["name","2"]]}]],["field_function",3,["hello"]]]""",
                    listValue -> """["list",3,[["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]],["field_function",3,["hello"]],["literal",3,["BoolLiteral",true]]]]""",
                    literalValue -> """["literal",3,["BoolLiteral",true]]""",
                    patternMatchValue -> """["pattern_match",3,["if_then_else",3,["literal",3,["BoolLiteral",true]],["field_function",3,["hello"]],["field",3,["constructor",3,[[["test"]],[["java","home"]],["morphir"]]],["name","1"]]],[[["wildcard_pattern",2],["literal",3,["BoolLiteral",true]]],[["empty_list_pattern",1],["field_function",3,["hello"]]]]]""",
                    recordValue -> """["record",3,[[["literal"],["literal",3,["BoolLiteral",true]]],[["lambda"],["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]]]]""",
                    referenceValue -> """["reference",3,[[["test"]],[["java","home"]],["morphir"]]]""",
                    tupleValue -> """["tuple",3,[["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]],["reference",3,[[["test"]],[["java","home"]],["morphir"]]]]]""",
                    updateRecordValue -> """["update_record",3,["record",3,[[["literal"],["literal",3,["BoolLiteral",true]]],[["lambda"],["lambda",3,["wildcard_pattern",2],["field_function",3,["hello"]]]]]],[[["lambda"],["field_function",3,["hello"]]]]]""",
                    unitValue     -> """["unit",6]""",
                    variableValue -> """["variable",3,["x"]]"""
                  )(unit => sut.ValueWriter(sut.IntWriter, sut.IntWriter).write(StringRenderer(), unit).toString)
                )
              }
          }
      }
  }
}
