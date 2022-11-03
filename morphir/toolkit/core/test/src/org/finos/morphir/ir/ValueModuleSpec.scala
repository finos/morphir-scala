package org.finos
package morphir
package ir

import zio.Chunk
import org.finos.morphir.ir.generator.LiteralGen
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Type.defineField
import org.finos.morphir.ir.Type.{Type => IrType, UType}
import org.finos.morphir.ir.Value.Pattern.LiteralPattern
import org.finos.morphir.ir.Value.{Definition => ValueDefinition, Pattern, TypedValue, _}
import Value._
import org.finos.morphir.ir.sdk.Basics.{boolType, floatType, intType}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object ValueModuleSpec extends MorphirBaseSpec {

  val boolType: UType                  = Type.reference(FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:Bool"))
  def listType(itemType: UType): UType = Type.reference(FQName.fromString("Morphir.SDK:List:List"), itemType)
  val stringType: UType                = sdk.String.stringType

  def spec = suite("Value Module")(
    applySuite,
    constructorSuite,
    // destructureSuite,
    fieldSuite,
    fieldFunctionSuite,
    lambdaSuite,
    listSuite,
    literalSuite,
    recordSuite,
    referenceSuite,
    tupleSuite,
    unitSuite,
    variableSuite,
    suite("Collect Variables should return as expected for:")(
      //   test("IfThenElse") {
      //     val ife = ifThenElse(
      //       condition = boolean(false),
      //       thenBranch = variable("y"),
      //       elseBranch = int(3)
      //     )
      //     assertTrue(ife.collectVariables == Set(Name.fromString("y")))
      //   },
      //   test("LetDefinition") {
      //     val ld = let(
      //       "y",
      //       int(intType, 42).toValDef,
      //       apply(intType --> intType, sdk.Basics.add(intType), variable("y", intType), int(intType, 42))
      //     )
      //     assertTrue(ld.collectVariables == Set(Name("y")))
      //   },
      //   test("LetRecursion") {
      //     val lr = LetRecursion.Typed(
      //       "x" -> valueDef(intType)(
      //         ifThenElse(
      //           condition = boolean(boolType, false),
      //           thenBranch = variable(intType, "y"),
      //           elseBranch = int(intType, 3)
      //         )
      //       ),
      //       "y" ->
      //         valueDef(intType)(
      //           IfThenElse
      //             .Typed(
      //               condition = literal(false) :> boolType,
      //               thenBranch = literal(2) :> intType,
      //               elseBranch = variable("z") :> intType
      //             )
      //         )
      //     )(
      //       apply(
      //         reference(intType, FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:add")),
      //         variable("x", intType),
      //         variable("y", intType)
      //       )
      //     )

      //     assertTrue(lr.collectVariables == Set(Name("x"), Name("y"), Name("z")))
      //   },
      //   test("PatternMatch") {
      //     val cases = Chunk(
      //       (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
      //       (asPattern(wildcardPattern, Name.fromString("y")), variable(Name("integer")))
      //     )

      //     val pm = patternMatch(
      //       wholeNumber(new java.math.BigInteger("42")),
      //       cases
      //     )
      //     assertTrue(pm.collectVariables == Set(Name("name"), Name("integer")))
      //   },
      //   test("UpdateRecord") {
      //     val ur = update(
      //       string("hello world"),
      //       Chunk(
      //         Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
      //         Name("fieldC") -> variable(Name("none"))
      //       )
      //     )
      //     assertTrue(ur.collectVariables == Set(Name("none")))
      //   }
    ),
    suite("Collect References should return as expected for:")(
      //   test("Destructure") {
      //     val fq = morphir.ir.FQName(
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       Name("RecordType")
      //     )
      //     val des = destructure(
      //       tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
      //       tuple(string("red"), reference(fq)),
      //       variable("x")
      //     )
      //     assertTrue(des.collectReferences == Set(fq))
      //   },
      //   test("IfThenElse") {
      //     val fqName = morphir.ir.FQName(
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       Name("RecordType")
      //     )
      //     val fqName2 = morphir.ir.FQName(
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       morphir.ir.Path(Name("Morphir.SDK")),
      //       Name("VariableType")
      //     )
      //     val ife = ifThenElse(
      //       condition = reference(fqName),
      //       thenBranch = variable("y"),
      //       elseBranch = reference(fqName2)
      //     )
      //     assertTrue(ife.collectReferences == Set(fqName, fqName2))
      //   },
      //   test("LetDefinition") {

      //     val fqName  = FQName.fromString("Morphir:SDK:valueType")
      //     val fqName2 = FQName.fromString("Morphir:SDK:typed")

      //     val ld = letDef(
      //       Name("y"),
      //       ValueDefinition()(intType)(reference(fqName)),
      //       tuple(
      //         int(42),
      //         reference(fqName2)
      //       )
      //     )
      //     assertTrue(ld.collectReferences == Set(fqName, fqName2))
      //   },
      //   test("LetRecursion") {
      //     val fqName = FQName.fromString("Zio:Morphir.Basics:constInt")
      //     val lr = LetRecursion.Typed(
      //       "x" -> valueDef(intType)(
      //         IfThenElse
      //           .Typed(
      //             condition = literal(false),
      //             thenBranch = reference(fqName, intType),
      //             elseBranch = Lit.int(42).toTypedValue
      //           )
      //       ),
      //       "y" ->
      //         valueDef(intType)(
      //           IfThenElse
      //             .Typed(
      //               condition = Lit.False.toTypedValue,
      //               thenBranch = Lit.int(2).toTypedValue,
      //               elseBranch = Lit.int(42).toTypedValue
      //             )
      //         )
      //     )(variable("y", intType))

      //     assertTrue(lr.collectReferences == Set(fqName))
      //   },
      //   test("PatternMatch") {
      //     val fq  = FQName.fromString("hello:world:star", ":")
      //     val fq2 = FQName.fromString("hello:world:mission", ":")
      //     val cases = Chunk(
      //       (asPattern(wildcardPattern, Name.fromString("x")), variable(Name("name"))),
      //       (asPattern(wildcardPattern, Name.fromString("y")), reference(fq2))
      //     )

      //     val pm = patternMatch(
      //       reference(fq),
      //       cases
      //     )
      //     assertTrue(pm.collectReferences == Set(fq, fq2))
      //   },
      //   test("UpdateRecord") {
      //     val fq = FQName.fromString("hello:world:string", ":")
      //     val ur = update(
      //       string("hello world"),
      //       Chunk(
      //         Name("fieldB") -> wholeNumber(new java.math.BigInteger("3")),
      //         Name("fieldC") -> reference(fq)
      //       )
      //     )
      //     assertTrue(ur.collectReferences == Set(fq))
      //   }
    )
    // suite("toRawValue should return as expected for:")(
    //   test("Destructure") {
    //     val lit    = Lit.string("timeout")
    //     val lit2   = Lit.string("username")
    //     val value  = literal(stringType, lit)
    //     val value2 = literal(stringType, lit2)

    //     val des: TypedValue =
    //       Destructure(
    //         stringType,
    //         Pattern.WildcardPattern(stringType),
    //         value,
    //         value2
    //       )
    //     assertTrue(
    //       des.toRawValue == destructure(
    //         wildcardPattern,
    //         string("timeout"),
    //         string("username")
    //       )
    //     )
    //   },
    //   test("IfThenElse") {
    //     val gt: TypedValue        = reference(FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:greaterThan"), intType)
    //     val x: TypedValue         = variable("x", intType)
    //     val y: TypedValue         = variable("y", intType)
    //     val condition: TypedValue = Apply.Typed(gt, x, y)

    //     val ife = IfThenElse.Typed(condition, x, y)
    //     assertTrue(ife.toRawValue == IfThenElse.Raw(condition.toRawValue, x.toRawValue, y.toRawValue))
    //   },
    //   test("LetDefinition") {
    //     val value   = Lit.False.toTypedValue
    //     val flagDef = ValueDefinition()(boolType)(value)

    //     val ld = LetDefinition.Typed(boolType, "flag", flagDef, variable("flag", boolType))
    //     assertTrue(
    //       ld.toRawValue == LetDefinition.Raw(
    //         "flag",
    //         ValueDefinition.Raw()(boolType)(value.toRawValue),
    //         variable("flag").toRawValue
    //       )
    //     )
    //   },
    //   test("LetRecursion") {

    //     val times = Reference(1, FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:multiply"))
    //     val body  = Apply(4, times, Variable(5, "x"), Variable(6, "y"))

    //     val lr = LetRecursion(
    //       0,
    //       "x" -> ValueDefinition.fromLiteral(1, Lit.int(0)),
    //       "y" -> ValueDefinition.fromLiteral(2, Lit.int(42))
    //     )(body)

    //     assertTrue(
    //       lr.toRawValue == LetRecursion.Raw(
    //         "x" -> ValueDefinition.Raw()(intType)(Lit.int(0)),
    //         "y" -> ValueDefinition.Raw()(intType)(Lit.int(42))
    //       )(Apply.Raw(times.toRawValue, Variable.Raw("x"), Variable.Raw("y")))
    //     )
    //   },
    //   test("Apply - typed with multiple arguments") {
    //     val x      = Lit.int(42).toTypedValue
    //     val y      = Lit.int(58).toTypedValue
    //     val addRef = sdk.Basics.add(x.attributes)

    //     val actual = Apply.Typed(addRef, x, y)
    //     assertTrue(actual.toRawValue == apply(apply(addRef.toRawValue, x.toRawValue), y.toRawValue))
    //   },
    //   test("PatternMatch") {
    //     val input = variable("magicNumber", intType)
    //     val yes   = string("yes") :> stringType
    //     val no    = string("no") :> stringType
    //     val n42   = Lit.int(42)

    //     val pm = caseOf(input)(
    //       LiteralPattern.Typed(n42)(intType) -> yes,
    //       wildcardPattern(yes.attributes)    -> no
    //     )
    //     assertTrue(
    //       pm.toRawValue == PatternMatch.Raw(
    //         input.toRawValue,
    //         LiteralPattern.Raw(n42) -> yes.toRawValue,
    //         Pattern.wildcardPattern -> no.toRawValue
    //       )
    //     )
    //   },
    //   test("UpdateRecord") {

    //     val recordType = Type.record(defineField("greeting", stringType))
    //     val greeter    = variable(recordType, "greeter")
    //     val actual     = UpdateRecord.Typed(recordType, greeter, ("greeting", string("world") :> stringType))

    //     assertTrue(
    //       actual.toRawValue == UpdateRecord.Raw(Variable.Raw("greeter"), "greeting" -> string("world"))
    //     )
    //   }
  )

  def applySuite = suite("Apply")(
    test("ToString should return as expected") {
      val x = variable("x", intType)
      val y = variable("y", intType)
      val z = variable("z", intType)

      val addRef      = sdk.Basics.add(x.attributes)
      val subtractRef = sdk.Basics.subtract(x.attributes)

      val add      = Apply.Typed(addRef, x, y)
      val subtract = Apply.Typed(subtractRef, add, z)

      assertTrue(subtract.toString == "Morphir.SDK.Basics.subtract Morphir.SDK.Basics.add x y z")
    },
    test("Should support collecting nested variables") {
      val ff     = fieldFunction("age")
      val rec    = record("age" -> variable("myAge"), "firstName" -> string("John"))
      val actual = apply(ff, rec)
      assertTrue(
        actual.collectVariables == Set(Name("myAge")),
        actual.toString == ".age {age = myAge, firstName = \"John\"}"
      )
    },
    test("Collect references should return as expected") {
      val name  = FQName.fromString("hello:world", ":")
      val name2 = Name.fromString("wonderful")
      val ff    = reference(name)
      val str   = string("string2")
      val rec   = record(Map((name2, str)))

      assertTrue(
        apply(ff, rec).collectReferences == Set(name)
      )
    },
    test("toRawValue should return as expected") {
      val function = reference("Test:Test:square", floatType)
      val in       = Apply.Typed(function, toTypedValue(Lit.float(2.0f)))

      assertTrue(in.toRawValue == Apply.Raw(function.toRawValue, toRawValue(Lit.float(2.0f))))
    }
  )

  def constructorSuite = suite("Constructor")(
    test("toString should return as expected") {
      val fqName = morphir.ir.FQName(
        morphir.ir.Path.fromString("Morphir.SDK"),
        morphir.ir.Path.fromString("My.Models"),
        Name("Transaction")
      )
      val constr = constructor(fqName)
      assertTrue(
        constr.toString == fqName.toReferenceName,
        constr.toString == "Morphir.SDK.My.Models.Transaction"
      )
    },
    test("Should support collecting nested variables") {
      val fqName = morphir.ir.FQName(
        morphir.ir.Path("Morphir.SDK"),
        morphir.ir.Path("Morphir.SDK"),
        Name("RecordType")
      )
      val constr = constructor(fqName)
      assertTrue(constr.collectVariables == Set.empty[Name])
    },
    test("Collect references should return as expected") {
      val fqName = morphir.ir.FQName(
        morphir.ir.Path("Morphir.SDK"),
        morphir.ir.Path("Morphir.SDK"),
        Name("RecordType")
      )
      val constr = constructor(fqName)
      assertTrue(constr.collectReferences == Set.empty[FQName])
    },
    test("Should support toRawValue") {
      val fqName = morphir.ir.FQName(
        morphir.ir.Path(Name("Morphir.SDK")),
        morphir.ir.Path(Name("Morphir.SDK")),
        Name("RecordType")
      )
      val typeRef = Type.reference(fqName)
      val constr  = constructor(fqName, typeRef)
      assertTrue(constr.toRawValue == constructor(fqName))
    }
  )

  def destructureSuite = suite("Destructure")(
    test("Should support collecting nested variables") {
      val des = destructure(
        tuplePattern(asPattern(wildcardPattern, Name("name1")), asPattern(wildcardPattern, Name("name2"))),
        tuple(string("red"), string("blue")),
        variable("x")
      )
      assertTrue(des.collectVariables == Set(Name("x")))
    }
  )

  def fieldSuite = suite("Field")(
    test("toString should return as expected") {
      val person        = variable("person")
      val ageField      = field(person, "age")
      val lastNameField = field(person, "lastName")
      assertTrue(ageField.toString == "person.age", lastNameField.toString() == "person.lastName")
    },
    test("Should support collecting nested variables") {
      assertTrue(
        field(variable("person"), "age").collectVariables == Set[Name](Name("person")),
        field(reference("Package:Module:People"), "count").collectVariables == Set.empty[Name]
      )
    },
    test("Should support collecting references") {
      val name = Name.fromString("Name")
      val fi   = field(string("String"), name)

      val fqName = morphir.ir.FQName(
        morphir.ir.Path("Morphir.SDK"),
        morphir.ir.Path("Morphir.SDK"),
        Name("RecordType")
      )
      val name2 = Name.fromString("Name3")
      val fi2   = field(reference(fqName), name2)

      assertTrue(
        fi.collectReferences == Set[FQName]() &&
          fi2.collectReferences == Set(fqName)
      )
    },
    test("toRawValue should return as expected") {
      val name  = Name.fromString("Name")
      val value = literal(42)

      val actual = Field.Typed(intType, value, name)

      assertTrue(
        actual.toRawValue == field(int(42), name)
      )
    }
  )

  def fieldFunctionSuite = suite("FieldFunction")(
    test("toString should return the expected string") {
      val sut = fieldFunction("price")
      assertTrue(sut.toString == ".price")
    },
    test("Should not have any nested variables when collected") {
      val ff = fieldFunction("name")
      assertTrue(ff.collectVariables == Set.empty[Name])
    },
    test("Should not have any nested references when collected") {
      val name = Name.fromString("Name")
      val ff   = fieldFunction(name)
      assertTrue(ff.collectReferences == Set[FQName]())
    },
    test("Should discard attributes when calling toRawValue") {
      val age = Name.fromString("age")
      val ff  = fieldFunction(age, intType)

      assertTrue(ff.toRawValue == fieldFunction(age))
    }
  )

  def lambdaSuite = suite("Lambda")(
    test("toString should return the expected value") {
      val sut = lambda(asPattern(wildcardPattern, Name.fromString("x")), variable("x"))
      assertTrue(sut.toString == "(\\x -> x)")
    },
    test("It should support collecting nested variables") {
      val v1 = variable("x")

      val lam1 = lambda(
        asPattern(wildcardPattern, Name("x")),
        list(v1, variable("y"))
      )
      val lam2 = lambda(
        asPattern(wildcardPattern, Name("x")),
        v1
      )
      assertTrue(
        lam1.collectVariables == Set[Name](Name("X"), Name("y")),
        lam2.collectVariables == Set(Name("x"))
      )
    },
    test("Should support collecting nested references") {
      val fqName = morphir.ir.FQName(
        morphir.ir.Path(Name("Morphir.SDK")),
        morphir.ir.Path(Name("Morphir.SDK")),
        Name("RecordType")
      )

      val lam1 = lambda(
        asPattern(wildcardPattern, Name("x")),
        reference(fqName)
      )
      val lam2 = lambda(
        asPattern(wildcardPattern, Name("x")),
        variable("x")
      )
      assertTrue(
        lam1.collectReferences == Set(fqName) &&
          lam2.collectReferences == Set[FQName]()
      )
    },
    test("toRawValue should return the expected value") {

      val actual = Lambda.Typed(
        Type.reference("Morphir.SDK", "Basics", "power"),
        asPattern(intType, wildcardPattern(intType), Name.fromString("x")),
        variable(Name.fromString("x"), intType)
      )

      assertTrue(
        actual.toRawValue == Lambda.Raw(
          Pattern.AsPattern((), wildcardPattern, Name.fromString("x")),
          variable(Name.fromString("x"))
        )
      )
    }
  )

  def listSuite = suite("List")(
    test("toString should return as expected") {
      val sut = list(string("red"), string("blue"))
      assertTrue(sut.toString == "[\"red\", \"blue\"]")
    },
    test("Should support collecting nested variables") {
      val list1 = listOf(string("hello"), string("world"))(stringType)
      val list2 = list(
        Chunk(
          variable(Name("hello")),
          int(3)
        )
      )
      assertTrue(
        list1.collectVariables == Set[Name]() &&
          list2.collectVariables == Set(Name("hello"))
      )
    },
    test("Should support collecting references") {
      val list1 = listOf(stringType, literal("hello"), literal("world"))
      val fq    = FQName.fromString("hello:world:star")
      val list2 = list(
        Chunk(
          reference(fq),
          int(3)
        )
      )
      assertTrue(
        list1.collectReferences == Set[FQName]() &&
          list2.collectReferences == Set(fq)
      )
    },
    test("Should support toRawValue") {

      val l1 = listOf(boolType, Lit.True, Lit.False)
      assertTrue(
        l1.toRawValue == list(boolean(true).toRawValue, boolean(false).toRawValue)
      )
    }
  )

  def literalSuite = suite("Literal")(
    test("toString should produce the expected string") {
      check(LiteralGen.literal) { lit =>
        val sut = literal(lit)
        assertTrue(sut.toString == lit.toString)
      }
    },
    test("Should support collecting variables") {
      check(LiteralGen.literal) { lit =>
        val sut = literal(lit)
        assertTrue(sut.collectVariables == Set.empty[Name])
      }
    },
    test("Should support collecting references") {
      check(LiteralGen.literal) { lit =>
        val sut = literal(lit)
        assertTrue(sut.collectReferences == Set.empty[FQName])
      }
    },
    test("Should discard attributes when calling toRawValue") {
      check(LiteralGen.literal) { lit =>
        val sut = lit.toTypedValue
        assertTrue(sut.toRawValue == literal(lit))
      }
    }
  )

  def recordSuite = suite("Record")(
    test("toString should return as expected") {
      val sut = record.withFields("age" -> int(42), "name" -> string("John"))
      assertTrue(sut.toString == "{age = 42, name = \"John\"}")
    },
    test("It should support collecting nested variables") {
      val someVar = variable("someVar")

      val rec = record("fieldA" -> string("string1"), "fieldB" -> someVar)
      assertTrue(rec.collectVariables == Set(Name.fromString("someVar")))
    },
    test("It should support collecting references") {
      val name   = Name.fromString("hello")
      val name2  = Name.fromString("world")
      val fqName = FQName.fromString("folder:location:name", ":")
      val str    = string("string1")
      val rf     = reference(fqName)

      val rec = record.withNamedFields(Chunk((name, str), (name2, rf)))
      assertTrue(rec.collectReferences == Set(fqName))
    },
    test("toRawValue should return as expected") {
      val recordType = Type.record(defineField("timeout", intType))
      val rec        = Record(recordType, fields("timeout" -> int(30000)))

      assertTrue(rec.toRawValue == Record((), fields("timeout" -> int(30000))))
    }
  )

  def referenceSuite = suite("Reference")(
    test("toString should return the expected string") {
      val fqn = FQName.fqn("Morphir.SDK", "String", "toUpperCase")
      val ref = reference(fqn)
      assertTrue(ref.toString == "Morphir.SDK.String.toUpperCase")
    },
    test("Should support collecting references") {
      val fq = morphir.ir.FQName(
        morphir.ir.Path(Name("Morphir.SDK")),
        morphir.ir.Path(Name("Morphir.SDK")),
        Name("RecordType")
      )
      val ref = reference(fq)
      assertTrue(ref.collectReferences == Set(fq))
    },
    test("Collect variables should be empty") {
      val ref = reference(
        morphir.ir.FQName(
          morphir.ir.Path(Name("Morphir.SDK")),
          morphir.ir.Path(Name("Morphir.SDK")),
          Name("RecordType")
        )
      )
      assertTrue(ref.collectVariables == Set[Name]())
    },
    test("toRawValue should discard attributes") {
      val intTypeName = FQName.fromString("Morphir.SDK:Morphir.SDK.Basics:Int")
      val ref         = Reference(morphir.ir.sdk.Basics.intType, intTypeName)
      assertTrue(ref.toRawValue == Reference.Raw(intTypeName))
    }
  )

  def tupleSuite = suite("Tuple")(
    test("toString should return the expected string") {
      val sut = tuple(string("red"), string("blue"))
      assertTrue(sut.toString == "(\"red\", \"blue\")")
    },
    test("Should support collecting nested variables") {
      val tuple1 = Tuple.Typed(Lit.string("hello").toTypedValue, Lit.string("world").toTypedValue)
      val tuple2 = tuple(
        Chunk(
          variable(Name("hello")),
          int(3),
          variable(Name.fromString("other"))
        )
      )
      assertTrue(
        tuple1.collectVariables == Set[Name](),
        tuple2.collectVariables == Set(Name("hello"), Name.fromString("other"))
      )
    },
    test("Should support collecting nested references") {
      val tuple1 = Tuple.Typed(
        Chunk(
          literal("hello"),
          literal("world")
        )
      )
      val fq = FQName.fromString("hello:world:star", ":")
      val tuple2 = tuple(
        Chunk(
          reference(fq),
          int(3)
        )
      )
      assertTrue(
        tuple1.collectReferences == Set[FQName]() &&
          tuple2.collectReferences == Set(fq)
      )
    },
    test("toRawValue should discard attributes") {

      val t1 = tuple(string("shimmy") -> stringType)
      assertTrue(
        t1.toRawValue == Tuple.Raw(string("shimmy"))
      )
    }
  )

  def unitSuite = suite("Unit")(
    test("toString should return the expected string") {
      assertTrue(unit(Type.unit).toString == "()")
    },
    test("Collect variables should be empty") {
      val actual = unit
      assertTrue(actual.collectVariables == Set[Name]())
    },
    test("Collect references should be empty") {
      val actual = unit
      assertTrue(actual.collectReferences == Set[FQName]())
    },
    test("toRawValue should return as expected") {
      val actual = unit(Type.unit)
      assertTrue(actual.toRawValue == Unit(()))
    }
  )

  def variableSuite = suite("Variable")(
    test("toString should return the expected string") {
      assertTrue(variable("someVariable").toString == "someVariable")
    },
    test("Should return a single item set containing the variable name when collecting variables") {
      val name = Name("slimShady")
      assertTrue(variable(name).collectVariables == Set(name))
    },
    test("Should support collecting references") {
      assertTrue(variable(Name("name")).collectReferences == Set.empty[FQName])
    },
    test("Should support toRawValue") {
      val name  = Name("somVar")
      val value = variable(stringType, name)
      assertTrue(value.toRawValue == variable(name))
    }
  )
}
