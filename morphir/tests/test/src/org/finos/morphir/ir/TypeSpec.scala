package org.finos.morphir
package ir

import zio.Chunk
import org.finos.morphir.naming._
import org.finos.morphir.syntax.NamingSyntax
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
import org.finos.morphir.ir.Type.*
import org.finos.morphir.ir.Type.Type.{ExtensibleRecord, Function, Record, Reference, Tuple, Unit, Variable}

import scala.annotation.nowarn

object TypeSpec extends MorphirBaseSpec with NamingSyntax {
  def spec = suite("Type Spec")(
    constructorsSuite,
    extensibleRecordSuite,
    fieldSuite,
    functionSuite,
    operationsSuite,
    recordSuite,
    referenceSuite,
    sizeSuite,
    tupleSuite,
    unitSuite,
    variableSuite
  )

  def constructorsSuite = suite("Constructors")(
    test("Can make type constructors for an enum") {
      val actual        = Constructors.forEnum("Red", "Yellow", "Green")
      val expectedNames = Set("Red", "Yellow", "Green").map(Name.fromString)
      assertTrue(
        actual.ctorNames == expectedNames,
        actual.toMap.values.forall(_.isEmpty)
      )
    }
  )

  def extensibleRecordSuite = suite("ExtensibleRecord")(
    suite("Misc")(
      test("When calling foldLeft it should work as expected") {
        val fieldA = field("fieldA", variable("A"))
        val fieldB = field("fieldB", variable("B"))
        val fieldC = field("fieldC", variable("C"))
        val sut    = extensibleRecord("RecordWithThreeFields", List(fieldA, fieldB, fieldC))
        val result = sut.foldLeft(0) { case (acc, _) =>
          acc + 1
        }
        assertTrue(result == 4)
      },
      test("testing first extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tupleVar(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecord(n1, List(f1, f2, f3))
        assertTrue(actual == ExtensibleRecord((), n1, f1, f2, f3))
      },
      test("testing second extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tupleVar(variable("v3"), variable("v4")))
        val n1     = Name("SomeName")
        val actual = extensibleRecordWithFields(n1, f1, f2, f3)
        assertTrue(actual == ExtensibleRecord((), n1, f1, f2, f3))
      },
      test("testing third extensible record constructor") {

        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tupleVar(variable("v3"), variable("v4")))
        val actual = extensibleRecord("SomeName", List(f1, f2, f3))
        assertTrue(
          actual == ExtensibleRecord((), "SomeName", List(f1, f2, f3)),
          actual.toString() == "{ someName | first : hello, second : there, third : (v3, v4) }"
        )
      },
      test("testing fourth extensible record constructor") {
        val f1     = field("first", variable("hello"))
        val f2     = field("second", variable("there"))
        val f3     = field("third", tupleVar(variable("v3"), variable("v4")))
        val actual = extensibleRecordWithFields("SomeName", f1, f2, f3)
        assertTrue(
          actual.exists { case ExtensibleRecord(_, name, fields) =>
            name.toString == "[some,name]" && fields.contains(f1) && fields.contains(f2) && fields.contains(f3)
          }
        )
      }
    ),
    suite("Without Attributes")(
      test("When constructing using the constructor accepting a Name and Chunk") {
        val firstField  = field("first", variable("hello"))
        val secondField = field("second", variable("world"))
        val tupleField  = field("tupleField", tupleVar(variable("v3"), variable("v4")))
        val recordName  = Name.fromString("MyRecord")
        val sut         = extensibleRecord(recordName, List(firstField, secondField, tupleField))
        assertTrue(sut.toString == "{ myRecord | first : hello, second : world, tupleField : (v3, v4) }")
      }
    )
  )

  def fieldSuite = suite("Field")(
    test("testing first field constructor") {
      val actual = field(Name("field1"), variable("FizzBuzz"))
      assertTrue(
        actual.name == Name("field1"),
        actual.fieldType == Variable((), "FizzBuzz"),
        actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
      )
    },
    test("testing second field constructor") {
      val actual = field("field1", variable("FizzBuzz"))
      assertTrue(
        actual.name == Name("field1"),
        actual.fieldType == Variable("FizzBuzz"),
        actual.fieldType.collectVariables == Set(Name.fromString("FizzBuzz"))
      )
    }
  )

  def functionSuite = suite("Function")(
    suite("Misc")(
      test("When calling foldLeft it should work as expected") {
        val inputParam = variable("input")
        val outputType = variable("output")
        val sut        = function(inputParam, outputType)
        val result = sut.foldLeft(0) { case (acc, _) =>
          acc + 1
        }
        assertTrue(result == 3)
      },
      test("testing simple non-curried function") {
        val param      = variable("Input")
        val returnType = variable("Output")
        val actual     = function(param, returnType)
        assertTrue(
          actual == Function((), param, returnType),
          actual.toString() == "input -> output"
        )
      },
      test("testing function with function argument") {
        val actual = function(function(variable("A"), variable("B")), variable("C"))
        assertTrue(
          actual == Function((), Function((), Variable((), "A"), Variable((), "B")), Variable((), "C")),
          actual.toString() == "(a -> b) -> c"
        )
      },
      test("testing function constructor(1)") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tupleVar(variable("v3"), variable("v4"))
        val actual  = function(param1, function(param2, retType))
        assertTrue(
          actual == Function((), param1, Function((), param2, retType)),
          actual.toString() == "v1 -> v2 -> (v3, v4)"
        )
      },
      test("testing function constructor(2)") {
        val param1  = variable("v1")
        val param2  = variable("v2")
        val retType = tupleVar(variable("v3"), variable("v4"))
        val actual  = function(param1, function(param2, retType))
        assertTrue(
          actual == Function((), param1, Function((), param2, retType))
        )
      }
    ),
    suite("Without Attributes")(
      test("testing simple non-curried function") {
        val param      = variable("Input")
        val returnType = variable("Output")
        val sut        = function(param, returnType)
        assertTrue(
          sut == Function((), param, returnType),
          sut.toString == "input -> output"
        )
      }
    ),
    suite("With Attributes")()
  )

  def operationsSuite = suite("Operations")(
    test("Can be documented") {
      val actual = variable("a") ?? "Some type variable"
      assertTrue(actual.doc == "Some type variable")
    },
    test("Calling transformReferenceName should remap the name of a reference type using the provided function") {
      val sut = record(
        "name"  -> reference(fqn("Morphir.SDK", "String", "String")),
        "age"   -> reference(fqn("Morphir.SDK", "Int", "Int")),
        "items" -> reference(fqn("Morphir.SDK", "List", "List"), reference(fqn("Morphir.SDK", "String", "String")))
      )
      val actual = sut.transformReferenceName { case FQName(_, _, localName) =>
        FQName(PackageName.fromString("Acme.SDK"), ModuleName.fromString("Basics"), localName)
      }
      assertTrue(
        sut.collectReferences == Set(
          fqn("Morphir.SDK", "String", "String"),
          fqn("Morphir.SDK", "Int", "Int"),
          fqn("Morphir.SDK", "List", "List")
        ),
        actual.collectReferences == Set(
          fqn("Acme.SDK", "Basics", "String"),
          fqn("Acme.SDK", "Basics", "Int"),
          fqn("Acme.SDK", "Basics", "List")
        )
      )
    }
  )

  def recordSuite = suite("Record")(
    suite("Misc")(
      test("When calling foldLeft it should work as expected") {
        val fieldA = field("fieldA", variable("A"))
        val fieldB = field("fieldB", variable("B"))
        val fieldC = field("fieldC", variable("C"))
        val sut    = record(fieldA, fieldB, fieldC)
        val result = sut.foldLeft(0) { case (acc, _) =>
          acc + 1
        }
        assertTrue(result == 4)
      }
    ),
    suite("Without Attributes")(
      test("When constructing using the constructor accepting a List of fields") {
        val firstField  = field("first", variable("hello"))
        val secondField = field("second", variable("world"))
        val tupleField  = field("tupleField", tupleVar(variable("v3"), variable("v4")))
        val sut         = record(List(firstField, secondField, tupleField))
        assertTrue(sut.toString == "{ first : hello, second : world, tupleField : (v3, v4) }", sut.size == 6)
      },
      test("testing unattributed record constructor given a list of fields") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val chunk  = List(var1, var2)
        val actual = record(chunk)
        assertTrue(
          actual.fieldCount == 2,
          actual == Record((), var1, var2),
          actual.toString() == "{ first : hello, second : there }"
        )
      },
      test("testing unattributed record constructor given a variadic list of fields") {
        val var1   = field("first", variable("hello"))
        val var2   = field("second", variable("there"))
        val actual = record(var1, var2)
        assertTrue(
          actual == Record((), var1, var2),
          actual.toString() == "{ first : hello, second : there }"
        )
      },
      test("testing unattributed record constructor given tuples representing fields") {
        val nameField   = ("name", reference("Morphir.SDK:Morphir.SDK.Basics:String"))
        val ageField    = ("age", reference("Morphir.SDK:Morphir.SDK.Basics:Int"))
        val salaryField = ("salary", reference("Morphir.SDK:Morphir.SDK.Basics:Double"))
        val actual      = record(nameField, ageField, salaryField)
        assertTrue(
          actual.attributes == (),
          actual == Record((), field(nameField), field(ageField), field(salaryField)),
          actual.fieldCount == 3,
          actual.toString() == "{ name : Morphir.SDK.Morphir.SDK.Basics.String, age : Morphir.SDK.Morphir.SDK.Basics.Int, salary : Morphir.SDK.Morphir.SDK.Basics.Double }"
        )
      }
    )
  )

  @nowarn
  def referenceSuite = suite("Reference")(
    suite("Misc")(
      test("When calling foldLeft it should work as expected") {
        val varA = variable("a")
        val varB = variable("b")
        val varC = variable("c")
        val sut  = reference("Morphir.Sdk.Bool", varA, varB, varC)
        val result = sut.foldLeft(0) { case (acc, _) =>
          acc + 1
        }
        assertTrue(result == 4)
      }
    ),
    suite("Without Attributes")(
      test("testing construction given a FQName and List of types") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tupleVar(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference(fqn1, List(v1, v2, v3))
        assertTrue(
          actual == Reference(
            (),
            fqn1,
            Variable((), "v1"),
            Variable((), "v2"),
            Tuple((), Variable((), "v3"), Variable((), "v4"))
          ),
          actual.exists { case Reference(attributes, fqName, typeParams) =>
            attributes == () && fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams
              .contains(v3)
          },
          actual.attributes == (),
          actual.toString() == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
        )
      },
      test("testing construction given an FQName and a variadic list of types") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tupleVar(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference(fqn1, v1, v2, v3)
        assertTrue(
          actual.exists { case Reference(_, fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          }
        )
      },
      test("testing construction given packageName, moduleName, localName and a List of Types") {
        val v1     = variable("v1")
        val v2     = variable("v2")
        val v3     = tupleVar(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("packageName", "moduleName", "localName")
        val actual = reference("packageName", "moduleName", "localName", List(v1, v2, v3))
        assertTrue(
          actual.exists { case Reference(_, fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          }
        )
      },
      test("testing given packageName, moduleName, localName and a variadic list of Types") {
        val v1     = variable("V1")
        val v2     = variable("V2")
        val v3     = tupleVar(variable("v3"), variable("v4"))
        val fqn1   = FQName.fqn("PackageName", "ModuleName", "LocalName")
        val actual = reference("PackageName", "ModuleName", "LocalName", v1, v2, v3)
        assertTrue(
          actual.exists { case Reference(_, fqName, typeParams) =>
            fqName == fqn1 && typeParams.contains(v1) && typeParams.contains(v2) && typeParams.contains(v3)
          },
          actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
        )
      }
    ),
    suite("With Attributes")(
      test("testing construction given attributes, FQName and Chunk no type parameters") {
        val refName: FQName = pkg("packageName") % "moduleName" % "localName"
        val actual          = reference(Source.Location.default, refName)
        assertTrue(
          actual.attributes == Source.Location.default,
          actual.collectReferences == Set(refName),
          actual == Reference(Source.Location.default, refName)
        )
      },
      test("testing construction given attributes, FQName, and a list of types") {
        val v1 = variable(Source.Location.default, "V1")
        val v2 = variable(Source.Location.default.offsetRowBy(1), "V2")
        val v3 = tupleWithAttr(
          Source.Location.default.offsetRowBy(4),
          variable(Source.Location.default.offsetRowBy(2), "v3"),
          variable(Source.Location.default.offsetRowBy(3), "v4")
        )
        val refName = FQName.fqn("PackageName", "ModuleName", "LocalName")
        val actual  = reference(Source.Location.default.offsetRowBy(6), refName, List(v1, v2, v3))
        assertTrue(
          actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)",
          actual.attributes == Source.Location.default.offsetRowBy(6),
          actual.collectReferences == Set(refName),
          actual == Reference(Source.Location.default.offsetRowBy(6), refName, v1, v2, v3)
        )
      },
      test("testing given FQName and a variadic list of Types") {
        val v1     = variable(1, "V1")
        val v2     = variable(2, "V2")
        val v3     = tupleWithAttr(3, variable(3, "v3"), variable(4, "v4"))
        val fqn    = FQName.fqn("PackageName", "ModuleName", "LocalName")
        val actual = reference(5, fqn, v1, v2, v3)
        assertTrue(
          actual.attributes == 5,
          actual.collectReferences == Set(fqn),
          actual == Reference(5, fqn, v1, v2, v3),
          actual.toString == "PackageName.ModuleName.LocalName v1 v2 (v3, v4)"
        )
      }
    )
  )

  def sizeSuite = suite("size")(
    test("size of Unit") {
      val sut    = unit
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Variable") {
      val sut    = variable("x")
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of simple Reference") {
      val sut    = reference("x")
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Reference with a single typeParam") {
      val sut    = Reference[Any]((), FQName.fromString("x"), List(Type.Variable[Any]((), Name.fromString("y"))))
      val actual = sut.size
      assertTrue(actual == 2)
    }
  )

  @nowarn def tupleSuite = suite("Tuple")(
    test("testing emptyTuple constructor") {
      val sut = emptyTuple("Attributes")
      assertTrue(
        sut.size == 1,
        sut.toString == "()",
        sut.attributes == "Attributes",
        sut == Tuple("Attributes", Nil)
      )
    },
    test("testing tuple constructor when given a Chunk") {
      val helloVar = variable("hello")
      val worldVar = variable("world")
      val elements = List(helloVar, worldVar)
      val actual   = tuple(elements)
      assertTrue(
        actual.size == 3,
        actual.toString == "(hello, world)",
        actual.attributes == (),
        actual == Tuple((), elements)
      )
    },
    test("testing tuple constructor when given multiple un-attributed elements") {
      val var1   = variable("one")
      val var2   = variable("two")
      val var3   = variable("three")
      val actual = tupleVar(var1, var2, var3)
      assertTrue(
        actual.size == 4,
        actual.toString == "(one, two, three)",
        actual.attributes == (),
        actual == Tuple((), List(var1, var2, var3))
      )
    },
    test("testing tuple with attributes constructor") {
      val varA   = variable("A", "a")
      val varB   = variable("B", "b")
      val varC   = variable("C", "c")
      val actual = tupleWithAttr("(a, b, c)", varA, varB, varC)
      assertTrue(
        actual.size == 4,
        actual.toString == "(a, b, c)",
        actual.attributes == "(a, b, c)",
        actual == Tuple("(a, b, c)", List(varA, varB, varC))
      )
    },
    test("When calling foldLeft it should work as expected") {
      val varA = variable("a")
      val varB = variable("b")
      val varC = variable("c")
      val sut  = tupleVar(varA, varB, varC)
      val result = sut.foldLeft(0) { case (acc, _) =>
        acc + 1
      }
      assertTrue(result == 4)
    },
    test("When calling foldLeft with an empty tuple it should work as expected") {
      val sut = emptyTuple
      val result = sut.foldLeft(0) { case (acc, _) =>
        acc + 1
      }
      assertTrue(result == 1)
    },
    test("When calling foldLeft with a nested empty tuple it should work as expected") {
      val sut = tupleVar(emptyTuple)
      val result = sut.foldLeft(0) { case (acc, _) =>
        acc + 1
      }
      assertTrue(result == 2)
    },
    test("testing emptyTuple constructor") {
      val actual = emptyTuple("FizzBuzz")
      assertTrue(
        actual.exists { case Tuple(attributes, fields) => fields.isEmpty && attributes == "FizzBuzz" },
        actual.attributes == "FizzBuzz",
        actual == Tuple("FizzBuzz"),
        actual.toString() == "()"
      )
    },
    test("testing tuple constructor when given a List") {
      val var1   = variable("hello")
      val var2   = variable("there")
      val chunk  = List(var1, var2)
      val actual = tuple(chunk)
      assertTrue(
        actual.toString == "(hello, there)",
        actual.exists { case Tuple(attributes, elements) =>
          attributes == () && elements.contains(var1) && elements.contains(var2)
        },
        actual == Tuple((), Variable((), "hello"), Variable((), "there")),
        actual match {
          case Tuple(attributes, List(v1, v2)) => attributes == () && v1 == var1 && v2 == var2
          case _                               => false
        }
      )
    },
    test("testing tuple constructor when given multiple un-attributed elements") {
      val var1   = variable("hello")
      val var2   = variable("There")
      val var3   = variable("notThere")
      val actual = tupleVar(var1, var2)
      assertTrue(
        actual.exists { case Tuple(_, elements) =>
          elements.contains(var1) && elements.contains(var2) && !elements.contains(var3)
        },
        actual.attributes == (),
        actual.toString() == "(hello, there)"
      )
    },
    test("testing tuple with attributes constructor") {
      val var1   = variable("A", "a")
      val var2   = variable("B", "b")
      val var3   = variable("C", "c")
      val actual = tupleWithAttr("Tuple3[a,b,c]", var1, var2, var3)
      assertTrue(
        actual.attributes == "Tuple3[a,b,c]",
        actual.exists { case Tuple(_, elements) => elements == Chunk(var1, var2, var3) },
        actual.toString() == "(a, b, c)"
      )
    }
  )

  def unitSuite = suite("Unit")(
    test("Unit has size 1") {
      val actual = unit.size
      assertTrue(actual == 1)
    },
    test("Unit toString should produce the expected result") {
      val actual = unit.toString
      assertTrue(actual == "()")
    },
    test("When calling foldLeft it should work as expected") {
      val actual = unit.foldLeft(0) { case (acc, _) => acc + 1 }
      assertTrue(actual == 1)
    },
    test("testing attributed unit constructor") {
      val attributes = ("foo.scala", (0, 0), (5, 80))
      val actual     = unit(attributes)
      assertTrue(
        actual.attributes == attributes,
        actual == Unit(attributes),
        actual.exists { case Unit(actualAttributes) => actualAttributes == attributes },
        actual match {
          case Type.Unit(actualAttrbutes @ (_, _, _)) => actualAttrbutes == attributes
          case _                                      => false
        },
        actual.toString == "()"
      )

    },
    test("testing unattributed unit constructor") {
      val actual = unit
      assertTrue(actual.attributes == (), actual.toString == "()")
    }
  )

  def variableSuite = suite("Variable")(
    test("Variable has size 1") {
      val actual = variable("x").size
      assertTrue(actual == 1)
    },
    test("testing first variable constructor") {
      val actual = variable("FizzBuzz")
      assertTrue(
        // actual.satisfies { case Variable(_, name) => name.toString == "[fizz, buzz]" },
        actual == Variable((), "FizzBuzz"),
        actual.toString == "fizzBuzz",
        actual.size == 1
      )
    },
    test("When calling foldLeft it should work as expected") {
      val actual = variable("foo").foldLeft(0) { case (acc, _) => acc + 1 }
      assertTrue(actual == 1)
    },
    test("testing first variable constructor") {
      val actual = variable("FizzBuzz")
      assertTrue(actual.exists { case Variable(_, name) => name.toString == "[fizz,buzz]" }) &&
      assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz"))) &&
      assertTrue(actual == Variable((), "FizzBuzz")) &&
      assertTrue(actual.toString == "fizzBuzz")

    },
    test("testing second variable constructor") {
      val actual = variable(Name("FizzBuzz"))
      assertTrue(actual.exists { case Variable(_, name) => name.toString == "[fizz,buzz]" }) &&
      assertTrue(actual.collectVariables == Set(Name.fromString("FizzBuzz"))) &&
      assertTrue(actual == Variable((), Name.fromString("FizzBuzz")))
    },
    test("eraseAttributes should clear out the Attributes") {
      val actual   = variable((0, 0), "foo")
      val expected = variable("foo")
      assertTrue(
        actual != expected,
        actual.attributes == ((0, 0)) && expected.attributes == (()),
        actual.eraseAttributes == variable("foo"),
        actual.eraseAttributes == actual.mapAttributes(_ => (())),
        actual.toString == "foo"
      )
    }
  )
}
