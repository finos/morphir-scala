package org.finos.morphir.ir.types
package folding

import zio.Chunk
import org.finos.morphir.ir.{FQName, Name}
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import Type._
object FoldingTypeSpec extends MorphirBaseSpec {
  def spec = suite("FoldingTypeSpec")(
    sizeSuite,
    tupleSuite,
    unitSuite,
    variableSuite
  )

  def sizeSuite = suite("size")(
    test("size of Unit") {
      val sut    = Type.unit
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Variable") {
      val sut    = variable("x")
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of simple Reference") {
      val sut    = Reference[Any]((), FQName.fromString("x"), Chunk.empty)
      val actual = sut.size
      assertTrue(actual == 1)
    },
    test("size of Reference with a single typeParam") {
      val sut    = Reference[Any]((), FQName.fromString("x"), Chunk(Type.Variable[Any]((), Name.fromString("y"))))
      val actual = sut.size
      assertTrue(actual == 2)
    }
  )

  def tupleSuite = suite("Tuple")(
    test("testing emptyTuple constructor") {
      val sut = Type.emptyTuple("Attributes")
      assertTrue(
        sut.size == 1,
        sut.toString == "()",
        sut.attributes == "Attributes",
        sut == Tuple("Attributes", Chunk.empty)
      )
    },
    test("testing tuple constructor when given a Chunk") {
      val helloVar = variable("hello")
      val worldVar = variable("world")
      val chunk    = Chunk(helloVar, worldVar)
      val actual   = tuple(chunk)
      assertTrue(
        actual.size == 3,
        actual.toString == "(hello, world)",
        actual.attributes == (),
        actual == Tuple((), chunk)
      )
    },
    test("testing tuple constructor when given multiple un-attributed elements") {
      val var1   = variable("one")
      val var2   = variable("two")
      val var3   = variable("three")
      val actual = tuple(var1, var2, var3)
      assertTrue(
        actual.size == 4,
        actual.toString == "(one, two, three)",
        actual.attributes == (),
        actual == Tuple((), Chunk(var1, var2, var3))
      )
    },
    test("testing tuple with attributes constructor") {
      val varA   = variable("A", "a")
      val varB   = variable("B", "b")
      val varC   = variable("C", "c")
      val actual = tuple("(a, b, c)", varA, varB, varC)
      assertTrue(
        actual.size == 4,
        actual.toString == "(a, b, c)",
        actual.attributes == "(a, b, c)",
        actual == Tuple("(a, b, c)", Chunk(varA, varB, varC))
      )
    }
  )

  def unitSuite = suite("Unit")(
    test("Unit has size 1") {
      val actual = Type.unit.size
      assertTrue(actual == 1)
    },
    test("Unit toString should produce the expected result") {
      val actual = Type.unit.toString
      assertTrue(actual == "()")
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
        actual == Variable[Any]((), "FizzBuzz"),
        actual.toString == "fizzBuzz",
        actual.size == 1
      )
    }
  )
}
