package org.finos.morphir

import org.finos.morphir.naming._
import org.finos.morphir.mir._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
object TypeSpec extends MorphirBaseSpec {
  def spec = suite("TypeSpec")(
    suite("Type[+A]")(
      tupleSuite,
      unitSuite,
      variableSuite
    )
  )

  def tupleSuite = suite("Tuple")(
    suite("Mapping")(
      test("Mapping over a Tuples attributes should work") {
        val element1     = Type.Variable("Variable", n"Element1")
        val element2     = Type.Variable("Variable", n"Element2")
        val element3     = Type.Variable("Variable", n"Element3")
        val stringRef    = Type.Reference("Reference", pkg"Morphir.SDK" % "Basics" % "String", Nil)
        val listOfString = Type.Reference("Reference", pkg"Morphir.SDK" % "List" % "List", stringRef :: Nil)
        val sut          = Type.Tuple("Tuple", element1 :: element2 :: element3 :: listOfString :: Nil)
        val actual       = sut.map(_ => ())
        assertTrue(
          actual.size == sut.size
        )
      }
    )
  )

  def unitSuite = suite("Unit")(
    suite("Mapping")(
      test("Mapping a Unit[Int] using toString should return a Unit[String]") {
        val actual   = Type.Unit(42)
        val expected = Type.Unit("42")
        assertTrue(
          actual.map(_.toString) == expected,
          actual.map(_.toString) == actual.mapAttributes(_.toString)
        )
      }
    )
  )

  def variableSuite = suite("Variable")(
    suite("Mapping")(
      test("Mapping a Variable[Int] using toString should return a Variable[String]") {
        val sut      = Type.Variable(100, n"Acme")
        val expected = Type.Variable("100", Name.fromList("acme"))
        assertTrue(
          sut.map(_.toString) == expected,
          sut.map(_.toString) == sut.mapAttributes(_.toString)
        )
      }
    )
  )
}
