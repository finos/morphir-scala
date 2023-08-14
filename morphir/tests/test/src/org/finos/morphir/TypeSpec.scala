package org.finos.morphir

import org.finos.morphir.naming._
import org.finos.morphir.mir._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
object TypeSpec extends MorphirBaseSpec {
  def spec = suite("TypeSpec")(
    suite("Type[+A]")(
      extensibleRecordSuite,
      functionSuite,
      recordSuite,
      tupleSuite,
      referenceSuite,
      unitSuite,
      variableSuite
    )
  )

  def extensibleRecordSuite = suite("ExtensibleRecord")()
  def functionSuite         = suite("Function")()

  def recordSuite = suite("Record")()

  def referenceSuite = suite("Reference")(
    suite("ToString")()
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
    ),
    suite("Folding")(
      test("Folding with foldUp should produce the expected results") {
        val sut = Type.Tuple(-43, Type.Variable(42, n"x") :: Type.Variable(43, n"y") :: Nil)
        val actual @ (actualSum, actualList) = sut.foldUp((0, List.empty[Int])) {
          case (tpe, (sum, lst)) => (sum + tpe.attributes, tpe.attributes :: lst)
        }
        assertTrue(
          actual == (42, List(42, 43, -43)),
          actualSum == 42,
          actualList.size == 3
        )
      }
    )
  )

  def unitSuite = suite("Unit")(
    suite("ToString")(
      test("It should provide an appropriate String representation of the type expr") {
        val sut = Type.Unit(())
        assertTrue(
          sut.toString == "()"
        )
      }
    ),
    suite("Mapping")(
      test("Mapping a Unit[Int] using toString should return a Unit[String]") {
        val actual   = Type.Unit(42)
        val expected = Type.Unit("42")
        assertTrue(
          actual.map(_.toString) == expected,
          actual.map(_.toString) == actual.mapAttributes(_.toString)
        )
      }
    ),
    suite("Folding")(
      test("Folding with foldUp should produce the expected results") {
        val sut = Type.Unit(100)
        val actual = sut.foldUp(List.empty[Int]) {
          case (tpe, acc) => tpe.attributes :: acc
        }
        assertTrue(
          actual == List(100),
          actual.size == 1
        )
      }
    )
  )

  def variableSuite = suite("Variable")(
    suite("ToString")(
      test("It should provide an appropriate String representation of the type expr") {
        val sut = Type.Variable(100, n"SomeVar")
        assertTrue(
          sut.toString == "someVar"
        )
      }
    ),
    suite("Mapping")(
      test("Mapping a Variable[Int] using toString should return a Variable[String]") {
        val sut      = Type.Variable(100, n"Acme")
        val expected = Type.Variable("100", Name.fromList("acme"))
        assertTrue(
          sut.map(_.toString) == expected,
          sut.map(_.toString) == sut.mapAttributes(_.toString)
        )
      }
    ),
    suite("Folding")(
      test("Folding with foldUp should produce the expected results") {
        val sut = Type.Variable(42, n"x")
        val actual = sut.foldUp(List.empty[Int]) {
          case (tpe, acc) => tpe.attributes :: acc
        }
        assertTrue(
          actual == List(42),
          actual.size == 1
        )
      }
    )
  )
}
