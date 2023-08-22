package org.finos.morphir

import org.finos.morphir.testing.MorphirBaseSpec
import org.finos.morphir.naming._
import org.finos.morphir.mir._
import zio.test._
object FieldKSpec extends MorphirBaseSpec {
  def spec = suite("FieldKSpec")(
    constructionSuite,
    mappingSuite
  )

  def constructionSuite = suite("Construction")(
    test("It should support creating a FieldK  from a Name and a non-Type data field") {
      val actual = FieldK(n"names", List("John", "Jane", "Joe"))
      assertTrue(
        actual.name == Name.fromList("names"),
        actual.data == List("John", "Jane", "Joe")
      )
    },
    test("It should support creating a FieldK from a String name and a non-Type data field") {
      val actual = FieldK("Users", List("John", "Jane", "Joe"))
      assertTrue(
        actual.name == Name.fromList("users"),
        actual.data == List("John", "Jane", "Joe")
      )
    }
  )

  def mappingSuite = suite("Mapping")(
    test("It should support mapping over a List") {
      val actual   = FieldK(n"names", List("John", "Jane", "Joe"))
      val expected = FieldK(n"names", List("JOHN", "JANE", "JOE"))
      assertTrue(
        actual.map(_.toUpperCase) == expected
      )
    },
    test("It should support mapping over an Option") {
      val actualWithSome   = FieldK(n"names", Option("John"))
      val expectedWithSome = FieldK(n"names", Option("JOHN"))
      val actualWithNone   = FieldK(n"names", Option.empty[String])
      val expectedWithNone = FieldK(n"names", Option.empty[String])
      assertTrue(
        actualWithSome.map(_.toUpperCase) == expectedWithSome,
        actualWithNone.map(_.toUpperCase) == expectedWithNone
      )
    }
  )
}
