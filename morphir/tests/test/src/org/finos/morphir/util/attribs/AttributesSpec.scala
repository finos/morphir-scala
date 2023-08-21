package org.finos.morphir.util.attribs

import zio.test._
import org.finos.morphir.testing.MorphirBaseSpec

object AttributesSpec extends MorphirBaseSpec {
  def spec = suite("AttributesSpec")(
    test("It should return the initial value if you try and get a property not in the bag") {
      val property = Attribute("isEmpty", true)
      val sut      = Attributes.empty
      assertTrue(sut.get(property) == true)
    },
    test("Can be updated from Bindings using `++=`") {
      val firstNameAttribute = Attribute("firstName", "N/A")
      val lastNameAttribute  = Attribute("lastName", "N/A")
      val ageAttribute       = Attribute("age", 0)
      val counterMetric      = Attribute.makeMetric[Int]("counter", 0, (l, r) => l + r)
      val sut                = Attributes.empty
      val actual = sut ++= Seq(
        firstNameAttribute := "John",
        lastNameAttribute  := "Smith",
        ageAttribute       := 42,
        counterMetric      := 2,
        counterMetric      := 3,
        firstNameAttribute := "Jane"
      )
      assertTrue(
        actual.get(firstNameAttribute) == "Jane",
        actual.get(lastNameAttribute) == "Smith",
        actual.get(ageAttribute) == 42,
        actual.get(counterMetric) == 5
      )
    },
    test("Can set an existing value") {
      val email = Attribute("email", "")
      val original = Attributes(
        email := "nobody@nowhere.com"
      )
      val actual = original.set(email, "somebody@everywhere.com")
      assertTrue(actual.get(email) == "somebody@everywhere.com")
    }
  )
}
