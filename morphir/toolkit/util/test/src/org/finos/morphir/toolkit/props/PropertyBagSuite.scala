package org.finos.morphir.toolkit.props

import com.eed3si9n.expecty.Expecty.expect

class PropertyBagSuite extends munit.ScalaCheckSuite {
  test("It should return the initial value if you try and get a property not in the bag") {
    val property = Property("isEmpty", true)
    val sut      = PropertyBag.empty
    assertEquals(sut.get(property), true)
  }

  test("Can be updated from Bindings using `++=`") {
    val firstNameProperty = Property("firstName", "N/A")
    val lastNameProperty  = Property("lastName", "N/A")
    val ageProperty       = Property("age", 0)
    val counterMetric     = Property.makeMetric[Int]("counter", 0, (l, r) => l + r)
    val sut               = PropertyBag.empty
    val actual = sut ++= Seq(
      firstNameProperty := "John",
      lastNameProperty  := "Smith",
      ageProperty       := 42,
      counterMetric     := 2,
      counterMetric     := 3,
      firstNameProperty := "Jane"
    )
    expect(
      actual.get(firstNameProperty) == "Jane",
      actual.get(lastNameProperty) == "Smith",
      actual.get(ageProperty) == 42,
      actual.get(counterMetric) == 5
    )
  }

  test("Can set an existing value") {
    val email = Property("email", "")
    val original = PropertyBag(
      email := "nobody@nowhere.com"
    )
    val actual = original.set(email, "somebody@everywhere.com")
    assertEquals(actual.get(email), "somebody@everywhere.com")
  }
}
