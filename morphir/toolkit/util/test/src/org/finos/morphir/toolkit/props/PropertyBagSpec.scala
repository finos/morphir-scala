package org.finos.morphir.toolkit.props

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object PropertyBagSpec extends MorphirBaseSpec {
  def spec = suite("PropertyBag Spec")(
    test("It should return the initial value if you try and get a property not in the bag") {
      val property = Property("isEmpty", true)
      val sut      = PropertyBag.empty
      assertTrue(sut.get(property) == true)
    },
    test("Can be updated from Bindings using `++=`"){
      val firstNameProperty = Property("firstName", "N/A")
      val lastNameProperty = Property("lastName", "N/A")
      val ageProperty = Property("age", 0)
      val counterMetric = Metric[Int]("counter", 0, (l,r) => l + r)
      val sut = PropertyBag.empty
      val actual = sut ++= Seq(
        firstNameProperty := "John",
        lastNameProperty := "Smith",
        ageProperty := 42,
        counterMetric := 2,
        counterMetric := 3,
        firstNameProperty := "Jane"
      )
      assertTrue(
        actual.get(firstNameProperty) == "Jane",
        actual.get(lastNameProperty) == "Smith",
        actual.get(ageProperty) == 42,
        actual.get(counterMetric) == 5
      )
    }
  )
}
