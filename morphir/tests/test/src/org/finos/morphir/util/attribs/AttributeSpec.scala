package org.finos.morphir.util.attribs

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object AttributeSpec extends MorphirBaseSpec {
  def spec = suite("AttributeSpec")(
    test("Two properties with the same name and type should be the same") {
      val propertyA = Attribute("user", "")
      val propertyB = Attribute("user", "")
      val propertyC = Attribute("user", "N/A")

      assertTrue(propertyA == propertyB, propertyA == propertyC)
    },
    test("It should support creating a Binding using the \":=\" operator") {
      val property = Attribute("lastName", "Smith")
      assertTrue((property := "Jones") == Attribute.Binding(property, "Jones"))
    }
  )
}
