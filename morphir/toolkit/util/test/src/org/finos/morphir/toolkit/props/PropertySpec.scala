package org.finos.morphir.toolkit.props
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
object PropertySpec extends MorphirBaseSpec {
  def spec = suite("Property Spec")(
    test("Two properties with the same name and type should be the same") {
      val propertyA = Property("user", "")
      val propertyB = Property("user", "")
      val propertyC = Property("user", "N/A")

      assertTrue(propertyA == propertyB, propertyA == propertyC)
    },
    test("It should support creating a Binding using the \":=\" operator") {
      val property = Property("lastName", "Smith")
      assertTrue((property := "Jones") == Property.Binding(property, "Jones"))
    }
  )
}
