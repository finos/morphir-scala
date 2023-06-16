package org.finos.morphir.toolkit.props
import com.eed3si9n.expecty.Expecty.expect

class PropertySpec extends munit.ScalaCheckSuite {
  test("Two properties with the same name and type should be the same") {
    val propertyA = Property("user", "")
    val propertyB = Property("user", "")
    val propertyC = Property("user", "N/A")

    expect(propertyA == propertyB, propertyA == propertyC)
  }

  test("It should support creating a Binding using the \":=\" operator") {
    val property = Property("lastName", "Smith")
    assertEquals((property := "Jones"), Property.Binding(property, "Jones"))
  }
}
