package morphir.knowledge.logic.core
import com.eed3si9n.expecty.Expecty.expect
import zio.*
import munit.*
class FieldsSpec extends ScalaCheckSuite {
  test("valueOf should return the value when the substitution directly contains the value") {
    val nameField = Field.define[String]("name")
    val bindings  = Fields.init(nameField -> "John Doe")
    val actual    = bindings.valueOf(nameField)
    assertEquals(actual, Some("John Doe"))
  }
  test("valueOf should return the value when the substitution in-directly contains the value") {
    val nameField  = Field.define[String]("name")
    val aliasField = Field.define[String]("alias")
    val bindings   = Fields.init(nameField -> "John Doe", aliasField -> nameField)
    val actual     = bindings.valueOf(aliasField)
    expect(
      actual == Some("John Doe"),
      bindings.fields == Set[Field[_]](nameField, aliasField)
    )
  }

  test("valueOf should return None if there is no path to the field value") {
    val a        = Field.define[Int]("a")
    val b        = Field.define[Int]("b")
    val c        = Field.define[Int]("c")
    val bindings = Fields.init(a -> 42, c -> b)
    val actual   = bindings.valueOf(b)
    expect(
      actual == None,
      bindings.fields == Set[Field[_]](a, c)
    )
  }

  test("valueOf should return the value only if it matches the field type") {
    val indirect    = Field.define[Int]("indirectTuple")
    val indirect2   = Field.define[Boolean]("indirect")
    val valueHolder = Field.define[Int]("valueHolder")
    val bindings    = Fields.init(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
    expect(
      bindings.valueOf(indirect) == Some(42),
      bindings.valueOf(indirect2) == None,
      bindings.valueOf(valueHolder) == Some(42)
    )
  }

  test("dynamicValueOf will return the field value irrespective of type") {
    val indirect    = Field.define[Int]("indirectTuple")
    val indirect2   = Field.define[Boolean]("indirect")
    val valueHolder = Field.define[Int]("valueHolder")
    val bindings    = Fields.init(indirect -> valueHolder, indirect2 -> valueHolder, valueHolder -> 42)
    expect(
      bindings.dynamicValueOf(indirect) == 42,
      Option(bindings.dynamicValueOf(indirect)) == bindings.valueOf(indirect),
      bindings.dynamicValueOf(indirect2) == 42,
      Option(bindings.dynamicValueOf(indirect2)) != bindings.valueOf(indirect2),
      bindings.dynamicValueOf(valueHolder) == 42,
      Option(bindings.dynamicValueOf(valueHolder)) == bindings.valueOf(valueHolder)
    )
  }

  test("Adding fields should add a field when fields is empty") {
    val nameField = Field.define[String]("name")
    val sut       = Fields.empty
    val actual    = sut + (nameField -> "John Doe")
    assertEquals(actual,Fields.init(nameField -> "John Doe"))
  }
}
