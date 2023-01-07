package org.finos.morphir.core.types

import org.finos.morphir.core.types.Name
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test.*
object NameSpec extends MorphirBaseSpec {
  def spec = suite("Name Spec")(
    test("fromString should work as expected") {
      def assert(input: String)(expected: String*) =
        assertTrue(Name.fromString(input).toList == expected.toList)

      assert("fooBar_baz 123")("foo", "bar", "baz", "123") &&
      assert("valueInUSD")("value", "in", "u", "s", "d") &&
      assert("ValueInUSD")("value", "in", "u", "s", "d") &&
      assert("value_in_USD")("value", "in", "u", "s", "d") &&
      assert("_-% ")()

    },
    test("An all lowercase alpha string should have a name whose textRepr matches that string") {
      val sut = Name("hello")
      assertTrue(sut.textRepr.encodedValue == "hello")
    },
    test("A title cased single word should encode as expected") {
      val sut = Name("True")
      assertTrue(sut.textRepr.encodedValue == "true")
    },
    test("A number should encode as expected") {
      val sut = Name("01")
      assertTrue(sut.textRepr.encodedValue == "01")
    },
    test("A PascalCased string should encode as expected") {
      val sut = Name("MorphirScala")
      assertTrue(sut.textRepr.encodedValue == s"morphir•scala")
    },
    test("ToList should work as expected") {
      def assert(name: Name, expected: List[String]) =
        assertTrue(name.toList == expected)

      assert(Name("DeltaSigmaTheta"), List("delta", "sigma", "theta")) &&
      assert(Name("SigmaGammaRho"), List("sigma", "gamma", "rho"))
    },
    test("ToString should work as expected") {
      def assert(input: String, expected: String) =
        assertTrue(Name(input).toString == expected)

      assert("foo", "foo") &&
      assert("fooBar", "foo•bar") &&
      assert("Buffalo Bills", "buffalo•bills")
    }
  )
}
