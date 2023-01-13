package org.finos.morphir.core.types

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import org.finos.morphir.core.types.Strings.EncodedString

object EncodedStringSpec extends MorphirBaseSpec {
  def spec = suite("EncodedString Spec")(
    test("It should be possible to create an encoded string") {
      val actual = EncodedString("Hello World")
      assertTrue(actual.value == "Hello World", actual.toString == "Hello World")
    }
  )
}
