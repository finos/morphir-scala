package org.finos.morphir
package ir
package internal

import org.finos.morphir.testing.MorphirBaseSpec
import types.NonEmptyString
import zio.Console
import zio.prelude.Validation
import zio.test._
import zio.test.Assertion.isLeft

object NonEmptyStringSpec extends MorphirBaseSpec {
  def spec = suite("NonEmptyString Spec")(
    test("Should error if making an empty string") {
      val actual = NonEmptyString.make("")
      assertTrue(actual == Validation.fail(" did not satisfy is non-empty String."))
      // for {
      //   res <- typeCheck("""
      //   import org.finos.morphir.ir.internal.types.NonEmptyString
      //   val s:NonEmptyString = NonEmptyString("1")
      //   """)
      //   _ <- Console.printLine(s"res: $res")
      // } yield assert(res)(isLeft)
    }
  )
}
