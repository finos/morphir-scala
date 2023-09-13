package org.finos.morphir
package ir
package internal
import zio.prelude._

object types {
  type NonEmptyString = NonEmptyString.Type
  object NonEmptyString extends Subtype[String] {
    import zio.prelude.Assertion._

    override def assertion = assertCustom { str =>
      if (str.isEmpty()) Left(AssertionError.Failure("is non-empty String.")) else Right(())
    }
  }
}
