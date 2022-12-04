package org.finos.morphir
package ir
package internal

import zio.prelude._

object NonEmptyStringValidator
    extends Validator[String](str =>
      if str.isEmpty then Left(AssertionError.Failure("is non-empty String.")) else Right(())
    )
