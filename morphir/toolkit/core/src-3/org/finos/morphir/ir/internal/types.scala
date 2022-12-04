package org.finos.morphir
package ir
package internal
import zio.prelude.Subtype
import zio.prelude.SubtypeCustom

object types:
  type NonEmptyString = NonEmptyString.Type
  object NonEmptyString extends SubtypeCustom[String]:
    import zio.prelude.Assertion._

    protected def validate(value: String) =
      NonEmptyStringValidator.validate(value)

    protected inline def validateInline(inline value: String) =
      ${ NonEmptyStringValidator.validateInlineImpl('value) }
