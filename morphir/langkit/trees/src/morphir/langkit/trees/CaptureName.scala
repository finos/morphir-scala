package morphir.langkit.trees

import neotype.*

/**
 * Validated name of a query capture (`@foo`).
 *
 * Must be non-empty, non-whitespace, and start with a letter or underscore (matching the identifier grammar used by
 * [[morphir.langkit.trees.query.QueryParser]]).
 */
type CaptureName = CaptureName.Type

object CaptureName extends Newtype[String]:

  override inline def validate(input: String): Boolean | String =
    if input.isEmpty then "CaptureName must not be empty"
    else if input.trim.isEmpty then "CaptureName must not be whitespace-only"
    else if !(input.head.isLetter || input.head == '_') then "CaptureName must start with a letter or underscore"
    else true

  given CanEqual[Type, Type] = CanEqual.derived
