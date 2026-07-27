package morphir.langkit.trees

import neotype.*

/**
 * Validated name of a named sub-tree on a tree node (`field_name: (Child)`).
 *
 * Must be non-empty, non-whitespace, and start with a letter or underscore.
 */
type FieldName = FieldName.Type

object FieldName extends Newtype[String]:

  override inline def validate(input: String): Boolean | String =
    if input.isEmpty then "FieldName must not be empty"
    else if input.trim.isEmpty then "FieldName must not be whitespace-only"
    else if !(input.head.isLetter || input.head == '_') then "FieldName must start with a letter or underscore"
    else true

  given CanEqual[Type, Type] = CanEqual.derived
