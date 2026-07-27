package morphir.langkit.trees

import neotype.*

/**
 * Validated newtype for the identifying name of a node kind.
 *
 * A `NodeTypeName` is never empty and never consists solely of whitespace. Construction goes through
 * `NodeTypeName.make`; callers with a compile-time literal can use `NodeTypeName(literal)` (subject to neotype's inline
 * validation).
 */
type NodeTypeName = NodeTypeName.Type

object NodeTypeName extends Newtype[String]:

  override inline def validate(input: String): Boolean | String =
    if input.isEmpty then "NodeTypeName must not be empty"
    else if input.trim.isEmpty then "NodeTypeName must not be whitespace-only"
    else true

  given CanEqual[Type, Type] = CanEqual.derived
