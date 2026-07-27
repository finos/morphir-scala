package morphir.langkit.trees.query

import neotype.*

/**
 * Validated name of a query predicate clause (`#eq?`, `#match?`, …).
 *
 * A predicate name starts with `#` and ends with `?`, and has at least one character between them.
 */
type PredicateName = PredicateName.Type

object PredicateName extends Newtype[String]:

  override inline def validate(input: String): Boolean | String =
    if input.length < 3 then "PredicateName must have at least one character between '#' and '?'"
    else if !input.startsWith("#") then "PredicateName must start with '#'"
    else if !input.endsWith("?") then "PredicateName must end with '?'"
    else true

  given CanEqual[Type, Type] = CanEqual.derived

  /** Built-in: text equality. */
  val Eq: PredicateName = unsafeMake("#eq?")

  /** Built-in: regex text match. */
  val Match: PredicateName = unsafeMake("#match?")

  /** Built-in: text inequality. */
  val NotEq: PredicateName = unsafeMake("#not-eq?")

  /** Built-in: regex text non-match. */
  val NotMatch: PredicateName = unsafeMake("#not-match?")
