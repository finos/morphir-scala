package morphir.langkit.trees.query

import scala.util.Try
import scala.util.matching.Regex

/**
 * A regular-expression pattern used by `#match?` predicates.
 *
 * Construction validates the source and pre-compiles the regex so the matcher never pays the compile cost more than
 * once per pattern. Equality and hash are source-string-only; the compiled `Regex` is cache-like state.
 */
final class RegexPattern private (val source: String) derives CanEqual:
  val compiled: Regex = source.r

  override def equals(obj: Any): Boolean = obj match
    case that: RegexPattern => source == that.source
    case _                  => false

  override def hashCode: Int    = source.hashCode
  override def toString: String = s"RegexPattern($source)"

object RegexPattern:

  /** Validate and compile the source. Returns an error string if the source is not a valid regex. */
  def make(source: String): Either[String, RegexPattern] =
    Try(new RegexPattern(source)).toEither.left
      .map(err => s"RegexPattern is not a valid regular expression: '$source' (${err.getMessage})")

  /** Construct without validation. Only callers that have already proven the source compiles may use this. */
  def unsafeMake(source: String): RegexPattern = new RegexPattern(source)
