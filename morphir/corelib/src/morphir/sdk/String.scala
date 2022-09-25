package morphir.sdk
import org.finos.morphir.interop._

import java.lang.{String => Str}
import scala.language.implicitConversions

@extern
object String:
  import morphir.sdk.Bool.Bool
  import morphir.sdk.Int
  import morphir.sdk.Int.Int

  opaque type String = Str

  /** Append two strings. */
  @extern def append(a: String, b: String): String = a + b

  /** Determine if a string is empty. */
  @extern def isEmpty(value: String): Bool = value.isEmpty

  /** Get the length of a string */
  @extern def length(value: String): Int = Int.wrap(value.length())

  /** Repeat a string n times */
  @extern def repeat(n: Int, value: String): String = value * Int.unwrap(n)

  /** Replace all occurrences of some substring. */
  def replace(before: String, after: String): String = extern

  /** Reverse a string */
  @extern def reverse(value: String): String = value.reverse

  private[sdk] def wrap(value: Str): String   = value
  private[sdk] def unwrap(value: String): Str = value
