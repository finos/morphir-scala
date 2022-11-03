package org.finos.morphir
package ir
package internal

import zio.Chunk
import Literal.Literal
import PatternModule._

trait PatternModule { module =>
  final type APattern = Pattern[Attributes]
  final type UPattern = Pattern[scala.Unit]

  final type Pattern[+A] = internal.Pattern[A]
  final val Pattern: internal.Pattern.type = internal.Pattern
  import Pattern._

  def mapPatternAttributes[A] = new MapPatternAttributesPartiallyApplied[A]()

  def asPattern[A](attributes: A, pattern: Pattern[A], name: Name)(implicit ev: NeedsAttributes[A]): Pattern[A] =
    AsPattern(attributes, pattern, name)

  final def asPattern[A](attributes: A, pattern: Pattern[A], alias: String): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = Name.fromString(alias))

  def asPattern(pattern: UPattern, name: Name): UPattern =
    AsPattern((), pattern, name)

  /**
   * Destructure a tuple using a pattern for every element
   */
  final def tuplePattern[A](attributes: A, patterns: Chunk[Pattern[A]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = patterns)

  final def tuplePattern[A](attributes: A, patterns: Pattern[A]*)(implicit ev: Not[A <:< Pattern[_]]): Pattern[A] =
    Pattern.TuplePattern(attributes = attributes, elementPatterns = Chunk.fromIterable(patterns))

  final def tuplePattern(patterns: Chunk[UPattern]): UPattern =
    Pattern.TuplePattern(attributes = (), elementPatterns = patterns)

  final def tuplePattern(patterns: UPattern*): UPattern =
    Pattern.TuplePattern(attributes = (), elementPatterns = Chunk.fromIterable(patterns))

  final def unitPattern: UPattern                     = Pattern.UnitPattern(())
  final def unitPattern[A](attributes: A): Pattern[A] = Pattern.UnitPattern(attributes)

  def wildcardPattern[A](attributes: A)(implicit ev: NeedsAttributes[A]): Pattern[A] = WildcardPattern(attributes)
  lazy val wildcardPattern: UPattern                                                 = WildcardPattern(())

  final def toString[A](pattern: Pattern[A]): String = pattern.toString()

}

object PatternModule {
  class MapPatternAttributesPartiallyApplied[A](private val dummy: Boolean = false) extends AnyVal {
    def apply[B](f: A => B, pattern: Pattern[A]): Pattern[B] =
      pattern.mapAttributes(f)
  }
}
