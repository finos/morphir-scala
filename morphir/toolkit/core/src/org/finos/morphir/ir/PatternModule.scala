package org.finos.morphir
package ir

import zio.Chunk
import Literal.Literal

trait PatternModule { module =>
  import Pattern._

  final type APattern = Pattern[Attributes]
  final type UPattern = Pattern[scala.Unit]

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

  sealed trait Pattern[+A] { self =>
    def attributes: A
  }

  object Pattern {
    sealed case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name) extends Pattern[A]
    sealed case class ConstructorPattern[+A](
        attributes: A,
        constructorName: FQName,
        argumentPatterns: Chunk[Pattern[A]]
    ) extends Pattern[A]
    sealed case class EmptyListPattern[+A](attributes: A) extends Pattern[A]
    sealed case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
        extends Pattern[A]
    sealed case class LiteralPattern[+A](attributes: A, literal: Literal)                 extends Pattern[A]
    sealed case class TuplePattern[+A](attributes: A, elementPatterns: Chunk[Pattern[A]]) extends Pattern[A]
    sealed case class UnitPattern[+A](attributes: A)                                      extends Pattern[A]
    sealed case class WildcardPattern[+A](attributes: A)                                  extends Pattern[A]
  }
}
