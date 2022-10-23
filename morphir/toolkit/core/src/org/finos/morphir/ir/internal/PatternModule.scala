package org.finos.morphir
package ir
package internal

import zio.Chunk

trait PatternModule extends LiteralModule { module =>
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
  def tuplePattern[A](attributes: A, elementPatterns: Chunk[Pattern[A]])(implicit ev: NeedsAttributes[A]): Pattern[A] =
    TuplePattern(attributes, elementPatterns)

  def tuplePattern[A](attributes: A, elementPatterns: List[Pattern[A]])(implicit ev: NeedsAttributes[A]): Pattern[A] =
    TuplePattern(attributes, Chunk.fromIterable(elementPatterns))

  final def unitPattern: UPattern                     = Pattern.UnitPattern(scala.Unit)
  final def unitPattern[A](attributes: A): Pattern[A] = Pattern.UnitPattern(attributes)

  def wildcardPattern[A](attributes: A)(implicit ev: NeedsAttributes[A]): Pattern[A] = WildcardPattern(attributes)
  lazy val wildcardPattern: Pattern[Attributes]                                      = WildcardPattern(Attributes.empty)

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
