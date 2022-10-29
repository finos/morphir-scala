package org.finos.morphir
package ir
package internal

import zio.Chunk
import Literal.Literal

private[internal] sealed trait Pattern[+A] { self =>
  def attributes: A
}

private[internal] object Pattern {
  final type UPattern = Pattern[scala.Unit]

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
