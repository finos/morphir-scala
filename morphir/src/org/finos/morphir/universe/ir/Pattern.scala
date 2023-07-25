package org.finos.morphir.universe.ir

import org.finos.morphir.universe.ir.Literal as Lit
import zio.Chunk

sealed trait Pattern[+A] {
  def attributes: A
}

object Pattern {
  final case class WildcardPattern[+A](attributes: A)                                  extends Pattern[A]
  final case class AsPattern[+A](attributes: A, pattern: Pattern[A], name: Name)       extends Pattern[A]
  final case class TuplePattern[+A](attributes: A, elementPatterns: Chunk[Pattern[A]]) extends Pattern[A]
  final case class ConstructorPattern[+A](attributes: A, constructorName: FQName, argumentPatterns: Chunk[Pattern[A]])
      extends Pattern[A]
  final case class EmptyListPattern[+A](attributes: A) extends Pattern[A]
  final case class HeadTailPattern[+A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A])
      extends Pattern[A]
  final case class LiteralPattern[+A](attributes: A, literal: Lit) extends Pattern[A]
  final case class UnitPattern[+A](attributes: A)                  extends Pattern[A]
}
