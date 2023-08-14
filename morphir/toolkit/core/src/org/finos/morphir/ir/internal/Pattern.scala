package org.finos.morphir
package ir
package internal

import org.finos.morphir.naming._
import zio.Chunk
import Literal.Literal
import Pattern._

sealed trait Pattern[+A] { self =>
  def attributes: A

  def map[B](f: A => B): Pattern[B] = self match {
    case AsPattern(attributes, pattern, name) => AsPattern(f(attributes), pattern.map(f), name)
    case EmptyListPattern(attributes)         => EmptyListPattern(f(attributes))
    case HeadTailPattern(attributes, headPattern, tailPattern) =>
      HeadTailPattern(f(attributes), headPattern.map(f), tailPattern.map(f))
    case TuplePattern(attributes, elementPatterns) => TuplePattern(f(attributes), elementPatterns.map(_.map(f)))
    case UnitPattern(attributes)                   => UnitPattern(f(attributes))
    case WildcardPattern(attributes)               => WildcardPattern(f(attributes))
    case LiteralPattern(attributes, literal)       => LiteralPattern(f(attributes), literal)
    case ConstructorPattern(attributes, name, patterns) =>
      ConstructorPattern(f(attributes), name, patterns.map(_.map(f)))
  }

  @inline final def mapAttributes[B](f: A => B): Pattern[B] = map(f)

  def withAttributes[B >: A](attributes: => B): Pattern[B] =
    self.map(_ => attributes)

  override def toString(): String = self match {
    case AsPattern(_, WildcardPattern(_), alias) => alias.toCamelCase
    case AsPattern(_, pattern, name)             => s"$pattern as ${name.toCamelCase}"
    case ConstructorPattern(_, constructorName, argumentPatterns) =>
      val ctor = constructorName.toReferenceName
      val args = argumentPatterns.map(_.toString).mkString(" ")
      s"$ctor $args"
    case EmptyListPattern(_)                          => "[]"
    case HeadTailPattern(_, headPattern, tailPattern) => s"$headPattern :: $tailPattern"
    case LiteralPattern(_, literal)                   => literal.toString()
    case TuplePattern(_, elementPatterns)             => elementPatterns.mkString("(", ", ", ")")
    case UnitPattern(_)                               => "()"
    case WildcardPattern(_)                           => "_"
  }
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
