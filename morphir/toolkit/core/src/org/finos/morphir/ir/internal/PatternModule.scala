package org.finos.morphir
package ir
package internal

import org.finos.morphir.naming._
import zio.Chunk
import Literal.Literal
import PatternModule._

trait PatternModule { module =>
  final type APattern = Pattern[Attributes]
  final type UPattern = Pattern[scala.Unit]

  final type Pattern[+A] = internal.Pattern[A]
  final val Pattern: internal.Pattern.type = internal.Pattern
  private final val DefaultAttributes      = ()
  import Pattern._

  def mapPatternAttributes[A] = new MapPatternAttributesPartiallyApplied[A]()

  final def asAlias[A](attributes: A, alias: String): Pattern[A] =
    Pattern.AsPattern(
      attributes = attributes,
      pattern = Pattern.WildcardPattern(attributes),
      name = Name.fromString(alias)
    )

  final def asAlias[A](attributes: A, alias: Name): Pattern[A] =
    Pattern.AsPattern(
      attributes = attributes,
      pattern = Pattern.WildcardPattern(attributes),
      name = alias
    )

  final def asAlias(alias: String): UPattern =
    Pattern.AsPattern(
      attributes = (),
      pattern = wildcardPattern,
      name = Name.fromString(alias)
    )

  final def asAlias(alias: Name): UPattern =
    Pattern.AsPattern(
      attributes = (),
      pattern = wildcardPattern,
      name = alias
    )

  def asPattern[A](attributes: A, pattern: Pattern[A], name: Name)(implicit ev: NeedsAttributes[A]): Pattern[A] =
    AsPattern(attributes, pattern, name)

  final def asPattern[A](attributes: A, pattern: Pattern[A], alias: String): Pattern[A] =
    Pattern.AsPattern(attributes = attributes, pattern = pattern, name = Name.fromString(alias))

  def asPattern(pattern: UPattern, name: Name): UPattern =
    AsPattern((), pattern, name)

  final def asPattern(pattern: UPattern, alias: String): UPattern =
    Pattern.AsPattern(attributes = (), pattern = pattern, name = Name.fromString(alias))

  final def booleanPattern[A](attributes: A, value: Boolean): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.boolean(value))

  final def booleanPattern(value: Boolean): UPattern =
    Pattern.LiteralPattern(attributes = (), literal = Literal.boolean(value))

  final def constructorPattern[A](
      attributes: A,
      constructorName: FQName,
      argumentPatterns: Chunk[Pattern[A]]
  ): Pattern[A] =
    Pattern.ConstructorPattern(
      attributes = attributes,
      constructorName = constructorName,
      argumentPatterns = argumentPatterns
    )

  final def constructorPattern[A](
      attributes: A,
      constructorName: String,
      argumentPatterns: Chunk[Pattern[A]]
  ): Pattern[A] =
    Pattern.ConstructorPattern(
      attributes = attributes,
      constructorName = FQName.fromString(constructorName),
      argumentPatterns = argumentPatterns
    )

  final def constructorPattern(
      constructorName: String,
      argumentPatterns: Chunk[UPattern]
  ): UPattern =
    Pattern.ConstructorPattern(
      attributes = (),
      constructorName = FQName.fromString(constructorName),
      argumentPatterns = argumentPatterns
    )

  final def constructorPattern(
      constructorName: FQName,
      argumentPatterns: Chunk[UPattern]
  ): UPattern =
    Pattern.ConstructorPattern(
      attributes = (),
      constructorName = constructorName,
      argumentPatterns = argumentPatterns
    )

  final def decimalPattern[A](attributes: A, value: BigDecimal): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.decimal(value))

  final def decimalPattern(value: BigDecimal): UPattern =
    Pattern.LiteralPattern(attributes = (), literal = Literal.decimal(value))

  final def emptyListPattern[A](attributes: A): Pattern[A] =
    EmptyListPattern(attributes)

  final def emptyListPattern: UPattern = EmptyListPattern(())

  final def falsePattern[A](attributes: A): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.boolean(false))

  final def falsePattern: UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.boolean(false))

  final def floatPattern[A](attributes: A, value: Float): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.float(value))

  final def floatPattern(value: Float): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.float(value))

  final def headTailPattern[A](attributes: A, headPattern: Pattern[A], tailPattern: Pattern[A]): Pattern[A] =
    HeadTailPattern(attributes, headPattern, tailPattern)

  def headTailPattern(headPattern: UPattern, tailPattern: UPattern): UPattern =
    HeadTailPattern((), headPattern, tailPattern)

  final def intPattern[A](attributes: A, value: Int): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.int(value))

  final def intPattern(value: Int): UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.int(value))

  final def literalPattern[A](attributes: A, value: Literal): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = value)

  final def literalPattern(value: Literal): UPattern =
    Pattern.LiteralPattern(attributes = (), literal = value)

  final def stringPattern[A](attributes: A, value: String): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.StringLiteral(value))

  final def stringPattern(value: String): UPattern =
    Pattern.LiteralPattern(attributes = (), literal = Literal.StringLiteral(value))

  final def truePattern[A](attributes: A): Pattern[A] =
    Pattern.LiteralPattern(attributes = attributes, literal = Literal.boolean(true))

  final def truePattern: UPattern =
    Pattern.LiteralPattern(attributes = DefaultAttributes, literal = Literal.boolean(true))

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
