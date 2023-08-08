package org.finos.morphir
package ir
package generator

import org.finos.morphir.naming._
import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.ir.Value.Pattern
import zio._
import zio.test.Gen

trait PatternGen {
  final def asPattern[R, A](
      attributesGen: Gen[R, A],
      patternGen: Gen[R, Pattern[A]],
      nameGen: Gen[R, Name]
  ): Gen[R, Pattern.AsPattern[A]] =
    for {
      attributes <- attributesGen
      pattern    <- patternGen
      name       <- nameGen
    } yield Pattern.AsPattern(attributes, pattern, name)

  final def asPatternFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Pattern.AsPattern[A]] =
    asPattern(attributes, pattern(attributes), NameGen.name)

  final def constructorPattern[R, A](
      attributesGen: Gen[R, A],
      constructorNameGen: Gen[R, FQName],
      argumentPatternsGen: Gen[R, Chunk[Pattern[A]]]
  ): Gen[R, Pattern.ConstructorPattern[A]] =
    for {
      attributes       <- attributesGen
      constructorName  <- constructorNameGen
      argumentPatterns <- argumentPatternsGen
    } yield Pattern.ConstructorPattern(attributes, constructorName, argumentPatterns)

  final def constructorPatternFromAttributes[R, A](implicit
      attributes: Gen[R, A]
  ): Gen[R, Pattern.ConstructorPattern[A]] =
    constructorPattern(attributes, FQNameGen.fqName, chunkOfPatternsGen)

  final def emptyListPattern[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Pattern.EmptyListPattern[A]] =
    for {
      attributes <- attributesGen
    } yield Pattern.EmptyListPattern(attributes)

  final def headTailPattern[R, A](
      attributesGen: Gen[R, A],
      headPatternGen: Gen[R, Pattern[A]],
      tailPatternGen: Gen[R, Pattern[A]]
  ): Gen[R, Pattern.HeadTailPattern[A]] =
    for {
      attributes  <- attributesGen
      headPattern <- headPatternGen
      tailPattern <- tailPatternGen
    } yield Pattern.HeadTailPattern(attributes, headPattern, tailPattern)

  final def headTailPatternFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Pattern.HeadTailPattern[A]] =
    headTailPattern(attributes, pattern(attributes), pattern(attributes))

  final def literalPattern[R, A](
      attributesGen: Gen[R, A],
      literalGen: Gen[R, Literal]
  ): Gen[R, Pattern.LiteralPattern[A]] =
    for {
      attributes <- attributesGen
      literal    <- literalGen
    } yield Pattern.LiteralPattern(attributes, literal)

  final def literalPatternFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Pattern.LiteralPattern[A]] =
    literalPattern(attributes, LiteralGen.literal)

  final def tuplePattern[R, A](
      attributesGen: Gen[R, A],
      elementPatternsGen: Gen[R, Chunk[Pattern[A]]]
  ): Gen[R, Pattern.TuplePattern[A]] = for {
    attributes      <- attributesGen
    elementPatterns <- elementPatternsGen
  } yield Pattern.TuplePattern(attributes, elementPatterns)

  final def tuplePatternFromAttributes[R, A](implicit attributes: Gen[R, A]): Gen[R, Pattern.TuplePattern[A]] =
    tuplePattern(attributes, chunkOfPatternsGen)

  final def unitPattern[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Pattern.UnitPattern[A]] =
    for {
      attributes <- attributesGen
    } yield Pattern.UnitPattern(attributes)

  final def wildcardPattern[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Pattern.WildcardPattern[A]] =
    for {
      attributes <- attributesGen
    } yield Pattern.WildcardPattern(attributes)

  final def pattern[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Pattern[A]] =
    Gen.suspend(
      Gen.oneOf(
        asPatternFromAttributes,
        constructorPatternFromAttributes,
        emptyListPattern,
        headTailPatternFromAttributes,
        literalPatternFromAttributes,
        tuplePatternFromAttributes,
        unitPattern,
        wildcardPattern
      )
    )

  private final def chunkOfPatternsGen[R, A](implicit attributesGen: Gen[R, A]): Gen[R, Chunk[Pattern[A]]] =
    Gen.chunkOfBounded(1, 2)(pattern)
}

object PatternGen extends PatternGen
