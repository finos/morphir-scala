package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Value.Pattern
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait PatternDeriveGen {
  implicit def asPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.AsPattern[A]] =
    DeriveGen.instance(PatternGen.asPatternFromAttributes(DeriveGen[A]))

  implicit def constructorPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.ConstructorPattern[A]] =
    DeriveGen.instance(PatternGen.constructorPatternFromAttributes(DeriveGen[A]))

  implicit def emptyListPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.EmptyListPattern[A]] =
    DeriveGen.instance(PatternGen.emptyListPattern(DeriveGen[A]))

  implicit def headTailPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.HeadTailPattern[A]] =
    DeriveGen.instance(PatternGen.headTailPatternFromAttributes(DeriveGen[A]))

  implicit def literalPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.LiteralPattern[A]] =
    DeriveGen.instance(PatternGen.literalPatternFromAttributes(DeriveGen[A]))

  implicit def tuplePatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.TuplePattern[A]] =
    DeriveGen.instance(PatternGen.tuplePatternFromAttributes(DeriveGen[A]))

  implicit def unitPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.UnitPattern[A]] =
    DeriveGen.instance(PatternGen.unitPattern(DeriveGen[A]))

  implicit def wildcardPatternDeriveGen[A: DeriveGen]: DeriveGen[Pattern.WildcardPattern[A]] =
    DeriveGen.instance(PatternGen.wildcardPattern(DeriveGen[A]))

  implicit def patternDeriveGen[A: DeriveGen]: DeriveGen[Pattern[A]] =
    DeriveGen.instance(PatternGen.pattern(DeriveGen[A]))
}

object PatternDeriveGen extends PatternDeriveGen
