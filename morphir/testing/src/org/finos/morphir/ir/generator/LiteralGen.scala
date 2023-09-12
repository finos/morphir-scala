package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Literal.Literal
import org.finos.morphir.testing.generators.WordGen
import zio.test.Gen

trait LiteralGen extends WordGen {
  final def asciiCharLiteral: Gen[Any, Literal.CharLiteral] = Gen.asciiChar.map(Literal.CharLiteral(_))

  final def boolLiteral: Gen[Any, Literal.BoolLiteral] = Gen.boolean.map(Literal.BoolLiteral(_))

  final def charLiteral: Gen[Any, Literal.CharLiteral] = Gen.char.map(Literal.CharLiteral(_))
  final def charLiteral(min: Char, max: Char): Gen[Any, Literal.CharLiteral] =
    Gen.char(min, max).map(Literal.CharLiteral(_))

  final def decimalLiteral(min: BigDecimal, max: BigDecimal): Gen[Any, Literal.DecimalLiteral] =
    Gen.bigDecimal(min, max).map(n => Literal.DecimalLiteral(n.bigDecimal))

  final def floatLiteral: Gen[Any, Literal.FloatLiteral] =
    Gen.double.map(n => Literal.FloatLiteral(n))
  final def floatLiteral(min: BigDecimal, max: BigDecimal): Gen[Any, Literal.FloatLiteral] =
    Gen.bigDecimal(min, max).map(n => Literal.FloatLiteral(n.doubleValue))

  final def literal: Gen[Any, Literal] = Gen.oneOf(
    boolLiteral,
    charLiteral('0', 'z'),
    decimalLiteral(BigDecimal(Long.MinValue), BigDecimal(Long.MaxValue)),
    stringLiteral,
    floatLiteral,
    wholeNumberLiteral
  )

  final def stringLiteral: Gen[Any, Literal.StringLiteral] = words.map(Literal.StringLiteral(_))

  final def wholeNumberLiteral(min: BigInt, max: BigInt): Gen[Any, Literal.WholeNumberLiteral] =
    Gen.bigInt(min, max).map(n => Literal.WholeNumberLiteral(n.longValue))
  final def wholeNumberLiteral(min: Long, max: Long): Gen[Any, Literal.WholeNumberLiteral] =
    Gen.long(min, max).map(n => Literal.WholeNumberLiteral(n))
  final def wholeNumberLiteral: Gen[Any, Literal.WholeNumberLiteral] =
    Gen.long.map(n => Literal.WholeNumberLiteral(n))
}

object LiteralGen extends LiteralGen
