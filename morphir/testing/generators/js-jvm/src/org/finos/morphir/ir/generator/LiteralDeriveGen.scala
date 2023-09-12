package org.finos.morphir
package ir
package generator

import org.finos.morphir.ir.Literal.Literal
import zio.test.magnolia.DeriveGen
import zio.test.magnolia.DeriveGen._

trait LiteralDeriveGen {
  implicit val boolLiteralDeriveGen: DeriveGen[Literal.BoolLiteral] = DeriveGen.instance(LiteralGen.boolLiteral)

  implicit val charLiteralDeriveGen: DeriveGen[Literal.CharLiteral] =
    DeriveGen.instance(LiteralGen.charLiteral('A', 'Z'))

  implicit val decimalLiteralDeriveGen: DeriveGen[Literal.DecimalLiteral] =
    DeriveGen.instance(LiteralGen.decimalLiteral(BigDecimal(Long.MinValue), BigDecimal(Long.MaxValue)))

  implicit val floatLiteralDeriveGen: DeriveGen[Literal.FloatLiteral] = DeriveGen.instance(LiteralGen.floatLiteral)

  implicit val stringLiteralDeriveGen: DeriveGen[Literal.StringLiteral] = DeriveGen.instance(LiteralGen.stringLiteral)

  implicit val wholeNumberLiteralDeriveGen: DeriveGen[Literal.WholeNumberLiteral] =
    DeriveGen.instance(LiteralGen.wholeNumberLiteral)

  implicit val literalDeriveGen: DeriveGen[Literal] = DeriveGen.instance(LiteralGen.literal)
}

object LiteralDeriveGen extends LiteralDeriveGen
