package org.finos.morphir.ir.internal
import java.math.{BigDecimal => BigDec}

trait LiteralModule { module =>

  /**
   * Represents a boolean value. The only possible values are `true` and `false`.
   */
  def boolLiteral(value: Boolean): Literal = Literal.BoolLiteral(value)

  /**
   * Represents a character value. Some possible values: `'a'`, `'b'`, `'3'`.
   */
  def charLiteral(value: Char): Literal = Literal.CharLiteral(value)
  def decimalLiteral(value: BigDecimal): Literal =
    Literal.DecimalLiteral(value)
  def decimalLiteral(value: BigDec): Literal =
    Literal.DecimalLiteral(BigDecimal(value))
  def floatLiteral(value: Double): Literal       = Literal.FloatLiteral(value)
  def floatLiteral(value: Float): Literal        = Literal.FloatLiteral(value.toDouble)
  def intLiteral(value: Int): Literal            = Literal.WholeNumberLiteral(BigInt(value))
  def stringLiteral(value: String): Literal      = Literal.StringLiteral(value)
  def wholeNumberLiteral(value: BigInt): Literal = Literal.WholeNumberLiteral(value)

  sealed trait Literal extends Product with Serializable
  object Literal {
    sealed case class BoolLiteral(value: Boolean)       extends Literal
    sealed case class CharLiteral(value: Char)          extends Literal
    sealed case class DecimalLiteral(value: BigDecimal) extends Literal
    sealed case class FloatLiteral(value: Double)       extends Literal
    sealed case class StringLiteral(value: String)      extends Literal
    sealed case class WholeNumberLiteral(value: BigInt) extends Literal
  }
}
