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
  def floatLiteral(value: Double): Literal     = Literal.FloatLiteral(value)
  def floatLiteral(value: Float): Literal      = Literal.FloatLiteral(value.toDouble)
  def intLiteral(value: Int): Literal          = Literal.WholeNumberLiteral(value)
  def stringLiteral(value: String): Literal    = Literal.StringLiteral(value)
  def wholeNumberLiteral(value: Long): Literal = Literal.WholeNumberLiteral(value)

  final type Lit = module.Literal
  final lazy val Lit: Literal.type = module.Literal

  sealed trait Literal extends Product with Serializable {}
  object Literal {
    def boolean(value: Boolean): Literal = BoolLiteral(value)
    def char(value: Char): Literal       = CharLiteral(value)
    def decimal(value: BigDecimal): Literal =
      DecimalLiteral(value)
    def decimal(value: BigDec): Literal = DecimalLiteral(BigDecimal(value))
    def float(value: Double): Literal   = FloatLiteral(value)
    def float(value: Float): Literal    = FloatLiteral(value.toDouble)
    def int(value: Int): Literal        = WholeNumberLiteral(value)
    def string(value: String): Literal  = StringLiteral(value)
    def wholeNumber(value: Long): Literal =
      WholeNumberLiteral(value)

    sealed case class BoolLiteral(value: Boolean)       extends Literal
    sealed case class CharLiteral(value: Char)          extends Literal
    sealed case class DecimalLiteral(value: BigDecimal) extends Literal
    sealed case class FloatLiteral(value: Double)       extends Literal
    sealed case class StringLiteral(value: String)      extends Literal
    sealed case class WholeNumberLiteral(value: Long)   extends Literal
  }
}
