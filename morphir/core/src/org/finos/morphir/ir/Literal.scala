package org.finos
package morphir
package ir

object Literal {
  sealed trait Literal extends Product with Serializable
  object Literal {
    final case class BoolLiteral(value: Boolean)       extends Literal
    final case class CharLiteral(value: Char)          extends Literal
    final case class StringLiteral(value: String)      extends Literal
    final case class WholeNumberLiteral(value: Long)   extends Literal
    final case class FloatLiteral(value: Double)       extends Literal
    final case class DecimalLiteral(value: BigDecimal) extends Literal
  }

  def boolLiteral(value: Boolean): Literal = Literal.BoolLiteral(value)
  def charLiteral(value: Char): Literal    = Literal.CharLiteral(value)

  def decimalLiteral(value: BigDecimal): Literal = Literal.DecimalLiteral(value)
  def stringLiteral(value: String): Literal =
    Literal.StringLiteral(value)

  def intLiteral(value: Int): Literal =
    Literal.WholeNumberLiteral(value.toLong)

  def longLiteral(value: Long): Literal =
    Literal.WholeNumberLiteral(value)

  def floatLiteral(value: Float): Literal =
    Literal.FloatLiteral(value.toDouble)

  def doubleLiteral(value: Double): Literal =
    Literal.FloatLiteral(value)
}
