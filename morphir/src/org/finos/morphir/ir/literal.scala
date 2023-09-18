package org.finos.morphir
package ir

import java.math.{BigDecimal => BigDec}
import scala.annotation.unused
import Type.UType
import Value.{RawValue, TypedValue, Value, literal}
import Value.{Definition => ValueDefinition}

object Literal { module =>

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
  def intLiteral(value: Int): Literal          = Literal.WholeNumberLiteral(value.toLong)
  def stringLiteral(value: String): Literal    = Literal.StringLiteral(value)
  def wholeNumberLiteral(value: Long): Literal = Literal.WholeNumberLiteral(value)

  final def toString(literal: Literal): String = literal.toString()

  final type Lit = module.Literal
  final lazy val Lit: Literal.type = module.Literal

  sealed trait Literal extends Product with Serializable { self =>
    import Literal._
    final def inferredType: UType  = InferredTypeOf[Literal].inferredType(self)
    final def toRawValue: RawValue = Value.Literal.Raw(self)

    final override def toString: String = self match {
      case BoolLiteral(true)         => "True"
      case BoolLiteral(false)        => "False"
      case CharLiteral(value)        => s"'$value'"
      case DecimalLiteral(value)     => s""""${value}M""""
      case FloatLiteral(value)       => value.toString()
      case StringLiteral(value)      => s""""$value""""
      case WholeNumberLiteral(value) => value.toString()
    }

    final def toTypedValue(implicit @unused ev: InferredTypeOf[Literal]): Value.TypedValue =
      literal(inferredType, self)

    final def toValueDef: ValueDefinition[Unit, UType] = ValueDefinition.fromLiteral(self)
  }
  object Literal {
    final lazy val False: TypedValue     = BoolLiteral(false).toTypedValue
    final lazy val True: TypedValue      = BoolLiteral(true).toTypedValue
    def boolean(value: Boolean): Literal = BoolLiteral(value)
    def char(value: Char): Literal       = CharLiteral(value)
    def decimal(value: BigDecimal): Literal =
      DecimalLiteral(value)
    def decimal(value: BigDec): Literal = DecimalLiteral(BigDecimal(value))
    def double(value: Double): Literal  = FloatLiteral(value)
    def float(value: Double): Literal   = FloatLiteral(value)
    def float(value: Float): Literal    = FloatLiteral(value.toDouble)
    def int(value: Int): Literal        = WholeNumberLiteral(value.toLong)
    def long(value: Long): Literal      = WholeNumberLiteral(value)
    def string(value: String): Literal  = StringLiteral(value)
    def wholeNumber(value: Long): Literal =
      WholeNumberLiteral(value)

    sealed case class BoolLiteral(value: Boolean)       extends Literal
    sealed case class CharLiteral(value: Char)          extends Literal
    sealed case class DecimalLiteral(value: BigDecimal) extends Literal
    sealed case class FloatLiteral(value: Double)       extends Literal
    sealed case class StringLiteral(value: String)      extends Literal
    sealed case class WholeNumberLiteral(value: Long)   extends Literal

    implicit val LiteralInferredTypeOf: InferredTypeOf[Literal] = new InferredTypeOf[Literal] {
      override def inferredType(value: Literal): UType = value match {
        case BoolLiteral(_)        => sdk.Basics.boolType
        case CharLiteral(_)        => sdk.Char.charType
        case DecimalLiteral(_)     => sdk.Decimal.decimalType
        case FloatLiteral(_)       => sdk.Basics.floatType
        case StringLiteral(_)      => sdk.String.stringType
        case WholeNumberLiteral(_) => sdk.Basics.intType
      }
    }

    class LiteralInterpolator(val sc: StringContext) extends AnyVal {
      import Literal._
      def lit(args: Any*): Literal = {
        val strings     = sc.parts.iterator
        val expressions = args.iterator
        val buf         = new StringBuilder(strings.next())
        while (strings.hasNext) {
          buf append expressions.next()
          buf append strings.next()
        }
        stringLiteral(buf.toString)
      }
    }
  }
}
