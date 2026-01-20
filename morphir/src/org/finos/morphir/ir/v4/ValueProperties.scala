package org.finos.morphir.ir.v4

enum Purity {
  case Pure
  case Effectful
  case Unknown
}

final case class ValueProperties(
    isConstant: Boolean,
    purity: Purity
)

object ValueProperties {
  val default: ValueProperties = ValueProperties(isConstant = false, purity = Purity.Unknown)
}
