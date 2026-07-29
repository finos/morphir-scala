package org.finos.morphir.codemodel

import kyo.Schema

enum Purity derives Schema {
  case Pure
  case Effectful
  case Unknown
}

final case class ValueProperties(
    isConstant: Boolean,
    purity: Purity
) derives Schema

object ValueProperties {
  val default: ValueProperties = ValueProperties(isConstant = false, purity = Purity.Unknown)
}
