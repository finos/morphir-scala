package org.finos.morphir
package toolkit
package runtime

import org.finos.morphir.ir.FQName
import zio.Tag

sealed trait Symbol { self =>
  //@compat.targetName("append")
  def :=[A](value: A)(implicit ev: Not[A =:= SymbolValue], tagged: Tag[A]): SymbolBinding =
    SymbolBinding(self, SymbolValue.Typed(value, tagged))

  //@compat.targetName("append")
  def :=(value: SymbolValue): SymbolBinding = SymbolBinding(self, value)
}
object Symbol {
  def variable(name: Name): Symbol   = Var(name)
  def variable(name: String): Symbol = Var(Name.fromString(name))

  final case class Var(name: Name) extends Symbol {
    override def toString: String = name.toCamelCase // TODO: We should perhaps a concept of a canonical case
  }
}

sealed trait SymbolValue {
  type Type
}
object SymbolValue {
  def typed[A](value: A)(implicit tagged: Tag[A]): SymbolValue = Typed(value, tagged)
  final case class Typed[A](value: A, tagged: Tag[A]) extends SymbolValue {
    type Type = A
  }
}

final case class SymbolBinding(symbol: Symbol, value: SymbolValue)
