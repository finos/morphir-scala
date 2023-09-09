package org.finos.morphir
package runtime

import org.finos.morphir.naming._
import org.typelevel.scalaccompat.annotation.{targetName3 => targetName}

final case class StackFrame(symbols: SymbolTable) { self =>
  def ++(frame: StackFrame): StackFrame = copy(symbols = self.symbols ++ frame.symbols)
  @targetName("append")
  def +(binding: SymbolBinding): StackFrame = copy(symbols = symbols + binding)
  @targetName("append")
  def ++=(bindings: Seq[SymbolBinding]): StackFrame = copy(symbols = symbols ++= bindings)

  def apply(symbol: Symbol): SymbolValue           = symbols(symbol)
  def get(symbol: Symbol): Option[SymbolValue]     = symbols.get(symbol)
  def update(bindings: SymbolBinding*): StackFrame = copy(symbols = symbols ++= bindings)
}

object StackFrame {
  val empty: StackFrame = StackFrame(SymbolTable.empty)
  def create(bindings: SymbolBinding*): StackFrame =
    StackFrame(symbols = SymbolTable.create(bindings: _*))
}
