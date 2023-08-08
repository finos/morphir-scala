package org.finos.morphir
package runtime

import org.finos.morphir.naming._

final case class StackFrame(symbols: SymbolTable) { self =>
  def ++(frame: StackFrame): StackFrame = copy(symbols = self.symbols ++ frame.symbols)
  @compat.targetName("append")
  def +(binding: SymbolBinding): StackFrame = copy(symbols = symbols + binding)
  @compat.targetName("append")
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
