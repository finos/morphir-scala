package org.finos.morphir
package runtime

import org.finos.morphir.ir.FQName
import zio.Tag

final case class SymbolTable private (bindings: Map[Symbol, SymbolValue]) { self =>
  @compat.targetName("append")
  def ++(symbols: SymbolTable): SymbolTable = copy(bindings = bindings ++ symbols.bindings)
  @compat.targetName("append")
  def +(binding: SymbolBinding): SymbolTable = copy(bindings = bindings + (binding.symbol -> binding.value))
  @compat.targetName("append")
  def ++=(newBindings: Seq[SymbolBinding]): SymbolTable = {
    val finalBindings = newBindings.foldLeft(bindings)((acc, binding) => acc + (binding.symbol -> binding.value))
    copy(bindings = finalBindings)
  }

  def apply(symbol: Symbol): SymbolValue = bindings(symbol)

  def bind[A](symbol: Symbol, value: A)(implicit ev: Not[A =:= SymbolValue], tagged: Tag[A]): SymbolTable =
    copy(bindings = self.bindings + (symbol -> SymbolValue.typed(value)))

  def get(symbol: Symbol): Option[SymbolValue] = bindings.get(symbol)

  def size: Int = bindings.size

  def update(bindings: SymbolBinding*): SymbolTable = self ++= bindings
}

object SymbolTable {
  val empty: SymbolTable = SymbolTable(Map.empty)
  def create(bindings: SymbolBinding*): SymbolTable = {
    val data = bindings.map(binding => binding.symbol -> binding.value).toMap
    SymbolTable(data)
  }
}



