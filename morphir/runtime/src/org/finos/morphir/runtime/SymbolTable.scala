package org.finos.morphir
package runtime

import org.finos.morphir.naming._
import scala.annotation.unused
import zio.Tag
import org.typelevel.scalaccompat.annotation.{targetName3 => targetName}
final case class SymbolTable private (bindings: Map[Symbol, SymbolValue]) { self =>
  @targetName("append")
  def ++(symbols: SymbolTable): SymbolTable = copy(bindings = bindings ++ symbols.bindings)
  @targetName("append")
  def +(binding: SymbolBinding): SymbolTable = copy(bindings = bindings + (binding.symbol -> binding.value))
  @targetName("append")
  def ++=(newBindings: Seq[SymbolBinding]): SymbolTable = {
    val finalBindings = newBindings.foldLeft(bindings)((acc, binding) => acc + (binding.symbol -> binding.value))
    copy(bindings = finalBindings)
  }

  def apply(symbol: Symbol): SymbolValue = bindings(symbol)

  def bind[A](symbol: Symbol, value: A)(implicit @unused ev: Not[A =:= SymbolValue], tagged: Tag[A]): SymbolTable =
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
