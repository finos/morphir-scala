package org.finos.morphir
package toolkit
package runtime

final case class CallStack(frames: List[StackFrame]) { self =>
  def depth: Int                         = frames.size
  def isEmpty: Boolean                   = frames.isEmpty
  def push(frame: StackFrame): CallStack = CallStack(frame :: frames)
  def peek: StackFrame                   = frames.head
  def peekOption: Option[StackFrame]     = frames.headOption
  def pop: CallStack                     = CallStack(self.frames.tail)
  def size: Int                          = frames.size
}

object CallStack {
  val empty: CallStack = CallStack(Nil)
}

final case class StackFrame(symbols: SymbolTable)
object StackFrame {
  val empty: StackFrame = StackFrame(SymbolTable.empty)
}

final case class SymbolTable()
object SymbolTable {
  val empty: SymbolTable = SymbolTable()
}

sealed trait Symbol
object Symbol {
  final case class Var(name: Name) extends Symbol
}
