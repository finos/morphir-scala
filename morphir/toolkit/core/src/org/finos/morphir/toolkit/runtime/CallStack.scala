package org.finos.morphir
package toolkit
package runtime

import zio.Tag
import org.finos.morphir.ir.FQName

import scala.annotation.tailrec

final case class CallStack(resolver: Resolver, frames: List[StackFrame]) { self =>
  def depth: Int                         = frames.size
  def isEmpty: Boolean                   = frames.isEmpty
  def push(frame: StackFrame): CallStack = copy(frames = frame :: frames)
  def push(bindings: SymbolBinding*): CallStack = {
    val newFrame = StackFrame.create(bindings: _*)
    copy(frames = newFrame :: self.frames)
  }
  def peek: StackFrame               = frames.head
  def peekOption: Option[StackFrame] = frames.headOption
  def pop: (CallStack, Option[StackFrame]) = self.frames match {
    case Nil           => self                -> None
    case frame :: tail => copy(frames = tail) -> Some(frame)
  }
  def popStack: Option[CallStack] = self.frames match {
    case Nil       => None
    case _ :: tail => Some(copy(frames = tail))
  }
  def size: Int = frames.size

  def get(symbol: Symbol): Option[SymbolValue] = {
    @tailrec
    def loop(maybeStack: Option[CallStack]): Option[SymbolValue] = maybeStack match {
      case None => None
      case Some(stack) =>
        stack.peekOption match {
          case None =>
            // The CallStack has no StackFrames so it can't contain any Symbols
            None
          case Some(frame) =>
            // Look for the Symbol in the current StackFrame
            frame.get(symbol) match {
              case None =>
                // The symbol is not in the frame so check "up" the stack
                loop(stack.popStack)
              case v => v
            }
        }
    }
    loop(Some(self))
  }
  def getLocal(symbol: Symbol): Option[SymbolValue] =
    peekOption.flatMap { frame =>
      frame.get(symbol)
    }
}

object CallStack {
  val empty: CallStack = CallStack(resolver = Resolver.default, frames = Nil)
}
