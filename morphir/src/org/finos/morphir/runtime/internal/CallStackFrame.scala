package org.finos.morphir.runtime.internal

import org.finos.morphir.naming.*

final case class CallStackFrame(
    bindings: Map[Name, StoredValue],
    parent: Option[CallStackFrame]
) {
  def get(name: Name): Option[StoredValue] =
    (bindings.get(name), parent) match {
      case (Some(res), _)            => Some(res)
      case (None, Some(parentFrame)) => parentFrame.get(name)
      case (None, None)              => None
    }
  def push(bindings: Map[Name, StoredValue]): CallStackFrame =
    CallStackFrame(bindings, Some(this))
}
