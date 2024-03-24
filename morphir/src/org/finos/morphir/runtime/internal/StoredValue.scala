package org.finos.morphir.runtime.internal

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.ir.Value.TypedDefinition
import org.finos.morphir.naming.*

sealed trait StoredValue
object StoredValue {
  case class Eager(value: RTValue) extends StoredValue
  case class Lazy(
      toEvaluate: TypedDefinition,
      parentContext: CallStackFrame,
      siblings: Map[Name, TypedDefinition]
  ) extends StoredValue
}
