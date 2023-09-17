package org.finos.morphir.runtime.internal

import org.finos.morphir.runtime.RTValue
import org.finos.morphir.runtime.TypedMorphirRuntimeDefs.RuntimeDefinition
import org.finos.morphir.naming.*

sealed trait StoredValue
object StoredValue {
  case class Eager(value: RTValue) extends StoredValue
  case class Lazy(
      toEvaluate: RuntimeDefinition,
      parentContext: CallStackFrame,
      siblings: Map[Name, RuntimeDefinition]
  ) extends StoredValue
}
