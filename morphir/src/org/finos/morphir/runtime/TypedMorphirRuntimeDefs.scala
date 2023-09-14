package org.finos.morphir.runtime

import org.finos.morphir.ir.Type.UType
import org.finos.morphir.ir.Value.Value

object TypedMorphirRuntimeDefs {
  final type TypeAttribs  = scala.Unit
  final type ValueAttribs = UType
  type RuntimeValue       = Value[TypeAttribs, ValueAttribs]
  type RuntimeDefinition  = Value.Definition[TypeAttribs, ValueAttribs]
}
