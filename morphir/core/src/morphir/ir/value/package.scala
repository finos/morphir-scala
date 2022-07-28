package morphir.ir

import morphir.ir.Type.UType

package object value {

  type RawValue = morphir.ir.value.recursive.Value.RawValue
  val RawValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value

  type TypedValue = morphir.ir.value.recursive.Value.TypedValue
  val TypedValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value.TypedValue

  type UDefinition = morphir.ir.value.recursive.Definition[Any, Any]
  val UDefinition: morphir.ir.value.recursive.Definition.type = morphir.ir.value.recursive.Definition

  type UPattern = morphir.ir.value.Pattern.UPattern
  val UPattern: Pattern.type = morphir.ir.value.Pattern.UPattern

  type USpecification = Specification[Any]
  val USpecification: Specification.type = Specification

}
