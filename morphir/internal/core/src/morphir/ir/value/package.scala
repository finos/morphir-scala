package morphir.mir

import morphir.mir.Type.UType

package object value {

  type RawValue = morphir.mir.value.recursive.Value.RawValue
  val RawValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value

  type TypedValue = morphir.mir.value.recursive.Value.TypedValue
  val TypedValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value.TypedValue

  type UDefinition = morphir.mir.value.recursive.Definition[Any, Any]
  val UDefinition: morphir.mir.value.recursive.Definition.type = morphir.mir.value.recursive.Definition

  type UPattern = morphir.mir.value.Pattern.UPattern
  val UPattern: Pattern.type = morphir.mir.value.Pattern.UPattern

  type USpecification = Specification[Any]
  val USpecification: Specification.type = Specification

}
