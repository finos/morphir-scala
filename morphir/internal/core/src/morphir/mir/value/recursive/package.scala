package org.finos
package morphir
package mir
package value

package object recursive {
  type RawValue = morphir.mir.value.recursive.Value.RawValue
  val RawValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value.RawValue

  type TypedValue = morphir.mir.value.recursive.Value.TypedValue
  val TypedValue: morphir.mir.value.recursive.Value.type = morphir.mir.value.recursive.Value.TypedValue
}
