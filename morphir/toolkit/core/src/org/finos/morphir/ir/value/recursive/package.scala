package org.finos
package morphir
package mir
package value

package object recursive {
  type RawValue = morphir.ir.value.recursive.Value.RawValue
  val RawValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value.RawValue

  type TypedValue = morphir.ir.value.recursive.Value.TypedValue
  val TypedValue: morphir.ir.value.recursive.Value.type = morphir.ir.value.recursive.Value.TypedValue
}
