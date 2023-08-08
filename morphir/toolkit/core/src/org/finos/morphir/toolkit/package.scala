package org.finos.morphir
import zio.prelude.fx.*
package object toolkit {

  type Documented[+A] = ir.Documented[A]
  val Documented = ir.Documented

  type MorphirType = ir.Type.Type[Attributes]

  type UType = ir.Type.UType
  val UType = ir.Type.Type

  // type TypedValueVisitor[Context] = ValueVisitor[Context, scala.Unit, MorphirType]

}
