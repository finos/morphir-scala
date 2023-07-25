package org.finos.morphir
import zio.prelude.fx.*
package object toolkit {

  type Documented[+A] = ir.Documented[A]
  val Documented = ir.Documented

  type FQName = ir.FQName
  val FQName = ir.FQName

  type Name = ir.Name
  val Name = ir.Name

  type MorphirType = ir.Type.Type[Attributes]

  type UType = ir.Type.UType
  val UType = ir.Type.Type

  // type TypedValueVisitor[Context] = ValueVisitor[Context, scala.Unit, MorphirType]

}
