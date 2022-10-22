package org.finos.morphir

package object toolkit {

  type Documented[+A] = ir.Documented[A]
  val Documented = ir.Documented

  type FQName = ir.FQName
  val FQName = ir.FQName

  type Name = ir.Name
  val Name = ir.Name

  type MorphirType = ir.Type.Type[Attributes]
}
