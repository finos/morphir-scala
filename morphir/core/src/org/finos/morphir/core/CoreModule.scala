package org.finos.morphir
package core

trait CoreModule {
  type TypeAttribs
  type ValueAttribs

  type DistroVisitor[-In, +Out] = internal.DistroVisitor[In, Out]
  // val DistorVisitor = internal.DistroVisitor

  type SimpleVisitor[-In, +Out] = internal.SimpleVisitor[TypeAttribs, ValueAttribs, In, Out]

  type Visitor[-In, +Out] = internal.Visitor[TypeAttribs, ValueAttribs, In, Out]
  val Visitor: internal.Visitor.type = internal.Visitor
}
