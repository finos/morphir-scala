package org.finos.morphir
package core
package internal

trait Visitor[TA, VA, -In, +Out] {
  def fail(index: Int): Out
  def visitName(value: Name, index: Int): Out
//  def visitPath(value: Array[Array[String]]): Out
//  def visitDist(): DistroVisitor[In, Out]
//
//  def visitType(): TypeVisitor[TA, In, Out]
//  def visitValue(): ValueVisitor[TA, VA, In, Out]
}

trait ValueVisitor[TA, VA, -In, +Out] { self => }

trait TypeVisitor[-Attribs, -In, +Out] { self =>
  def visitTuple(index: Int, attributes: Attribs): Out
  def visitUnit(index: Int, attributes: Attribs): Out
}

trait TupleTypeVisitor[Attribs, -In, +Out] {}

trait AccessControlledVisitor[-In, +Out] { self =>
  def visitPublic(index: Int): Out
  def visitPrivate(index: Int): Out
}

private[core] trait DistroVisitor[-In, +Out] { self =>
  def visitVersion(index: Int, version: MorphirVersion): Out
}
