package org.finos
package morphir
package core
package internal

import morphir.core.types.Versioning.MorphirVersion

trait Visitor[TA, VA, -In, +Out]:
  def visitName(value: CharSequence, index: Int): Out
  def visitNull(index: Int): Out
//  def visitPath(value: Array[Array[String]]): Out
//  def visitDist(): DistroVisitor[In, Out]
//
//  def visitType(): TypeVisitor[TA, In, Out]
//  def visitValue(): ValueVisitor[TA, VA, In, Out]
  def map[Z](f: Out => Z): Visitor[TA, VA, In, Z] = new Visitor.MapReader[TA, VA, In, Out, Z](Visitor.this):
    def mapNonNullsFunction(v: Out): Z = f(v)

end Visitor

object Visitor:
  abstract class MapReader[TA, VA, -In, V, Z](delegatedReader: Visitor[TA, VA, In, V]) extends Visitor[TA, VA, In, Z]:

    def mapNonNullsFunction(v: V): Z
    def mapFunction(v: V): Z =
      if (v == null) then null.asInstanceOf[Z]
      else mapNonNullsFunction(v)

    override def visitName(value: CharSequence, index: Int): Z =
      mapFunction(delegatedReader.visitName(value, index))

    override def visitNull(index: Int) = mapFunction(delegatedReader.visitNull(index))

  end MapReader

end Visitor

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

/**
 * Signals failure processsing JSON after parsing.
 */
case class AbortException(clue: String, index: Int, line: Int, col: Int, cause: Throwable)
    extends Exception(clue + " at index " + index, cause)

/**
 * Throw this inside a [[Visitor]]'s handler functions to fail the processing of the model. The Facade just needs to
 * provide the error message, and it is up to the driver to ensure it is properly wrapped in a [[AbortException]] with
 * the relevant source information.
 */
case class Abort(msg: String) extends Exception(msg)
