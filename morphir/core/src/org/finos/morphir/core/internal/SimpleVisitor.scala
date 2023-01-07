package org.finos.morphir.core.internal

import org.finos.morphir.core.types.Name

trait SimpleVisitor[TA, VA, -In, +Out] extends Visitor[TA, VA, In, Out] {
  def expectedMsg: String
  def visitName(value: Name, index: Int): Out =
    throw new Abort(expectedMsg + "got name")

  def visitNull(index: Int): Out = null.asInstanceOf[Out]
}
