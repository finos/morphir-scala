package org.finos.morphir.core.internal
import org.finos.morphir.core.Name

trait SimpleVisitor[TA, VA, -In, +Out] extends Visitor[TA, VA, In, Out] {
  def expectedMsg: String

  override def visitName(value: Name, index: Int): Out = ???
}
