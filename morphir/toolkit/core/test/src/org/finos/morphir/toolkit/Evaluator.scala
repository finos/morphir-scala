package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO}

trait Evaluator[-Context, +TA, +VA] {
  def evaluate[TA1 >: TA, VA1 >: VA](value: Value[TA1, VA1]): ZIO[Context, Throwable, Any]
}
object Evaluator {
  def forTyped[Context](visitor: ValueVisitor[Context, scala.Unit, UType]): Evaluator[Context, scala.Unit, UType] = ???
}

trait ValueVisitor[Context, TA, VA] extends Folder[Context, TA, VA, ZIO[Context, Throwable, Any]] { self =>
  def visit(value: Value[TA, VA])(implicit tag: Tag[Context]): ZIO[Context, Throwable, Any] =
    ZIO.environment[Context].flatMap { context =>
      value.foldContext(context.get)(self)
    }
}
