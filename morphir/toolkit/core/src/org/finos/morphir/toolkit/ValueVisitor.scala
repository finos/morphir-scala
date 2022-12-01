package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO}

trait ValueVisitor[Context, TA, VA] extends Folder[Context, TA, VA, ZIO[Context, Throwable, Any]] { self =>
  def visit(value: Value[TA, VA])(implicit tag: Tag[Context]): ZIO[Context, Throwable, Any] =
    ZIO.environment[Context].flatMap { context =>
      value.foldContext(context.get)(self)
    }
}

object ValueVisitor {  
}
