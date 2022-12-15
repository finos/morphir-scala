package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO}

trait Evaluator[TA, VA] {
  final type Ctx = EvaluationContext[TA, VA]

  def evaluate(value: Value[TA, VA]): ZIO[Ctx, Throwable, Any]
}

object Evaluator {
  type TypedEvaluationContext = EvaluationContext[scala.Unit, UType]

  def forTyped(
      visitor: ValueVisitor[TypedEvaluationContext, scala.Unit, UType]
  ): Evaluator[scala.Unit, UType] = new Evaluator[scala.Unit, UType] {

    def evaluate(value: TypedValue): ZIO[Ctx, Throwable, Any] =
      ZIO.environment[Ctx].flatMap { context =>
        value.foldContext(context.get)(visitor)
      }
  }
}
