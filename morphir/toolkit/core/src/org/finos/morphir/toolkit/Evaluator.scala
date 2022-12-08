package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import zio.{Tag, ZIO, ZState}

trait Evaluator[TA, VA] extends Folder[scala.Unit, TA, VA, ZIO[ZEvaluationContext[TA, VA], EvaluationError, Any]] {
  self =>

  def evaluate(value: Value[TA, VA])(implicit
      typeAttributeTag: Tag[TA],
      valueAttributeTag: Tag[VA]
  ): ZIO[ZEvaluationContext[TA, VA], EvaluationError, Any] =
   ZIO.serviceWithZIO { context =>
      value.foldContext(())(self)
    }
}

object Evaluator {

  def evaluate[TA: Tag, VA: Tag](
      value: Value[TA, VA]
  ): ZIO[Evaluator[TA, VA] with ZEvaluationContext[TA, VA], EvaluationError, Any] =
    ZIO.serviceWithZIO[Evaluator[TA, VA]] { evaluator =>
      evaluator.evaluate(value)
    }
  // def make[TA,VA](initialContext:EvaluationContext[TA,VA]) = ZIO.succeed()

  type Typed = Evaluator[scala.Unit, UType]
  object Typed {
    type Ctx = EvaluationContext[scala.Unit, UType]
    val Ctx            = EvaluationContext.Typed
    def apply(): Typed = new TypedValueEvaluator
  }
}
