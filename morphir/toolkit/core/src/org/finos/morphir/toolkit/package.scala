package org.finos.morphir
import zio.ZState

import zio.prelude.fx._
package object toolkit {

  type Documented[+A] = ir.Documented[A]
  val Documented = ir.Documented

  type FQName = ir.FQName
  val FQName = ir.FQName

  type Name = ir.Name
  val Name = ir.Name

  type MorphirType = ir.Type.Type[Attributes]

  type UType = ir.Type.UType
  val UType = ir.Type.Type

  type ZEvaluationContext[TA, VA] = ZState[EvaluationContext[TA, VA]]
  // type TypedValueVisitor[Context] = ValueVisitor[Context, scala.Unit, MorphirType]

  type StepCompanion

  type Step[TA, VA, A] = ZStep[Any, TA, VA, A]
  val Step = ZPure.asInstanceOf[ZPure.type with StepCompanion]

  type ZStep[-R, TA, VA, A] =
    ZPure[EngineEvent, EvaluationEngine.Context[TA, VA], EvaluationEngine.Context[TA, VA], R, EvaluationError, A]
  val ZStep = ZPure.asInstanceOf[ZPure.type with StepCompanion]

  implicit class ZStepCompanionOps(val self: ZPure.type with StepCompanion) extends AnyVal {
    import EvaluationEngine._

    def scoped[R, TA,VA,A](bindings:VarBinding*)(block : => ZStep[R, TA,VA,A]):ZStep[R, TA,VA,A] = 
      for {
        originalContext <- ZStep.get[Context[TA,VA]]
        result <- ZStep.set(originalContext)

      } yield ???
  }
}
