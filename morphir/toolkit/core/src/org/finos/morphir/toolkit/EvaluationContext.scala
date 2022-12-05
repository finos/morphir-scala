package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.{Type, UType}
import Value.Folder
import zio.{Tag, ZIO}
import zio.prelude._
import EvaluationContext._

final case class EvaluationContext[+TA, +VA](parent: Option[EvaluationContext[TA, VA]], variables: Variables[TA, VA]) {
  self =>
  def variable(name: Name): Option[VariableRef[TA, VA]] =
    variables.get(name) match {
      case None        => parent.flatMap(_.variable(name))
      case Some(value) => Some(value)
    }

}

object EvaluationContext {
  def root[TA, VA]: EvaluationContext[TA, VA] = EvaluationContext[TA, VA](parent = None, variables = Variables.empty)

  final case class Variables[+TA, +VA](variables: Map[Name, VariableRef[TA, VA]]) extends AnyVal {
    def apply(name: Name): VariableRef[TA, VA] = variables(name)

    def get(name: Name): Option[VariableRef[TA, VA]] = variables.get(name)
  }

  object Variables {
    def empty[TA, VA]: Variables[TA, VA] = Variables(Map.empty)
    // implicit class VariablesOps(val self: Variables) extends AnyVal {

    //   // def apply(name: Name): Option[Any] = unwrap(self).get(name).map(_.value)
    //   // def set[A](name: Name, value: Any, tpe: UType): Variables = wrap(
    //   //   unwrap(self) + (name -> VariableValue(value, tpe))
    //   // )

    //   // def get(name: Name): Option[Any]                = unwrap(self).get(name).map(_.value)
    //   // def variable(name: Name): Option[VariableValue] = unwrap(self).get(name)
    // }

  }

  sealed trait VariableRef[+TA, +VA] extends Any { self =>
    import VariableRef._

    def getValue[TA1 >: TA, VA1 >: VA](implicit
        taTag: Tag[TA1],
        vaTag: Tag[VA1]
    ): ZIO[Evaluator[TA1, VA1] with EvaluationContext[TA1, VA1], Throwable, Any] =
      self match {
        case Evaluated(value, tpe) => ZIO.succeed(value)
        case Suspended(value) =>
          for {
            evaluator <- ZIO.service[Evaluator[TA1, VA1]]
          } yield evaluator.evaluate(value)
      }
  }
  object VariableRef {
    final case class Evaluated[+TA](value: Any, tpe: Type[TA]) extends VariableRef[TA, Nothing]
    final case class Suspended[+TA, +VA](value: Value[TA, VA]) extends VariableRef[TA, VA]
  }
}
