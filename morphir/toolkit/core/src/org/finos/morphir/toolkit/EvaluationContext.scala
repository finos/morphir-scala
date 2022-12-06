package org.finos.morphir
package toolkit

import ir.Value.{TypedValue, Value}
import ir.Type.{Type, UType}
import Value.Folder
import zio._
import zio.prelude._
import EvaluationContext._
import scala.annotation.tailrec

final case class EvaluationContext[+TA, +VA](parent: Option[EvaluationContext[TA, VA]], variables: Variables[TA, VA]) {
  self =>
  def allVariables: Chunk[VariableRef[TA, VA]] = {
    @tailrec
    def loop(context: EvaluationContext[TA, VA], acc: Chunk[VariableRef[TA, VA]]): Chunk[VariableRef[TA, VA]] =
      context.parent match {
        case Some(parent) => loop(parent, acc ++ context.variables.getAll)
        case None         => acc ++ context.variables.getAll
      }
    loop(self, Chunk.empty)
  }

  def localVaribles: Chunk[(Name, VariableRef[TA, VA])] = Chunk.fromIterable(variables.variables)

  def variable(name: Name): Option[VariableRef[TA, VA]] =
    variables.get(name) match {
      case None        => parent.flatMap(_.variable(name))
      case Some(value) => Some(value)
    }

  def pushFrame[TA1 >: TA, VA1 >: VA](vars: Variables[TA1, VA1]): EvaluationContext[TA1, VA1] =
    EvaluationContext(parent = Some(self), variables = self.variables ++ vars)

  def popFrame: EvaluationContext[TA, VA] = self match {
    case EvaluationContext(Some(parent), _) => parent
    case _                                  => self
  }

}

object EvaluationContext {

  def localVaribles[TA: Tag, VA: Tag]
      : ZIO[ZState[EvaluationContext[TA, VA]], Nothing, Chunk[(Name, VariableRef[TA, VA])]] =
    ZIO.getStateWith[EvaluationContext[TA, VA]](_.localVaribles)

  def make[TA, VA](variables: Variables[TA, VA]): UIO[EvaluationContext[TA, VA]] =
    ZIO.succeed(EvaluationContext(None, variables))

  def pushFrame[TA: Tag, VA: Tag](variables: Variables[TA, VA]): ZIO[ZState[EvaluationContext[TA, VA]], Nothing, Unit] =
    ZIO.updateState[EvaluationContext[TA, VA]](_.pushFrame(variables))

  def popFrame[TA: Tag, VA: Tag]: ZIO[ZState[EvaluationContext[TA, VA]], Nothing, Unit] =
    ZIO.updateState[EvaluationContext[TA, VA]](_.popFrame)

  def root[TA, VA]: EvaluationContext[TA, VA] = EvaluationContext[TA, VA](parent = None, variables = Variables.empty)
  def root[TA, VA](variables: Variables[TA, VA]): EvaluationContext[TA, VA] =
    EvaluationContext[TA, VA](parent = None, variables = variables)

  type Typed = EvaluationContext[scala.Unit, UType]
  object Typed {

    def localVaribles: ZIO[ZState[Typed], Nothing, Chunk[(Name, VariableRef[Unit, UType])]] =
      ZIO.getStateWith[Typed](_.localVaribles)

    def popFrame: ZIO[ZState[Typed], Nothing, Unit] =
      ZIO.updateState[Typed](_.popFrame)

    def pushFrame(variables: Variables.Typed): ZIO[ZState[Typed], Nothing, Unit] =
      ZIO.updateState[Typed](_.pushFrame(variables))

    def root: Typed                             = EvaluationContext.root[scala.Unit, UType]
    def root(variables: Variables.Typed): Typed = EvaluationContext.root(variables)
  }

  final case class Variables[+TA, +VA](variables: Map[Name, VariableRef[TA, VA]]) extends AnyVal { self =>
    def ++[TA1 >: TA, VA1 >: VA](other: Variables[TA1, VA1]): Variables[TA1, VA1] =
      Variables(self.variables ++ other.variables)

    def apply(name: Name): VariableRef[TA, VA] = variables(name)

    def get(name: Name): Option[VariableRef[TA, VA]] = variables.get(name)
    def getAll: Chunk[VariableRef[TA, VA]]           = variables.values.toChunk

    def set[TA1 >: TA, VA1 >: VA](name: Name, ref: VariableRef[TA1, VA1]): Variables[TA1, VA1] =
      copy(variables = self.variables + (name -> ref))
    def set[A, TA1 >: TA, VA1 >: VA](name: Name, value: A, tpe: Type[TA1]): Variables[TA1, VA1] =
      copy(variables = self.variables + (name -> VariableRef.Evaluated(name, tpe)))
  }

  object Variables {
    def empty[TA, VA]: Variables[TA, VA]                                          = new Variables(Map.empty)
    def apply[TA, VA](variables: (Name, VariableRef[TA, VA])*): Variables[TA, VA] = Variables(variables.toMap)
    def fromIterable[TA, VA](variables: Iterable[(Name, VariableRef[TA, VA])]): Variables[TA, VA] =
      Variables(variables.toMap)

    type Typed = Variables[scala.Unit, UType]
    object Typed {
      def empty: Typed = Variables.empty[scala.Unit, UType]
      def apply(variables: (Name, VariableRef[scala.Unit, UType])*): Typed =
        Variables[scala.Unit, UType](variables.toMap)
      def fromIterable(variables: Iterable[(Name, VariableRef[scala.Unit, UType])]): Typed =
        Variables[scala.Unit, UType](variables.toMap)
    }
  }

  sealed trait VariableRef[+TA, +VA] { self =>
    import VariableRef._

    def resolve[TA1 >: TA: Tag, VA1 >: VA: Tag]
        : ZIO[Evaluator[TA1, VA1] with ZEvaluationContext[TA1, VA1], EvaluationError, Any] =
      ZIO.serviceWithZIO[Evaluator[TA1, VA1]](evaluator => resolve(evaluator))

    def resolve[TA1 >: TA: Tag, VA1 >: VA: Tag](
        evaluator: Evaluator[TA1, VA1]
    ): ZIO[ZEvaluationContext[TA1, VA1], EvaluationError, Any] =
      self match {
        case Evaluated(value, tpe) => ZIO.succeed(value)
        case Suspended(value)      => evaluator.evaluate(value)
      }

  }
  object VariableRef {
    final case class Evaluated[+TA](value: Any, tpe: Type[TA]) extends VariableRef[TA, Nothing]
    final case class Suspended[+TA, +VA](value: Value[TA, VA]) extends VariableRef[TA, VA]
  }
}

object ZEvaluationContext {
  def initial[TA: Tag, VA: Tag](variables: Variables[TA, VA]): ZIO[ZEvaluationContext[TA, VA], Nothing, Unit] =
    ZIO.setState(EvaluationContext.root(variables))

  def make[TA: Tag, VA: Tag](
      variables: Variables[TA, VA] = Variables.empty[TA, VA]
  ): URIO[ZState[EvaluationContext[TA, VA]], EvaluationContext[TA, VA]] =
    ZIO.setState(EvaluationContext.root(variables)) *> ZIO.getStateWith[EvaluationContext[TA, VA]](x => x)
}
