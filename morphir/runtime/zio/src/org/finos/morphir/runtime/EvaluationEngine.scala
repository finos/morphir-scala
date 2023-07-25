package org.finos.morphir
package runtime

import ir.Value.{TypedValue, Value}
import ir.Type.UType
import Value.Folder
import org.finos.morphir.ir.{FQName, Name, Type}
import org.finos.morphir.ir.Literal.Lit
import org.finos.morphir.ir.Value.Pattern
import zio.*
import org.finos.morphir.ir.Literal.Literal.*
import zio.prelude.*
import zio.prelude.fx.*
import EvaluationEngine.*
import org.finos.morphir.runtime.{EngineEvent, EvaluationError, LogEvent, MorphirRecord}
import step.*

abstract class EvaluationEngine[TA: Tag, VA: Tag] extends Folder[scala.Unit, TA, VA, Step[TA, VA, EvalResult]] {
  self =>
  import EvaluationEngine._
  final type Ctx = Context[TA, VA]

  def createContext(bindings: VarBinding*): Ctx = Context.createRoot[TA, VA](bindings: _*)

  def evaluate(value: Value[TA, VA]): Step[TA, VA, EvalResult] =
    value.foldContext(())(self)

  def makeContext(bindings: VarBinding*): Reader[Any, Ctx] = Reader.succeed {
    createContext(bindings: _*)
  }

  def applyCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      function: Step[TA, VA, EvalResult],
      argument: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitApply(value, attributes, function, argument)

  def constructorCase(context: Unit, value: Value[TA, VA], attributes: VA, name: FQName): Step[TA, VA, EvalResult] =
    visitConstructor(value, attributes, name)

  def destructureCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Step[TA, VA, EvalResult],
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitDestructure(value, attributes, pattern, valueToDestruct, inValue)

  def fieldCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      subjectValue: Step[TA, VA, EvalResult],
      fieldName: Name
  ): Step[TA, VA, EvalResult] = visitField(value, attributes, subjectValue, fieldName)

  def fieldFunctionCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      fieldName: Name
  ): Step[TA, VA, EvalResult] =
    visitFieldFunction(value, attributes, fieldName)

  def ifThenElseCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      condition: Step[TA, VA, EvalResult],
      thenBranch: Step[TA, VA, EvalResult],
      elseBranch: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitIfThenElse(value, attributes, condition, thenBranch, elseBranch)

  def lambdaCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      argumentPattern: Pattern[VA],
      body: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitLambda(value, attributes, argumentPattern, body)

  def letDefinitionCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      valueName: Name,
      valueDefinition: (Chunk[(Name, VA, Type.Type[TA])], Type.Type[TA], Step[TA, VA, EvalResult]),
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitLetDefinition(value, attributes, valueName, valueDefinition, inValue)

  def letRecursionCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      valueDefinitions: Map[Name, (Chunk[(Name, VA, Type.Type[TA])], Type.Type[TA], Step[TA, VA, EvalResult])],
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = visitLetRecursion(value, attributes, valueDefinitions, inValue)

  def listCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      elements: Chunk[Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] = visitList(value, attributes, elements)

  def literalCase(context: Unit, value: Value[TA, VA], attributes: VA, literal: Lit): Step[TA, VA, EvalResult] =
    visitLiteral(value, attributes, literal)

  def patternMatchCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      branchOutOn: Step[TA, VA, EvalResult],
      cases: Chunk[(Pattern[VA], Step[TA, VA, EvalResult])]
  ): Step[TA, VA, EvalResult] = visitPatternMatch(value, attributes, branchOutOn, cases)

  def recordCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      fields: Chunk[(Name, Step[TA, VA, EvalResult])]
  ): Step[TA, VA, EvalResult] = visitRecord(value, attributes, fields)
  def referenceCase(context: Unit, value: Value[TA, VA], attributes: VA, name: FQName): Step[TA, VA, EvalResult] =
    visitReference(value, attributes, name)

  def tupleCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      elements: Chunk[Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] = visitTuple(value, attributes, elements)

  override def unitCase(context: Unit, value: Value[TA, VA], attributes: VA): Step[TA, VA, EvalResult] =
    visitUnit(value, attributes)

  override def updateRecordCase(
      context: Unit,
      value: Value[TA, VA],
      attributes: VA,
      valueToUpdate: Step[TA, VA, EvalResult],
      fieldsToUpdate: Map[Name, Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] = visitUpdateRecord(value, attributes, valueToUpdate, fieldsToUpdate)

  override def variableCase(context: Unit, value: Value[TA, VA], attributes: VA, name: Name): Step[TA, VA, EvalResult] =
    visitVariable(value, attributes, name)

  def visitApply(
      value: Value[TA, VA],
      attributes: VA,
      function: Step[TA, VA, EvalResult],
      argument: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = ???

  def visitConstructor(value: Value[TA, VA], attributes: VA, name: FQName): Step[TA, VA, EvalResult] = ???

  def visitDestructure(
      value: Value[TA, VA],
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Step[TA, VA, EvalResult],
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = ???
  def visitField(
      value: Value[TA, VA],
      attributes: VA,
      subjectValue: Step[TA, VA, EvalResult],
      fieldName: Name
  ): Step[TA, VA, EvalResult] = ???

  def visitFieldFunction(value: Value[TA, VA], attributes: VA, fieldName: Name): Step[TA, VA, EvalResult] = ???

  def visitIfThenElse(
      value: Value[TA, VA],
      attributes: VA,
      condition: Step[TA, VA, EvalResult],
      thenBranch: Step[TA, VA, EvalResult],
      elseBranch: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = condition.flatMap { cond =>
    cond match {
      case true => thenBranch
      case _    => elseBranch
    }
  }

  def visitLambda(
      value: Value[TA, VA],
      attributes: VA,
      argumentPattern: Pattern[VA],
      body: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = ???

  def visitLetDefinition(
      value: Value[TA, VA],
      attributes: VA,
      valueName: Name,
      valueDefinition: (Chunk[(Name, VA, Type.Type[TA])], Type.Type[TA], Step[TA, VA, EvalResult]),
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] =
    for {
      variable <- Step.succeed(Var(valueName))
      (_, _, body) = valueDefinition
      evaluatedBody   <- body
      originalContext <- Step.get[Context[TA, VA]]
      updatedContext = originalContext.push(variable := evaluatedBody)
      _   <- Step.set(updatedContext)
      res <- inValue
      _   <- Step.set(originalContext)
    } yield res

  def visitLetRecursion(
      value: Value[TA, VA],
      attributes: VA,
      valueDefinitions: Map[Name, (Chunk[(Name, VA, Type.Type[TA])], Type.Type[TA], Step[TA, VA, EvalResult])],
      inValue: Step[TA, VA, EvalResult]
  ): Step[TA, VA, EvalResult] = ???

  def visitList(
      value: Value[TA, VA],
      attributes: VA,
      elements: Chunk[Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] =
    Step.collectAll(elements).map(_.toList)

  def visitLiteral(value: Value[TA, VA], attributes: VA, literal: Lit): Step[TA, VA, EvalResult] = Step.succeed {
    // TODO: Look at the type annotation and convert to the correct type
    literal match {
      case StringLiteral(value)      => value
      case FloatLiteral(value)       => value
      case CharLiteral(value)        => value
      case BoolLiteral(value)        => value
      case WholeNumberLiteral(value) => value
      case DecimalLiteral(value)     => value
    }
  }

  def visitPatternMatch(
      value: Value[TA, VA],
      attributes: VA,
      branchOutOn: Step[TA, VA, EvalResult],
      cases: Chunk[(Pattern[VA], Step[TA, VA, EvalResult])]
  ): Step[TA, VA, EvalResult] = ???

  def visitRecord(
      value: Value[TA, VA],
      attributes: VA,
      fields: Chunk[(Name, Step[TA, VA, EvalResult])]
  ): Step[TA, VA, EvalResult] = {
    val fieldTuples: Chunk[Step[TA, VA, (Name, EvalResult)]] = fields.map { case (name, value) =>
      value.map { case er: EvalResult => name -> er }
    }
    Step.collectAll(fieldTuples).map(MorphirRecord(_))
  }

  def visitReference(value: Value[TA, VA], attributes: VA, name: FQName): Step[TA, VA, EvalResult] = ???

  def visitTuple(
      value: Value[TA, VA],
      attributes: VA,
      elements: Chunk[Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] =
    Step.collectAll(elements).flatMap { items =>
      items match {
        case Chunk(a, b)                                     => Step.succeed((a, b))
        case Chunk(a, b, c)                                  => Step.succeed((a, b, c))
        case Chunk(a, b, c, d)                               => Step.succeed((a, b, c, d))
        case Chunk(a, b, c, d, e)                            => Step.succeed((a, b, c, d, e))
        case Chunk(a, b, c, d, e, f)                         => Step.succeed((a, b, c, d, e, f))
        case Chunk(a, b, c, d, e, f, g)                      => Step.succeed((a, b, c, d, e, f, g))
        case Chunk(a, b, c, d, e, f, g, h)                   => Step.succeed((a, b, c, d, e, f, g, h))
        case Chunk(a, b, c, d, e, f, g, h, i)                => Step.succeed((a, b, c, d, e, f, g, h, i))
        case Chunk(a, b, c, d, e, f, g, h, i, j)             => Step.succeed((a, b, c, d, e, f, g, h, i, j))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k)          => Step.succeed((a, b, c, d, e, f, g, h, i, j, k))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l)       => Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m)    => Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n) => Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u))
        case Chunk(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
          Step.succeed((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v))
        case _ => Step.fail(EvaluationError.UnsupportedTupleArity(value, elements.size))
      }
    }
  def visitUnit(value: Value[TA, VA], attributes: VA): Step[TA, VA, EvalResult] = Step.succeed(())

  def visitUpdateRecord(
      value: Value[TA, VA],
      attributes: VA,
      valueToUpdate: Step[TA, VA, EvalResult],
      fieldsToUpdate: Map[Name, Step[TA, VA, EvalResult]]
  ): Step[TA, VA, EvalResult] = ???

  def visitVariable(value: Value[TA, VA], attributes: VA, name: Name): Step[TA, VA, EvalResult] =
    Context.lookupVariableStep[TA, VA](Var(name)).map(_.resolvedValue)
}

object EvaluationEngine {

  /**
   * Evaluate a value in the context of the given evaluation engine.
   */
  def evaluate[TA: Tag, VA: Tag](value: Value[TA, VA]): ZStep[EvaluationEngine[TA, VA], TA, VA, EvalResult] =
    Step.serviceWithPure[EvaluationEngine[TA, VA]] { engine =>
      engine.evaluate(value)
    }

  def evaluateZIO[TA: Tag, VA: Tag](
      value: Value[TA, VA]
  ): ZIO[EvaluationEngine[TA, VA] with Context[TA, VA], EvaluationError, EvalResult] =
    ZIO.serviceWithZIO[EvaluationEngine[TA, VA]] { engine =>
      ZIO.serviceWithZIO[Context[TA, VA]] { context =>
        val runnable = engine.evaluate(value).provideService(engine).provideState(context)
        ZIO.fromEither(runnable.runEither)
      }
    }

  def evaluateZIO[TA: Tag, VA: Tag](
      value: Value[TA, VA],
      context: Context[TA, VA]
  ): ZIO[EvaluationEngine[TA, VA], EvaluationError, EvalResult] =
    evaluateZIO(value).provideSome[EvaluationEngine[TA, VA]](ZLayer.succeed(context))

  def typed: EvaluationEngine[scala.Unit, UType] = new EvaluationEngine[scala.Unit, UType] {}

  type EvalResult = Any

  final case class Context[+TA, +VA](parent: Option[Context[TA, VA]], variables: Variables) { self =>
    def +=(binding: VarBinding): Context[TA, VA]    = copy(variables = self.variables += binding)
    def ++=(bindings: VarBinding*): Context[TA, VA] = copy(variables = self.variables.++=(bindings: _*))

    def push(bindings: VarBinding*): Context[TA, VA] =
      copy(variables = Variables.withBindings(bindings: _*), parent = Some(self))

    // def scoped[R,A](bindings: VarBinding*)(use: => ZSet[R, TA,VA, A]): ZSet[R,TA,VA,A] =
    //   Context(parent = Some(self), variables = variables)

    def variable(variable: Var): Option[VarValue] = variables.get(variable).orElse(parent.flatMap(_.variable(variable)))

    def lookupVariable(
        variable: Var
    ): ZPure[EngineEvent, Any, Any, Any, EvaluationError.VariableNotFound, VarValue] = {
      def loop(
          context: Context[TA, VA]
      ): ZPure[EngineEvent, Any, Any, Any, EvaluationError.VariableNotFound, VarValue] =
        (context.parent, context.variables.get(variable)) match {
          case (_, Some(value))  => ZPure.succeed(value)
          case (Some(parent), _) => ZPure.log(LogEvent.Trace("Looking up variable in parent context")) *> loop(parent)
          case _                 => ZPure.fail(EvaluationError.VariableNotFound(variable.name))
        }
      loop(self)
    }

    def withBindings(bindings: VarBinding*): Context[TA, VA] =
      copy(variables = self.variables.withBindings(bindings: _*))
  }

  object Context {
    def createRoot[TA, VA](bindings: VarBinding*): Context[TA, VA] =
      Context(parent = None, Variables.withBindings(bindings: _*))

    def lookupVariable[TA: Tag, VA: Tag](
        variable: Var
    ): ZPure[EngineEvent, Any, Any, Context[TA, VA], EvaluationError.VariableNotFound, VarValue] =
      ZPure.serviceWithPure[Context[TA, VA]](_.lookupVariable(variable))

    def lookupVariableStep[TA: Tag, VA: Tag](variable: Var): Step[TA, VA, VarValue] =
      Step.get[Context[TA, VA]].flatMap(context => context.lookupVariable(variable) <* Step.set(context))

    type Typed = Context[scala.Unit, UType]
    object Typed {
      def createRoot(bindings: VarBinding*): Typed = Context.createRoot[scala.Unit, UType](bindings: _*)
      def lookupVariable(
          variable: Var
      ): ZPure[EngineEvent, Any, Any, Context[Unit, UType], EvaluationError.VariableNotFound, VarValue] =
        Context.lookupVariable[scala.Unit, UType](variable)
    }
  }

  final case class Variables(bindings: Map[Var, VarValue]) { self =>
    def +=(binding: VarBinding): Variables = Variables(
      bindings + (binding.variable -> VarValue.Resolved(binding.value))
    )

    /**
     * Alias for `withBindings`
     */
    def ++=(bindings: VarBinding*): Variables = withBindings(bindings: _*)

    def apply(variable: Var): VarValue = bindings.get(variable) match {
      case Some(value) => value
      case None        => throw new EvaluationError.VariableNotFound(variable.name)
    }

    def get(variable: Var): Option[VarValue] = bindings.get(variable)

    def withBindings(bindings: VarBinding*): Variables = Variables(
      self.bindings ++ bindings.map(b => b.variable -> VarValue.Resolved(b.value))
    )
  }

  object Variables {
    val empty: Variables = Variables(Map.empty)
    def withBindings(bindings: VarBinding*): Variables = Variables(
      bindings.map(b => b.variable -> VarValue.Resolved(b.value)).toMap
    )
  }

  type Var = Var.Type
  object Var extends Newtype[Name] {
    implicit class VarOps(val self: Var) extends AnyVal {
      def name: Name = unwrap(self)

      def :=(value: Any) = VarBinding(self, value)
    }
  }

  final case class VarBinding(variable: Var, value: Any)

  sealed trait VarValue { self =>
    def resolvedValue: Any = self match {
      case VarValue.Resolved(value) => value
    }
  }
  object VarValue {
    final case class Resolved(value: Any) extends VarValue
  }

}
