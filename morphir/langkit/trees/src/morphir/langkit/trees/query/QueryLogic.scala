package morphir.langkit.trees.query

import kyo.*

object QueryLogic:

  final case class QueryState[Ctx, Err](context: Ctx, errors: Vector[Err]) derives CanEqual

  final case class Result[Ctx, Log, Err, A](
      context: Ctx,
      logs: Vector[Log],
      errors: Vector[Err],
      value: Either[Vector[Err], A]
  ) derives CanEqual

  type QueryEffects[Ctx, Log, Err] = Var[QueryState[Ctx, Err]] & Emit[Log] & Abort[Err]

  type QueryEffect[Ctx, Log, Err, A] = A < QueryEffects[Ctx, Log, Err]

  def run[Ctx, Log, Err, A](initialContext: Ctx)(effect: QueryEffect[Ctx, Log, Err, A])(using
      Tag[Ctx],
      Tag[Log],
      Tag[Err],
      Tag[Var[QueryState[Ctx, Err]]],
      Tag[Emit[Log]],
      ConcreteTag[Err],
      Frame
  ): Result[Ctx, Log, Err, A] =
    val initial                      = QueryState(initialContext, Vector.empty[Err])
    val (state, (logs, abortResult)) =
      Var.runTuple(initial) {
        Emit.run {
          Abort.run(effect)
        }
      }.eval
    val value: Either[Err, A] = abortResult match
      case kyo.Result.Success(a)   => Right(a)
      case kyo.Result.Failure(err) => Left(err)
    val allErrors = value.left.toOption.fold(state.errors)(state.errors :+ _)
    val rendered  = if allErrors.nonEmpty then Left(allErrors) else value.left.map(_ => allErrors)
    Result(state.context, logs.toVector, allErrors, rendered)

  def readContext[Ctx, Log, Err](using Tag[Var[QueryState[Ctx, Err]]]): Ctx < QueryEffects[Ctx, Log, Err] =
    Var.use(_.context)

  def setContext[Ctx, Log, Err](context: Ctx)(using
      Tag[Var[QueryState[Ctx, Err]]]
  ): Unit < QueryEffects[Ctx, Log, Err] =
    Var.updateDiscard(_.copy(context = context))

  def updateContext[Ctx, Log, Err](f: Ctx => Ctx)(using
      Tag[Var[QueryState[Ctx, Err]]]
  ): Unit < QueryEffects[Ctx, Log, Err] =
    Var.updateDiscard(s => s.copy(context = f(s.context)))

  def log[Ctx, Log, Err](entry: Log)(using Tag[Emit[Log]]): Unit < QueryEffects[Ctx, Log, Err] =
    Emit.value(entry)

  def error[Ctx, Log, Err](err: Err)(using Tag[Var[QueryState[Ctx, Err]]]): Unit < QueryEffects[Ctx, Log, Err] =
    Var.updateDiscard(s => s.copy(errors = s.errors :+ err))

  def failFast[Ctx, Log, Err](
      err: Err
  )(using tag: ConcreteTag[Err], frame: Frame): Nothing < QueryEffects[Ctx, Log, Err] =
    Abort.fail[Err](err)
