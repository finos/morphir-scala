package org.finos.morphir.runtime
import zio.prelude.fx.*
import zio.*

/**
 * A runtime action that can be executed to produce a result
 */
final case class RTAction[-R, +E, +A](computation: ZPure[Nothing, RTExecutionContext, RTExecutionContext, R, E, A])
    extends AnyVal { self =>
  import RTAction.*

  def *>[R1 <: R, E1 >: E, B](that: RTAction[R1, E1, B]): RTAction[R1, E1, B] =
    RTAction(computation *> that.computation)

  def <*[R1 <: R, E1 >: E, B](that: RTAction[R1, E1, B]): RTAction[R1, E1, A] =
    RTAction(computation <* that.computation)

  def as[B](b: B): RTAction[R, E, B] =
    RTAction(computation.as(b))

  def flatMap[R1 <: R, E1 >: E, B](f: A => RTAction[R1, E1, B]): RTAction[R1, E1, B] =
    RTAction(computation.flatMap(f(_).computation))

  def map[B](f: A => B): RTAction[R, E, B] =
    RTAction(computation.map(f))

  def provideEnvionment(r: ZEnvironment[R]): RTAction[Any, E, A] =
    RTAction(computation.provideEnvironment(r))

  def runEither(implicit ev2: Any <:< R): Either[E, A] =
    runEitherWith(RTExecutionContext.default)

  def runEitherWith(context: RTExecutionContext)(implicit ev2: Any <:< R): Either[E, A] =
    (ZPure.set(context) *> computation).runEither

  def runResult(context: RTExecutionContext)(implicit ev1: Any <:< R, ev2: E <:< Nothing): A =
    computation.runResult(context)

  def toZIO(implicit ev2: Any <:< R): ZIO[R & RTExecutionContext, E, A] =
    ZIO.serviceWithZIO[RTExecutionContext](context => computation.toZIOWith(context))

  def toZIOWith: ZIO[R, E, A]                              = computation.toZIOWith(RTExecutionContext.default)
  def toZIOWith(context: RTExecutionContext): ZIO[R, E, A] = computation.toZIOWith(context)
  @inline def toZPure: ZPure[Nothing, RTExecutionContext, RTExecutionContext, R, E, A] = computation
}

object RTAction {
  val unit: RTAction[Any, Nothing, Unit] =
    RTAction(ZPure.unit)

  def fail[E](e: E): RTAction[Any, E, Nothing] =
    RTAction(ZPure.fail(e))

  def fromEither[E, A](either: Either[E, A]): RTAction[Any, E, A] =
    RTAction(ZPure.fromEither(either))

  def fromOption[A](option: Option[A]): RTAction[Any, Unit, A] =
    RTAction(ZPure.fromOption(option))

  def fromTry[A](t: scala.util.Try[A]): RTAction[Any, Throwable, A] =
    RTAction(ZPure.fromTry(t))

  def setContext[R, E](context: RTExecutionContext): RTAction[R, E, Unit] =
    RTAction(ZPure.set(context))

  def succeed[A](a: A): RTAction[Any, Nothing, A] =
    RTAction(ZPure.succeed(a))

  implicit def toZPure[R, E, A](action: RTAction[R, E, A])
      : ZPure[Nothing, RTExecutionContext, RTExecutionContext, R, E, A] =
    action.toZPure
}
