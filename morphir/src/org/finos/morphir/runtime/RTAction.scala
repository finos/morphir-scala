package org.finos.morphir.runtime
import zio.prelude.fx.*
import zio.ZIO

/**
 * A runtime action that can be executed to produce a result
 */
final case class RTAction[-R, +E, +A](computation: ZPure[Nothing, Unit, Unit, R, E, A]) extends AnyVal { self =>

  def flatMap[R1 <: R, E1 >: E, B](f: A => RTAction[R1, E1, B]): RTAction[R1, E1, B] =
    RTAction(computation.flatMap(f(_).computation))

  def map[B](f: A => B): RTAction[R, E, B] =
    RTAction(computation.map(f))

  def runEither(implicit ev2: Any <:< R): Either[E, A] =
    computation.runEither

  def runResult(implicit ev1: Any <:< R, ev2: E <:< Nothing): A =
    computation.runResult(())

  def toZIO: ZIO[R, E, A]                                  = computation.toZIO
  @inline def toZPure: ZPure[Nothing, Unit, Unit, R, E, A] = computation
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

  def succeed[A](a: A): RTAction[Any, Nothing, A] =
    RTAction(ZPure.succeed(a))

  implicit def toZPure[R, E, A](action: RTAction[R, E, A]): ZPure[Nothing, Unit, Unit, R, E, A] =
    action.toZPure
}
