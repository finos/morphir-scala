package org.finos.morphir.core.capabilities.free
import scala.annotation.tailrec
import Free.*
sealed trait Free[F[+_, +_], +E, +A] { self =>

  final def catchAll[E2, A1 >: A](
      f: E => Free[F, E2, A1]
  ): Free[F, E2, A1] = self match {
    case free @ Sequence(fa, onSuccess, onFailure) =>
      Sequence(
        fa,
        (a: free.InSuccess) => onSuccess(a).catchAll(f),
        (e: free.InFailure) => onFailure(e).catchAll(f)
      )
    case _ => Free.Sequence[F, E, E2, A, A1](self, Free.Succeed(_), f)
  }

  final def flatMap[E1 >: E, B](f: A => Free[F, E1, B]): Free[F, E1, B] = self match {
    case free @ Sequence(fa, onSuccess, onFailure) =>
      Sequence(fa, (a: free.InSuccess) => onSuccess(a).flatMap(f), (e: free.InFailure) => onFailure(e).flatMap(f))
    case _ => Free.Sequence[F, E, E1, A, B](self, f, Free.Fail(_))
  }

  final def flatten[E1 >: E, B](implicit ev: A <:< Free[F, E1, B]): Free[F, E1, B] =
    self.flatMap(ev)

  def interpret[G[+_, +_]](
      interpreter: Free.Interpreter[F, G]
  )(implicit g: Free.Executable[G]): G[E, A] = self match {
    case Free.Succeed(a)                                => g.succeed(a)
    case Free.Fail(e)                                   => g.fail(e)
    case Free.Eval(fa)                                  => interpreter.interpret(fa)
    case free @ Free.Sequence(fa, onSuccess, onFailure) =>
      g.sequence(
        fa.interpret(interpreter),
        (a: free.InSuccess) => onSuccess(a).interpret(interpreter),
        (e: free.InFailure) => onFailure(e).interpret(interpreter)
      )
  }

  final def map[B](f: A => B): Free[F, E, B] =
    self.flatMap(a => Free.Succeed(f(a)))

  final def mapError[E2](f: E => E2): Free[F, E2, A] =
    self.catchAll(e => Free.Fail(f(e)))

  def unsafeInterpret(
      unsafeInterpreter: Free.UnsafeInterpreter[F]
  ): Either[E, A] = {
    @tailrec
    def loop(
        free: Free[F, Any, Any],
        stack: List[Free.Sequence[F, Any, Any, Any, Any]]
    ): Either[E, A] =
      free match {
        case Free.Succeed(a) =>
          stack match {
            case Free.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
            case Nil                                     => Right(a.asInstanceOf[A])
          }
        case Free.Fail(e) =>
          stack match {
            case Free.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
            case Nil                                     => Left(e.asInstanceOf[E])
          }
        case Free.Eval(fa) =>
          unsafeInterpreter.interpret(fa) match {
            case Left(e) =>
              stack match {
                case Free.Sequence(_, _, onFailure) :: stack => loop(onFailure(e), stack)
                case Nil                                     => Left(e.asInstanceOf[E])
              }
            case Right(a) =>
              stack match {
                case Free.Sequence(_, onSuccess, _) :: stack => loop(onSuccess(a), stack)
                case Nil                                     => Right(a.asInstanceOf[A])
              }
          }
        case free @ Free.Sequence(fa, _, _) =>
          loop(fa, (free :: stack).asInstanceOf[List[Free.Sequence[F, Any, Any, Any, Any]]])
      }
    loop(self, Nil)
  }
}

object Free {
  def eval[F[+_, +_], E, A](fa: F[E, A]): Free[F, E, A] = Eval(fa)
  def fail[F[+_, +_], E](e: E): Free[F, E, Nothing]     = Fail(e)
  def succeed[F[+_, +_], A](a: A): Free[F, Nothing, A]  = Succeed(a)

  final case class Succeed[F[+_, +_], A](a: A)        extends Free[F, Nothing, A]
  final case class Fail[F[+_, +_], E](a: E)           extends Free[F, E, Nothing]
  final case class Eval[F[+_, +_], E, A](fa: F[E, A]) extends Free[F, E, A]
  final case class Sequence[F[+_, +_], E1, E2, A1, A2] private[Free] (
      fa: Free[F, E1, A1],
      onSuccess: A1 => Free[F, E2, A2],
      onFailure: E1 => Free[F, E2, A2]
  ) extends Free[F, E2, A2] {
    type InSuccess = A1
    type InFailure = E1
  }

  trait Interpreter[F[+_, +_], G[+_, +_]] {
    def interpret[E, A](fa: F[E, A]): G[E, A]
  }

  trait UnsafeInterpreter[F[+_, +_]] {
    def interpret[E, A](fa: F[E, A]): Either[E, A]
  }

  trait Executable[F[+_, +_]] {
    def succeed[A](a: A): F[Nothing, A]
    def fail[E](e: E): F[E, Nothing]
    def eval[E, A](fa: F[E, A]): F[E, A]
    def sequence[E1, E2, A1, A2](
        fa: F[E1, A1],
        onSuccess: A1 => F[E2, A2],
        onFailure: E1 => F[E2, A2]
    ): F[E2, A2]
  }
}
