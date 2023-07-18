package org.finos.morphir.foundations.capabilities.free

import Free.*
sealed trait Free[F[+_, +_], +E, +A] { self =>

  def catchAll[E2, A1 >: A](onFailure: E => Free[F, E2, A1]): Free[F, E2, A1] =
    Sequence[F, E, E2, A, A1](self, Succeed(_), onFailure)

  def flatMap[E1 >: E, B](onSuccess: A => Free[F, E1, B]): Free[F, E1, B] =
    Sequence[F, E, E1, A, B](self, onSuccess, Fail(_))

  def interpret[G[+_, +_]](interpreter: Free.Interpreter[F, G])(implicit g: Executable[G]): G[E, A] = self match {
    case Succeed(a) => g.succeed(a)
    case Fail(e)    => g.fail(e)
    case Eval(fa)   => interpreter.interpret(fa)
    case free @ Free.Sequence(fa, onSuccess, onFailure) =>
      g.sequence(
        fa.interpret(interpreter),
        (a: free.InSuccess) => onSuccess(a).interpret(interpreter),
        (e: free.InFailure) => onFailure(e).interpret(interpreter)
      )
  }

  def map[B](f: A => B): Free[F, E, B] =
    self.flatMap(a => Free.Succeed(f(a)))

  def unsafeInterpret(unsafeInterpreter: Free.UnsafeInterpreter[F]): Either[E, A] = {
    def loop(free: Free[F, Any, Any], stack: List[Sequence[F, Any, Any, Any, Any]]): Either[E, A] = free match {
      case Succeed(a) => Right(a.asInstanceOf[A])
      case Fail(e)    => Left(e.asInstanceOf[E])
      case free @ Eval(fa) =>
        unsafeInterpreter.interpret(fa).asInstanceOf[Either[E, A]]
      case free @ Sequence(fa, onSuccess, onFailure) => ???
      // loop(unsafeInterpreter.interpret(fa), stack)
    }
    loop(self, Nil)
  }
}

object Free {
  def eval[F[+_, +_], E, A](fa: F[E, A]): Free[F, E, A] = Eval(fa)
  def fail[F[+_, +_], E](e: E): Free[F, E, Nothing]     = Fail(e)
  def succeed[F[+_, +_], A](a: A): Free[F, Nothing, A]  = Succeed(a)

  final case class Succeed[F[+_, +_], A](a: A) extends Free[F, Nothing, A]
  final case class Fail[F[+_, +_], E](a: E)    extends Free[F, E, Nothing]
  final case class Eval[F[+_, +_], E, A](fa: F[E, A]) extends Free[F, E, A] {
    type InSuccess = A
    type InFailure = E
  }
  final case class Sequence[F[+_, +_], E1, E2, A1, A2](
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
    // def execute[E, A](fa: F[E, A]): Either[E, A]
  }
}
