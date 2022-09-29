package org.finos
package morphir
package ir
package pipeline

import zio.prelude.fx._
import io.lemonlabs.uri.Urn

import zio._

trait Step[-In, -Env, +Err, +Out] { self =>
  def run(in: In): ZIO[Env, Err, Out]

  final def >>>[Env1 <: Env, Err1 >: Err, Out2](that: Step[Out, Env1, Err1, Out2]): Step[In, Env1, Err1, Out2] =
    new Step[In, Env1, Err1, Out2] {
      def run(in: In): ZIO[Env1, Err1, Out2] =
        self.run(in).flatMap(that.run(_))
    }
}

object Step extends NameSteps {

  def fail[Err](err: Err): Step[Any, Any, Err, Nothing] =
    new Step[Any, Any, Err, Nothing] {
      def run(in: Any): ZIO[Any, Err, Nothing] = ZIO.fail(err)
    }
  def succeed[A](a: => A) = new Step[Any, Any, Nothing, A] {
    def run(in: Any): ZIO[Any, Nothing, A] = ZIO.succeed(a)
  }

  // /**
  //  * Constructs a step that returns the input state without modifying it.
  //  */
  // def context: Step[Unit, Any, Nothing, AnyStepContext] = ZPure.get[AnyStepContext]

  def fromArgs[Args, A](f: Args => A): Step[Args, Any, Nothing, A] = new Step[Args, Any, Nothing, A] {
    def run(in: Args): ZIO[Any, Nothing, A] = ZIO.succeed(f(in))
  }

  // def inputContext[Args]: Step[Args, Any, Nothing, StepContext[Args]] = ZPure.get[StepContext[Args]].mapState(_.toAny)

  // // def getStateAndInput[S, Input]: Step[Any, Nothing, (S, Input)] =
  // //   ZPure.get[(S, Input)].mapState { case (s, _) => s }

  // // def getInput[Input]: Step[Any, Nothing, Input] =
  // //   Step.get[(Any, Input)].mapState { case (s, _) => s }.map { case (_, a) => a }

  // // def fromFunction[S, A](f: S => A): Step[S, S, Any, Nothing, A] = ???

  // implicit final class StepOps[Args, Context, +E, +A](private val self: Step[Args, Context, E, A]) extends AnyVal {}
}
