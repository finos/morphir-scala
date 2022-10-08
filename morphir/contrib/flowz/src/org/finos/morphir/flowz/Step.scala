package org.finos
package morphir
package flowz

import zio._

trait Step[-Env, +Err, -In, +Out] { self =>
  def run(in: In): ZIO[Env, Err, Out]

  final def >>>[Env1 <: Env, Err1 >: Err, Out2](that: Step[Env1, Err1, Out, Out2]): Step[Env1, Err1, In, Out2] =
    new Step[Env1, Err1, In, Out2] {
      def run(in: In): ZIO[Env1, Err1, Out2] =
        self.run(in).flatMap(that.run(_))
    }
}

object Step {

  def attemptFunction[In, Out](f: In => Out) = new Step[Any, Throwable, In, Out] {
    def run(in: In): ZIO[Any, Throwable, Out] = ZIO.attempt(f(in))
  }

  def fail[Err](err: Err): Step[Any, Err, Any, Nothing] =
    new Step[Any, Err, Any, Nothing] {
      def run(in: Any): ZIO[Any, Err, Nothing] = ZIO.fail(err)
    }

  /**
   * Creates a step that maps from `In` to `Out` using the specified function.
   */
  def map[In, Out](f: In => Out): Step[Any, Nothing, In, Out] =
    new Step[Any, Nothing, In, Out] {
      def run(in: In): ZIO[Any, Nothing, Out] = ZIO.succeed(f(in))
    }
  def succeed[A](a: => A) = new Step[Any, Nothing, Any, A] {
    def run(in: Any): ZIO[Any, Nothing, A] = ZIO.succeed(a)
  }
}
