package morphir.langkit.elm.compiler

import kyo.*

/**
 * A composable pipeline stage from `I` to `O` tracking the effect row `S`.
 *
 * `Stage` is the building block for compiler pipelines (lex → tokenize → parse → lower → ...). Composition with `>>>`
 * preserves effect-row tracking at the type level: composing a stage that needs `Sync` with one that needs `Abort[E]`
 * produces a stage requiring both.
 */
trait Stage[-I, +O, S]:
  def run(input: I): O < S

  final def >>>[O2, S2](next: Stage[O, O2, S2]): Stage[I, O2, S & S2] =
    new Stage[I, O2, S & S2]:
      def run(input: I): O2 < (S & S2) =
        Stage.this.run(input).map(next.run)

object Stage:

  /** A stage that returns its input unchanged with no effects. */
  def identity[A]: Stage[A, A, Any] =
    new Stage[A, A, Any]:
      def run(input: A): A < Any = input

  /** Lift a pure function into a stage with no effects. */
  def pure[A, B](f: A => B): Stage[A, B, Any] =
    new Stage[A, B, Any]:
      def run(input: A): B < Any = f(input)

  /** Lift an effect-tracked function into a stage. */
  def fromKyo[A, B, S](f: A => B < S): Stage[A, B, S] =
    new Stage[A, B, S]:
      def run(input: A): B < S = f(input)
end Stage
