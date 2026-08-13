package morphir.buildkit

import kyo.*

/**
 * A composable pipeline stage from `I` to `O` tracking a declared error `E` and the effect row `S`.
 *
 * `Stage` is the building block for compiler pipelines (lex → tokenize → parse → lower → ...). It is a reified
 * structure, not a bare closure: composition and labelling build a tree that interpreters — executors, progress
 * reporting, [[describe]] — can walk. Composition with [[andThen]] preserves effect-row tracking at the type level:
 * composing a stage that needs `Sync` with one that needs `Abort[E]` produces a stage requiring both.
 *
 * `E` and `S` are both invariant: `run`'s own return type `O < (Abort[E] & S)` nests `E` inside two contravariant slots
 * in a row — Kyo's `Abort[-E]` and the pending type's own `<[+A, -S]` — and composing those two contravariant steps
 * lands back on a covariant occurrence, which only an invariant class parameter can host without a variance error (the
 * same reason `S` was already invariant before `E` existed). Composing two stages' declared errors uses `E1 | E2`, not
 * `E1 & E2`: `Abort` is contravariant, so the combined declared error must be a common *supertype* of both operands'
 * errors for `Abort[E1 | E2]` to satisfy what each side individually required — a narrow `E1` satisfies a pipeline
 * whose combined row declares the wider `E1 | E2`.
 *
 * A label is optional and attaches through [[named]]; a stage without a [[Stage.Named]] wrapper simply has none.
 */
enum Stage[-I, +O, E, S]:

  /** A stage that applies `f` to its input, which may abort with `E` alongside whatever `S` requires. */
  case Run(f: I => O < (Abort[E] & S))

  /** Two stages composed left-to-right; both operands survive as values for interpreters to walk. */
  case AndThen[I2, M, O2, E1, E2, S1, S2](
      left: Stage[I2, M, E1, S1],
      right: Stage[M, O2, E2, S2]
  ) extends Stage[I2, O2, E1 | E2, S1 & S2]

  /** A stage carrying [[StageMeta]]; the wrapper adds context and delegates execution unchanged. */
  case Named[I2, O2, E2, S2](stageMeta: StageMeta, stage: Stage[I2, O2, E2, S2]) extends Stage[I2, O2, E2, S2]

  /**
   * Run this stage on `input`.
   *
   * This is the plain interpreter: it executes the tree with no observability — no progress reporting, no provenance
   * tracking. Observable execution arrives as separate interpreters in the pipeline layer.
   */
  def run(input: I): O < (Abort[E] & S) =
    this match
      case Run(f)               => f(input)
      case AndThen(left, right) => left.run(input).map(right.run)
      case Named(_, stage)      => stage.run(input)

  /**
   * Compose this stage with `next`, left to right — Function1-style composition, the primary spelling.
   *
   * Not Kyo's `andThen` on `A < S`, which sequences two computations and discards the first value; here both stages
   * transform data and the output of this stage feeds `next`.
   */
  infix def andThen[O2, E2, S2](next: Stage[O, O2, E2, S2]): Stage[I, O2, E | E2, S & S2] =
    AndThen[I, O, O2, E, E2, S, S2](this, next)

  /** Alias for [[andThen]], for readers arriving from ZIO's `>>>` on pipeline-like types. */
  def >>>[O2, E2, S2](next: Stage[O, O2, E2, S2]): Stage[I, O2, E | E2, S & S2] =
    andThen(next)

  /** Attach a label (and optionally a description) to this stage, replacing any existing one. */
  def named(label: String, description: Maybe[String] = Absent): Stage[I, O, E, S] =
    this match
      case Named(_, inner) => Named(StageMeta(label, description), inner)
      case _               => Named(StageMeta(label, description), this)

  /** The metadata of the outermost [[Stage.Named]] wrapper, if any. */
  def meta: Maybe[StageMeta] =
    this match
      case Named(m, _) => Present(m)
      case _           => Absent

  /** The label of the outermost [[Stage.Named]] wrapper, if any. */
  def label: Maybe[String] =
    meta.map(_.label)

  /**
   * Render this stage's structure: labels where present, `<anonymous>` where not.
   *
   * A label is taken to *summarize* its subtree: a [[Stage.Named]] renders as its label alone, hiding the inner
   * structure it wraps. Richer tree rendering — one that shows a labelled stage's inner structure alongside its label —
   * belongs to later interpreters in the pipeline layer, not to this baseline renderer.
   */
  def describe: String =
    this match
      case Run(_)               => "<anonymous>"
      case AndThen(left, right) => s"${left.describe} andThen ${right.describe}"
      case Named(m, _)          => if m.label.isBlank then "<anonymous>" else m.label

end Stage

object Stage:

  /**
   * Lift an `Abort`/effect-tracked function into a stage — the direct constructor.
   *
   * A bare `Stage(...)` call with no expected type flowing in leaves `S` underconstrained, and inference defaults it to
   * `Nothing` rather than `Any` — a valid but unusable row. Ascribe the stage's type (`Stage[I, O, E, S]`) when
   * composing one standalone, rather than inline inside a pipeline where the expected type already pins `S`.
   */
  def apply[I, O, E, S](f: I => O < (Abort[E] & S)): Stage[I, O, E, S] =
    Run(f)

  /** A stage that returns its input unchanged with no effects. */
  def identity[A]: Stage[A, A, Nothing, Any] =
    Run(a => a)

  /** Lift a pure function into a stage with no effects. */
  def pure[A, B](f: A => B): Stage[A, B, Nothing, Any] =
    Run(a => f(a))

  /** Lift an effect-tracked function into a stage with no declared error. */
  def fromKyo[A, B, S](f: A => B < S): Stage[A, B, Nothing, S] =
    Run(f)

  /** A stage that never aborts: its declared error is `Nothing`, so it composes into any pipeline's error row. */
  type Infallible[-I, +O, S] = Stage[I, O, Nothing, S]
end Stage
