package morphir.buildkit

import kyo.*

/**
 * A composable pipeline stage from `I` to `O` tracking the effect row `S`.
 *
 * `Stage` is the building block for compiler pipelines (lex → tokenize → parse → lower → ...). It is a reified
 * structure, not a bare closure: composition and labelling build a tree that interpreters — executors, progress
 * reporting, [[describe]] — can walk. Composition with [[andThen]] preserves effect-row tracking at the type level:
 * composing a stage that needs `Sync` with one that needs `Abort[E]` produces a stage requiring both.
 *
 * A label is optional and attaches through [[named]]; a stage without a [[Stage.Named]] wrapper simply has none.
 */
enum Stage[-I, +O, S]:

  /** A stage that applies `f` to its input. */
  case Run(f: I => O < S)

  /** Two stages composed left-to-right; both operands survive as values for interpreters to walk. */
  case AndThen[I2, M, O2, S1, S2](
      left: Stage[I2, M, S1],
      right: Stage[M, O2, S2]
  ) extends Stage[I2, O2, S1 & S2]

  /** A stage carrying [[StageMeta]]; the wrapper adds context and delegates execution unchanged. */
  case Named[I2, O2, S2](stageMeta: StageMeta, stage: Stage[I2, O2, S2]) extends Stage[I2, O2, S2]

  /**
   * Run this stage on `input`.
   *
   * This is the plain interpreter: it executes the tree with no observability — no progress reporting, no provenance
   * tracking. Observable execution arrives as separate interpreters in the pipeline layer.
   */
  def run(input: I): O < S =
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
  infix def andThen[O2, S2](next: Stage[O, O2, S2]): Stage[I, O2, S & S2] =
    AndThen(this, next)

  /** Alias for [[andThen]], for readers arriving from ZIO's `>>>` on pipeline-like types. */
  def >>>[O2, S2](next: Stage[O, O2, S2]): Stage[I, O2, S & S2] =
    andThen(next)

  /** Attach a label (and optionally a description) to this stage, replacing any existing one. */
  def named(label: String, description: Maybe[String] = Absent): Stage[I, O, S] =
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

  /** A stage that returns its input unchanged with no effects. */
  def identity[A]: Stage[A, A, Any] =
    Run(a => a)

  /** Lift a pure function into a stage with no effects. */
  def pure[A, B](f: A => B): Stage[A, B, Any] =
    Run(a => f(a))

  /** Lift an effect-tracked function into a stage. */
  def fromKyo[A, B, S](f: A => B < S): Stage[A, B, S] =
    Run(f)
end Stage
