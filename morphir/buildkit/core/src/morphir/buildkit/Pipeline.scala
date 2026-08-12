package morphir.buildkit

import kyo.*
import kyo.kernel.Effect
import morphir.Zippable
import morphir.buildkit.internal.*

/**
 * A linear pipeline of stages: either a buildable definition ([[PipelineDef]]) or a validated, executable plan
 * ([[SealedPipeline]]). Construction starts at [[Pipeline.stage]]; validation happens once, at [[seal]].
 */
sealed trait Pipeline[-I, +O, S]:

  /** Validate into an executable plan, accumulating every failure. A [[SealedPipeline]] returns itself. */
  def seal: Result[SealErrors, SealedPipeline[I, O, S]]

  /** Render the node chain: stage descriptions joined with `andThen`, forks rendered as `par(left, right)`. */
  def describe: String

  /** The element chain this pipeline is built from — a [[SealedPipeline]] rebuilds it from its sealed chain. */
  private[buildkit] def definitionChain: NodeChain[I, O, S]

object Pipeline:

  /** The provenance path of the currently executing pipeline run, outermost node first. */
  private[buildkit] val provenanceLocal: Local[Chunk[StageMeta]] = Local.init(Chunk.empty)

  /**
   * The provenance path of the currently executing pipeline run, outermost node first.
   *
   * A stage reads this during its own execution to see the labelled ancestors that ran before it. Outside pipeline
   * execution — including in a stage that runs standalone, never wrapped in a [[Pipeline]] — it is empty.
   */
  def provenance: Chunk[StageMeta] < Any = provenanceLocal.get

  /** Entry point: a single-node pipeline whose node id derives from the stage's label, or position. */
  def stage[I, O, S](s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Absent, s)))

  /** Entry point with an explicit node id, validated at seal. */
  def stage[I, O, S](id: String, s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Present(id), s)))

  /**
   * Entry point with a pre-validated node id (typically a `nodeId"..."` literal).
   *
   * Stores `id.render` as the explicit-id string: safe only while `NodeId` is always a single segment, as it is in this
   * slice. When multi-segment ids become constructible, revisit so a path is not flattened through `render`.
   */
  def stage[I, O, S](id: NodeId, s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Present(id.render), s)))

  /**
   * Fork two peer pipelines that both receive the same input `I`, pairing their results into a flat `(O1, O2)`.
   * Permanent, arity-fixed companion to the instance [[PipelineDef#par]]: inference is guaranteed since no `Zippable`
   * search is involved.
   */
  def par2[I, O1, O2, S1, S2](
      a: Pipeline[I, O1, S1],
      b: Pipeline[I, O2, S2]
  ): PipelineDef[I, (O1, O2), S1 & S2] =
    new PipelineDef(NodeChain.Single(DefElem.ParElem(a.definitionChain, b.definitionChain, (x: O1, y: O2) => (x, y))))

  /** Fork three peer pipelines on the same input `I`, pairing their results into a flat `(O1, O2, O3)`. */
  def par3[I, O1, O2, O3, S1, S2, S3](
      a: Pipeline[I, O1, S1],
      b: Pipeline[I, O2, S2],
      c: Pipeline[I, O3, S3]
  ): PipelineDef[I, (O1, O2, O3), S1 & S2 & S3] =
    // `ParElem`'s own case type parameters happen to share names (I2, O1, O2, S1, S2) with these method type
    // parameters; that's a naming coincidence, not a conflict, but it leaves Scala 3's bidirectional inference
    // unable to decompose the nested `&` intersections when the expected result type is pushed down through several
    // levels of `ParElem` nesting — it tries to unify the whole accumulated intersection against one branch's `S`,
    // which fails because `NodeChain`/`DefElem` are invariant in `S`. Explicit type arguments on each `ParElem` call
    // sidestep the guesswork entirely.
    val ab: NodeChain[I, (O1, O2), S1 & S2] =
      NodeChain.Single(DefElem.ParElem[I, O1, O2, (O1, O2), S1, S2](
        a.definitionChain,
        b.definitionChain,
        (x: O1, y: O2) => (x, y)
      ))
    new PipelineDef(
      NodeChain.Single(DefElem.ParElem[I, (O1, O2), O3, (O1, O2, O3), S1 & S2, S3](
        ab,
        c.definitionChain,
        (xy: (O1, O2), z: O3) => (xy._1, xy._2, z)
      ))
    )

  /** Fork four peer pipelines on the same input `I`, pairing their results into a flat `(O1, O2, O3, O4)`. */
  def par4[I, O1, O2, O3, O4, S1, S2, S3, S4](
      a: Pipeline[I, O1, S1],
      b: Pipeline[I, O2, S2],
      c: Pipeline[I, O3, S3],
      d: Pipeline[I, O4, S4]
  ): PipelineDef[I, (O1, O2, O3, O4), S1 & S2 & S3 & S4] =
    // See the comment in `par3`: explicit type arguments on each `ParElem` call avoid a constraint-solver failure
    // from nesting `ParElem` constructions with an expected type pushed down from the outside.
    val ab: NodeChain[I, (O1, O2), S1 & S2] =
      NodeChain.Single(DefElem.ParElem[I, O1, O2, (O1, O2), S1, S2](
        a.definitionChain,
        b.definitionChain,
        (x: O1, y: O2) => (x, y)
      ))
    val abc: NodeChain[I, (O1, O2, O3), S1 & S2 & S3] =
      NodeChain.Single(DefElem.ParElem[I, (O1, O2), O3, (O1, O2, O3), S1 & S2, S3](
        ab,
        c.definitionChain,
        (xy: (O1, O2), z: O3) => (xy._1, xy._2, z)
      ))
    new PipelineDef(
      NodeChain.Single(DefElem.ParElem[I, (O1, O2, O3), O4, (O1, O2, O3, O4), S1 & S2 & S3, S4](
        abc,
        d.definitionChain,
        (xyz: (O1, O2, O3), w: O4) => (xyz._1, xyz._2, xyz._3, w)
      ))
    )
end Pipeline

/**
 * A buildable, inert pipeline definition. Invalid states (duplicate ids) are representable here and rejected at
 * [[seal]].
 */
final class PipelineDef[-I, +O, S] private[buildkit] (
    private[buildkit] val chain: NodeChain[I, O, S]
) extends Pipeline[I, O, S]:

  /** Append a stage; its node id derives from the stage's label, or position. */
  infix def andThen[O2, S2](next: Stage[O, O2, S2]): PipelineDef[I, O2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Absent, next)))

  /** Append a stage with an explicit node id, validated at seal. */
  def andThen[O2, S2](id: String, next: Stage[O, O2, S2]): PipelineDef[I, O2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Present(id), next)))

  /**
   * Append a stage with a pre-validated node id (typically a `nodeId"..."` literal).
   *
   * Stores `id.render` as the explicit-id string: safe only while `NodeId` is always a single segment, as it is in this
   * slice. When multi-segment ids become constructible, revisit so a path is not flattened through `render`.
   */
  def andThen[O2, S2](id: NodeId, next: Stage[O, O2, S2]): PipelineDef[I, O2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Present(id.render), next)))

  /**
   * Fork this pipeline against `other`: both receive the same input `I`, and their outputs pair through `Zippable` so
   * chained `par` calls stay flat (`(A, B)` par `C` is `(A, B, C)`, not `((A, B), C)`).
   *
   * The par element ''replaces'' this pipeline's chain as a single element whose left side is everything accumulated so
   * far — `a.andThen(b).par(c)` forks the whole `a andThen b` against `c`, both fed the pipeline input `I`. That is the
   * natural reading of "fork": everything before the fork runs once per branch, not once total.
   *
   * '''On the `I1 <: I` type parameter.''' Writing `other: Pipeline[I, O2, S2]` directly — reusing this class's own `I`
   * — fails to compile: `Pipeline`'s own first parameter is contravariant, so nesting `I` inside a second use of
   * `Pipeline[-I2, ...]` doubles the contravariance back into a covariant occurrence of `PipelineDef`'s `I`, which `-I`
   * forbids. A fresh, method-scoped `I1` bounded by `I` sidesteps that: at the ordinary call site `I1` is inferred as
   * exactly `I`, and `chain: NodeChain[I, O, S]` widens to `NodeChain[I1, O, S]` for free, since `I1 <: I` makes that
   * widening exactly what contravariance already allows.
   */
  infix def par[I1 <: I, O2, S2](other: Pipeline[I1, O2, S2])(using
      z: Zippable[O, O2]
  ): PipelineDef[I1, z.Out, S & S2] =
    new PipelineDef(NodeChain.Single(DefElem.ParElem(chain, other.definitionChain, z.zip)))

  /**
   * Run `each` once per element of this pipeline's output — an `O` that is itself a `Chunk[A]` — pairing the
   * per-element results back into a `Chunk[B]`. The fan-out node's own id derives like any node: explicit id, else
   * position — there is no label case, since a pipeline (unlike a `Stage`) carries no `StageMeta` to slugify. `each` is
   * sealed once, at this pipeline's own `seal`, and then run once per element at execution.
   *
   * `ev: O <:< Chunk[A]` witnesses that this pipeline's chain, `NodeChain[I, O, S]`, is already a
   * `NodeChain[I, Chunk[A], S]` by `NodeChain`'s own covariance in `O` — `substituteCo` makes that widening explicit
   * since the compiler cannot see through the abstract `O` on its own.
   */
  def fanOut[A, B, S2](each: Pipeline[A, B, S2])(using ev: O <:< Chunk[A]): PipelineDef[I, Chunk[B], S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, S]](chain),
        DefElem.FanOutElem(Absent, each.definitionChain)
      )
    )

  /** [[fanOut]] with an explicit node id, validated at seal. */
  def fanOut[A, B, S2](id: String, each: Pipeline[A, B, S2])(using
      ev: O <:< Chunk[A]
  ): PipelineDef[I, Chunk[B], S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, S]](chain),
        DefElem.FanOutElem(Present(id), each.definitionChain)
      )
    )

  /**
   * Pick one of two peer pipelines by `pred`, both fed this pipeline's own output and producing the same output type.
   * The untaken arm never runs: at execution, every static node reachable through it emits [[StageEvent.Skipped]]
   * instead. The branch node's own id derives like any node: explicit id, else position.
   *
   * '''On the `O1 >: O` type parameter.''' Writing `ifTrue: Pipeline[O, O2, S1]` directly — reusing this class's own
   * `O` — fails to compile for the mirror-image reason [[par]]'s own doc explains for `I1 <: I`: `Pipeline`'s second
   * parameter is covariant, so nesting `O` inside a second use of `Pipeline[-I2, ...]` — where `O` fills the
   * '''contravariant''' `I2` slot — doubles back into a contravariant occurrence of `PipelineDef`'s own covariant `O`,
   * which `+O` forbids. A fresh, method-scoped `O1` bounded '''above''' by `O` (rather than below, as `par`'s `I1` is)
   * sidesteps that: at the ordinary call site `O1` is inferred as exactly `O`, and `O1 >: O` makes the widening from
   * `chain: NodeChain[I, O, S]`'s own `O` sound wherever `O1` is expected.
   */
  def branch[O1 >: O, O2, S1, S2](pred: O1 => Boolean)(
      ifTrue: Pipeline[O1, O2, S1],
      ifFalse: Pipeline[O1, O2, S2]
  ): PipelineDef[I, O2, S & S1 & S2] =
    new PipelineDef(
      NodeChain.Append(
        chain,
        DefElem.BranchElem(Absent, pred, ifTrue.definitionChain, ifFalse.definitionChain)
      )
    )

  /**
   * Run `arm` when `pred` holds, yielding `Present` of its result; otherwise `Absent`, and `arm` never runs. Derived
   * from [[branch]]: the true arm is `arm` extended with a stage wrapping its result in `Present`, and the false arm is
   * a single stage that ignores its input and returns `Absent`. See [[branch]] for the `O1 >: O` parameter.
   */
  def when[O1 >: O, O2, S2](pred: O1 => Boolean)(arm: Pipeline[O1, O2, S2]): PipelineDef[I, Maybe[O2], S & S2] =
    val ifTrue: NodeChain[O1, Maybe[O2], S2] =
      NodeChain.Append(arm.definitionChain, DefElem.StageElem(Absent, Stage.pure((o2: O2) => Present(o2))))
    val ifFalse: NodeChain[O1, Maybe[O2], Any] =
      NodeChain.Single(DefElem.StageElem(Absent, Stage.pure((_: O1) => Absent)))
    new PipelineDef(
      NodeChain.Append(chain, DefElem.BranchElem(Absent, pred, ifTrue, ifFalse))
    )

  def seal: Result[SealErrors, SealedPipeline[I, O, S]] =
    Sealing.sealChain(chain).map(new SealedPipeline(_))

  def describe: String = chain.describe

  private[buildkit] def definitionChain: NodeChain[I, O, S] = chain
end PipelineDef

/**
 * A validated, immutable, shareable execution plan. Per-run state lives in the executor's handler scope, so one plan
 * may run concurrently.
 */
final class SealedPipeline[-I, +O, S] private[buildkit] (
    private[buildkit] val sealedChain: SealedChain[I, O, S]
) extends Pipeline[I, O, S]:

  /** Node ids of this plan, in definition order. */
  def nodeIds: Chunk[NodeId] = sealedChain.nodeIds

  def seal: Result[SealErrors, SealedPipeline[I, O, S]] = Result.succeed(this)

  def describe: String = sealedChain.describe

  private[buildkit] def definitionChain: NodeChain[I, O, S] = SealedPipeline.toNodeChain(sealedChain)

  /**
   * Run the plan sequentially, emitting [[StageEvent]]s. Deterministic: nodes run in definition order — a fork runs its
   * left side fully, then its right side, then pairs the results — and events are emitted in that same order. A
   * `branch`/`when` node's untaken arm emits `Skipped` for every static node it would have run.
   *
   * '''Panics close their `Entered` with `Exited(id, Failed)`.''' Every bracketing node (a `StageNode`'s own run, a
   * `FanOutNode`'s whole per-element loop, a `BranchNode`'s decision and taken arm) is wrapped with
   * `kyo.kernel.Effect.catching`: a raw thrown `Throwable` is caught, `Exited(id, Failed)` is emitted, and the
   * exception is rethrown so the panic keeps propagating outward exactly as before — `Effect.catching` is
   * effect-row-neutral (`B < (S & S2)` with `S2` fixed to `Emit[StageEvent]` here, which this method's own row already
   * carries), so this needed no change to `execute`'s own signature.
   *
   * '''Known gap: a typed `Abort[E]` short-circuit still leaves its `Entered` unclosed.''' Unlike a panic, a typed
   * `Abort` failure never reaches `Effect.catching`'s `catch` block — it propagates through Kyo's own suspend/resume
   * `ArrowEffect` protocol, not a JVM `throw`. Closing `Entered` for that case would need the executor to intercept a
   * statically unknown `Abort[E]` inside `S` for an abstract, unbounded `E` and re-raise it with its original type —
   * every candidate Kyo RC6 offers for that (`Sync.ensure`, `Scope.ensure`, and every
   * `Abort.run`/`fold`/`recover`/`tapError` shape, including the `Abort[Any]`-widened one) either fails to type-check
   * against an abstract `S`, or requires widening `execute`'s own public row in a way that breaks direct `.eval()`
   * calls across the existing test suite. See the task-4 implementation report for the full evidence. `Halted` remains
   * unemitted for the same reason `Skipped` was before conditional branches: nothing yet triggers it.
   */
  def execute(input: I): O < (S & Emit[StageEvent]) =
    SealedPipeline.executeChain(sealedChain, Chunk.empty, input)

object SealedPipeline:

  private def toNodeChain[I, O, S](chain: SealedChain[I, O, S]): NodeChain[I, O, S] =
    chain match
      case SealedChain.Single(elem)       => NodeChain.Single(toDefElem(elem))
      case SealedChain.Append(init, last) => NodeChain.Append(toNodeChain(init), toDefElem(last))

  private def toDefElem[I, O, S](elem: SealedElem[I, O, S]): DefElem[I, O, S] =
    elem match
      case SealedElem.StageNode(id, stage)      => DefElem.StageElem(Present(id.render), stage)
      case SealedElem.ParNode(left, right, zip) => DefElem.ParElem(toNodeChain(left), toNodeChain(right), zip)
      case SealedElem.FanOutNode(id, each)      => DefElem.FanOutElem(Present(id.render), toNodeChain(each))
      case SealedElem.BranchNode(id, pred, ifTrue, ifFalse) =>
        DefElem.BranchElem(Present(id.render), pred, toNodeChain(ifTrue), toNodeChain(ifFalse))

  /**
   * Run `v` — a bracketing node's own value-producing work — so that a raw panic closes `eventId`'s `Entered` with
   * `Exited(id, Failed)` before continuing to propagate. `kyo.kernel.Effect.catching` is the one Kyo RC6 primitive that
   * observes an arbitrary, statically unknown effect row's panic without needing to know anything about that row: its
   * own `S2` is fixed here to `Emit[StageEvent]`, which `v`'s row already carries, so wrapping is row-neutral —
   * `A < (S2 & Emit[StageEvent])` in, `A < (S2 & Emit[StageEvent])` out, no change to the caller's own declared type.
   * It does '''not''' see a typed `Abort[E]` failure — see [[SealedPipeline#execute]]'s own doc for why that half of
   * the contract is still open.
   */
  private def bracketed[A, S3](eventId: NodeId)(v: => A < (S3 & Emit[StageEvent])): A < (S3 & Emit[StageEvent]) =
    Effect.catching(v) { (ex: Throwable) =>
      Emit.value(StageEvent.Exited(eventId, StageOutcome.Failed)).map(_ => throw ex)
    }

  /**
   * Run `elem` on `input`, qualifying every event id it emits with `prefix` — the segments of every enclosing fan-out
   * node, outermost first. A plain top-level run passes `Chunk.empty`; a `FanOutNode` extends `prefix` with its own
   * segment and the running element's index before running `each` on that element, so a doubly-nested fan-out's event
   * ids carry the full path.
   *
   * A `FanOutNode` has an id of its own — unlike a `ParNode`'s two sides, which carry none — so it emits `Entered` and
   * `Exited` around the whole per-element loop, bracketing every child event: `Entered(fo)`, then each element's own
   * `Entered`/`Exited` pair under `fo/<index>/...`, then `Exited(fo)`. With zero elements the bracket still fires —
   * `Entered(fo)` immediately followed by `Exited(fo)`, with no child events between — so a fan-out's own lifecycle is
   * always observable even when it has nothing to iterate.
   */
  private def executeElem[A, B, S2](
      elem: SealedElem[A, B, S2],
      prefix: Chunk[String],
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    elem match
      case SealedElem.StageNode(id, stage) =>
        val eventId = NodeId.unsafe(prefix ++ id.segments)
        for
          _   <- Emit.value(StageEvent.Entered(eventId, stage.meta))
          out <- bracketed(eventId) {
            stage.meta match
              case Present(meta) => Pipeline.provenanceLocal.update(_.append(meta))(stage.run(input))
              case Absent        => stage.run(input)
          }
          _ <- Emit.value(StageEvent.Exited(eventId, StageOutcome.Succeeded))
        yield out
      case SealedElem.ParNode(left, right, zip) =>
        executeChain(left, prefix, input).map(l => executeChain(right, prefix, input).map(r => zip(l, r)))
      case SealedElem.FanOutNode(id, each) =>
        val eventId     = NodeId.unsafe(prefix ++ id.segments)
        val childPrefix = prefix ++ id.segments
        for
          _      <- Emit.value(StageEvent.Entered(eventId, Absent))
          result <- bracketed(eventId) {
            Kyo.foreach(input.zipWithIndex) { case (element, index) =>
              executeChain(each, childPrefix :+ index.toString, element)
            }
          }
          _ <- Emit.value(StageEvent.Exited(eventId, StageOutcome.Succeeded))
        yield result
      case SealedElem.BranchNode(id, pred, ifTrue, ifFalse) =>
        val eventId                               = NodeId.unsafe(prefix ++ id.segments)
        val whenTrue: B < (S2 & Emit[StageEvent]) =
          executeChain(ifTrue, prefix, input).map { (result: B) =>
            emitSkips(ifFalse, prefix, "predicate was true").map(_ => result)
          }
        val whenFalse: B < (S2 & Emit[StageEvent]) =
          executeChain(ifFalse, prefix, input).map { (result: B) =>
            emitSkips(ifTrue, prefix, "predicate was false").map(_ => result)
          }
        for
          _   <- Emit.value(StageEvent.Entered(eventId, Absent))
          out <- bracketed(eventId)(if pred(input) then whenTrue else whenFalse)
          _   <- Emit.value(StageEvent.Exited(eventId, StageOutcome.Succeeded))
        yield out

  /**
   * Emit [[StageEvent.Skipped]] for every static node reachable through `chain` — its own [[SealedChain#nodeIds]],
   * qualified with `prefix` the same way a live node's own event id would be. A nested fan-out contributes only its own
   * id (not one per element: an unrun fan-out has no elements to count), and a nested branch contributes its own id
   * followed by both of *its* arms' ids in turn, recursively — nothing under an untaken arm ever ran, all the way down,
   * except through a fan-out's per-element children, whose count doesn't exist unexecuted.
   */
  private def emitSkips[A, B, S2](
      chain: SealedChain[A, B, S2],
      prefix: Chunk[String],
      reason: String
  ): Unit < Emit[StageEvent] =
    Kyo.foreachDiscard(chain.nodeIds) { id =>
      Emit.value(StageEvent.Skipped(NodeId.unsafe(prefix ++ id.segments), reason))
    }

  private def executeChain[A, B, S2](
      chain: SealedChain[A, B, S2],
      prefix: Chunk[String],
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    chain match
      case SealedChain.Single(elem) =>
        executeElem(elem, prefix, input)
      case SealedChain.Append(init, last) =>
        executeChain(init, prefix, input).map(mid => executeElem(last, prefix, mid))
end SealedPipeline
