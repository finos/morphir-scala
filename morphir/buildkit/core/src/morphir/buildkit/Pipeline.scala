package morphir.buildkit

import kyo.*
import kyo.kernel.Effect
import morphir.Zippable
import morphir.buildkit.internal.*

import scala.util.control.NonFatal

/**
 * A linear pipeline of stages: either a buildable definition ([[PipelineDef]]) or a validated, executable plan
 * ([[SealedPipeline]]). Construction starts at [[Pipeline.stage]]; validation happens once, at [[seal]].
 */
sealed trait Pipeline[-I, +O, E, S]:

  /** Validate into an executable plan, accumulating every failure. A [[SealedPipeline]] returns itself. */
  def seal: Result[SealErrors, SealedPipeline[I, O, E, S]]

  /** Render the node chain: stage descriptions joined with `andThen`, forks rendered as `par(left, right)`. */
  def describe: String

  /** The element chain this pipeline is built from — a [[SealedPipeline]] rebuilds it from its sealed chain. */
  private[buildkit] def definitionChain: NodeChain[I, O, E, S]

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
  def stage[I, O, E, S](s: Stage[I, O, E, S]): PipelineDef[I, O, E, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Absent, s)))

  /** Entry point with an explicit node id, validated at seal. */
  def stage[I, O, E, S](id: String, s: Stage[I, O, E, S]): PipelineDef[I, O, E, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Present(id), s)))

  /**
   * Entry point with a pre-validated node id (typically a `nodeId"..."` literal).
   *
   * Stores `id.render` as the explicit-id string: safe only while `NodeId` is always a single segment, as it is in this
   * slice. When multi-segment ids become constructible, revisit so a path is not flattened through `render`.
   */
  def stage[I, O, E, S](id: NodeId, s: Stage[I, O, E, S]): PipelineDef[I, O, E, S] =
    new PipelineDef(NodeChain.Single(DefElem.StageElem(Present(id.render), s)))

  /**
   * Fork two peer pipelines that both receive the same input `I`, pairing their results into a flat `(O1, O2)`.
   * Permanent, arity-fixed companion to the instance [[PipelineDef#par]]: inference is guaranteed since no `Zippable`
   * search is involved.
   */
  def par2[I, O1, O2, E1, E2, S1, S2](
      a: Pipeline[I, O1, E1, S1],
      b: Pipeline[I, O2, E2, S2]
  ): PipelineDef[I, (O1, O2), E1 | E2, S1 & S2] =
    new PipelineDef(NodeChain.Single(DefElem.ParElem(a.definitionChain, b.definitionChain, (x: O1, y: O2) => (x, y))))

  /** Fork three peer pipelines on the same input `I`, pairing their results into a flat `(O1, O2, O3)`. */
  def par3[I, O1, O2, O3, E1, E2, E3, S1, S2, S3](
      a: Pipeline[I, O1, E1, S1],
      b: Pipeline[I, O2, E2, S2],
      c: Pipeline[I, O3, E3, S3]
  ): PipelineDef[I, (O1, O2, O3), E1 | E2 | E3, S1 & S2 & S3] =
    // `ParElem`'s own case type parameters happen to share names (I2, O1, O2, E1, E2, S1, S2) with these method type
    // parameters; that's a naming coincidence, not a conflict, but it leaves Scala 3's bidirectional inference
    // unable to decompose the nested `&`/`|` intersections and unions when the expected result type is pushed down
    // through several levels of `ParElem` nesting — it tries to unify the whole accumulated intersection against one
    // branch's `S` (or union against one branch's `E`), which fails because `NodeChain`/`DefElem` are invariant in
    // both. Explicit type arguments on each `ParElem` call sidestep the guesswork entirely.
    val ab: NodeChain[I, (O1, O2), E1 | E2, S1 & S2] =
      NodeChain.Single(DefElem.ParElem[I, O1, O2, (O1, O2), E1, E2, S1, S2](
        a.definitionChain,
        b.definitionChain,
        (x: O1, y: O2) => (x, y)
      ))
    new PipelineDef(
      NodeChain.Single(DefElem.ParElem[I, (O1, O2), O3, (O1, O2, O3), E1 | E2, E3, S1 & S2, S3](
        ab,
        c.definitionChain,
        (xy: (O1, O2), z: O3) => (xy._1, xy._2, z)
      ))
    )

  /** Fork four peer pipelines on the same input `I`, pairing their results into a flat `(O1, O2, O3, O4)`. */
  def par4[I, O1, O2, O3, O4, E1, E2, E3, E4, S1, S2, S3, S4](
      a: Pipeline[I, O1, E1, S1],
      b: Pipeline[I, O2, E2, S2],
      c: Pipeline[I, O3, E3, S3],
      d: Pipeline[I, O4, E4, S4]
  ): PipelineDef[I, (O1, O2, O3, O4), E1 | E2 | E3 | E4, S1 & S2 & S3 & S4] =
    // See the comment in `par3`: explicit type arguments on each `ParElem` call avoid a constraint-solver failure
    // from nesting `ParElem` constructions with an expected type pushed down from the outside.
    val ab: NodeChain[I, (O1, O2), E1 | E2, S1 & S2] =
      NodeChain.Single(DefElem.ParElem[I, O1, O2, (O1, O2), E1, E2, S1, S2](
        a.definitionChain,
        b.definitionChain,
        (x: O1, y: O2) => (x, y)
      ))
    val abc: NodeChain[I, (O1, O2, O3), E1 | E2 | E3, S1 & S2 & S3] =
      NodeChain.Single(DefElem.ParElem[I, (O1, O2), O3, (O1, O2, O3), E1 | E2, E3, S1 & S2, S3](
        ab,
        c.definitionChain,
        (xy: (O1, O2), z: O3) => (xy._1, xy._2, z)
      ))
    new PipelineDef(
      NodeChain.Single(DefElem.ParElem[I, (O1, O2, O3), O4, (O1, O2, O3, O4), E1 | E2 | E3, E4, S1 & S2 & S3, S4](
        abc,
        d.definitionChain,
        (xyz: (O1, O2, O3), w: O4) => (xyz._1, xyz._2, xyz._3, w)
      ))
    )

  /**
   * The DSL word for a node giving up: write `Pipeline.halt(error)` in a stage body in place of `Abort.fail(error)`. A
   * plain veneer over `Abort.fail` — no custom effect, no `Tag`, no handler of its own — so the executor folds the
   * resulting typed failure into `NodeOutcome.Failed` exactly as it would for any other `Abort[E]` failure raised
   * directly. See "Halt mechanism" in `kb/bundles/morphir/morphir-scala/design/buildkit-task-graph.md` for why a
   * bespoke halting effect is deferred rather than built now.
   */
  def halt[E](error: E)(using Frame): Nothing < Abort[E] = Abort.fail(error)
end Pipeline

/**
 * A buildable, inert pipeline definition. Invalid states (duplicate ids) are representable here and rejected at
 * [[seal]].
 */
final class PipelineDef[-I, +O, E, S] private[buildkit] (
    private[buildkit] val chain: NodeChain[I, O, E, S]
) extends Pipeline[I, O, E, S]:

  /** Append a stage; its node id derives from the stage's label, or position. */
  infix def andThen[O2, E2, S2](next: Stage[O, O2, E2, S2]): PipelineDef[I, O2, E | E2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Absent, next)))

  /** Append a stage with an explicit node id, validated at seal. */
  def andThen[O2, E2, S2](id: String, next: Stage[O, O2, E2, S2]): PipelineDef[I, O2, E | E2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Present(id), next)))

  /**
   * Append a stage with a pre-validated node id (typically a `nodeId"..."` literal).
   *
   * Stores `id.render` as the explicit-id string: safe only while `NodeId` is always a single segment, as it is in this
   * slice. When multi-segment ids become constructible, revisit so a path is not flattened through `render`.
   */
  def andThen[O2, E2, S2](id: NodeId, next: Stage[O, O2, E2, S2]): PipelineDef[I, O2, E | E2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, DefElem.StageElem(Present(id.render), next)))

  /**
   * Fork this pipeline against `other`: both receive the same input `I`, and their outputs pair through `Zippable` so
   * chained `par` calls stay flat (`(A, B)` par `C` is `(A, B, C)`, not `((A, B), C)`).
   *
   * The par element ''replaces'' this pipeline's chain as a single element whose left side is everything accumulated so
   * far — `a.andThen(b).par(c)` forks the whole `a andThen b` against `c`, both fed the pipeline input `I`. That is the
   * natural reading of "fork": everything before the fork runs once per branch, not once total.
   *
   * '''On the `I1 <: I` type parameter.''' Writing `other: Pipeline[I, O2, E2, S2]` directly — reusing this class's own
   * `I` — fails to compile: `Pipeline`'s own first parameter is contravariant, so nesting `I` inside a second use of
   * `Pipeline[-I2, ...]` doubles the contravariance back into a covariant occurrence of `PipelineDef`'s `I`, which `-I`
   * forbids. A fresh, method-scoped `I1` bounded by `I` sidesteps that: at the ordinary call site `I1` is inferred as
   * exactly `I`, and `chain: NodeChain[I, O, E, S]` widens to `NodeChain[I1, O, E, S]` for free, since `I1 <: I` makes
   * that widening exactly what contravariance already allows.
   */
  infix def par[I1 <: I, O2, E2, S2](other: Pipeline[I1, O2, E2, S2])(using
      z: Zippable[O, O2]
  ): PipelineDef[I1, z.Out, E | E2, S & S2] =
    new PipelineDef(NodeChain.Single(DefElem.ParElem(chain, other.definitionChain, z.zip)))

  /**
   * Run `each` once per element of this pipeline's output — an `O` that is itself a `Chunk[A]` — pairing the
   * per-element results back into a `Chunk[B]`. The fan-out node's own id derives like any node: explicit id, else
   * position — there is no label case, since a pipeline (unlike a `Stage`) carries no `StageMeta` to slugify. `each` is
   * sealed once, at this pipeline's own `seal`, and then run once per element at execution.
   *
   * `ev: O <:< Chunk[A]` witnesses that this pipeline's chain, `NodeChain[I, O, E, S]`, is already a
   * `NodeChain[I, Chunk[A], E, S]` by `NodeChain`'s own covariance in `O` — `substituteCo` makes that widening explicit
   * since the compiler cannot see through the abstract `O` on its own.
   */
  def fanOut[A, B, E2, S2](each: Pipeline[A, B, E2, S2])(using
      ev: O <:< Chunk[A]
  ): PipelineDef[I, Chunk[B], E | E2, S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, E, S]](chain),
        DefElem.FanOutElem(Absent, each.definitionChain)
      )
    )

  /** [[fanOut]] with an explicit node id, validated at seal. */
  def fanOut[A, B, E2, S2](id: String, each: Pipeline[A, B, E2, S2])(using
      ev: O <:< Chunk[A]
  ): PipelineDef[I, Chunk[B], E | E2, S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, E, S]](chain),
        DefElem.FanOutElem(Present(id), each.definitionChain)
      )
    )

  /**
   * [[fanOut]] with keyed, rather than positional, child identity: `key` renders each element's stable identifier.
   * Element order (input position) still drives execution and report order — `key` is identity, not order, matching
   * Mill cross values and the survey's position-vs-key verdict. Each rendered key is validated at run time (never at
   * `seal`: a key is a function of runtime values, so `seal` never sees one) against the same rules an explicit node id
   * already follows ([[NodeId.segment]]), and checked for per-parent uniqueness. A bad or duplicate key fails the
   * fan-out node itself — `Failed(Result.Failure(FanOutKeyError(...)))`, with every child unstarted — which is why this
   * overload's declared error unions the child's `E2` with [[FanOutKeyError]], the same technique [[par]]/[[branch]]
   * already use to union two peer error channels. Child event/report ids become `parent/<key>/<childId>`, in place of
   * [[fanOut]]'s `parent/<index>/<childId>`.
   */
  def fanOutKeyed[A, B, E2, S2](key: A => String)(each: Pipeline[A, B, E2, S2])(using
      ev: O <:< Chunk[A]
  ): PipelineDef[I, Chunk[B], E | E2 | FanOutKeyError, S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, E, S]](chain),
        DefElem.FanOutKeyedElem(Absent, key, each.definitionChain)
      )
    )

  /** [[fanOutKeyed]] with an explicit node id, validated at seal. */
  def fanOutKeyed[A, B, E2, S2](id: String, key: A => String)(each: Pipeline[A, B, E2, S2])(using
      ev: O <:< Chunk[A]
  ): PipelineDef[I, Chunk[B], E | E2 | FanOutKeyError, S & S2] =
    new PipelineDef(
      NodeChain.Append(
        ev.substituteCo[[X] =>> NodeChain[I, X, E, S]](chain),
        DefElem.FanOutKeyedElem(Present(id), key, each.definitionChain)
      )
    )

  /**
   * Pick one of two peer pipelines by `pred`, both fed this pipeline's own output and producing the same output type.
   * The untaken arm never runs: at execution, every static node reachable through it emits
   * `NodeFinished(id, NodeStatus.Skipped)` instead, with no matching `NodeStarted`. The branch node's own id derives
   * like any node: explicit id, else position.
   *
   * '''On the `O1 >: O` type parameter.''' Writing `ifTrue: Pipeline[O, O2, E1, S1]` directly — reusing this class's
   * own `O` — fails to compile for the mirror-image reason [[par]]'s own doc explains for `I1 <: I`: `Pipeline`'s
   * second parameter is covariant, so nesting `O` inside a second use of `Pipeline[-I2, ...]` — where `O` fills the
   * '''contravariant''' `I2` slot — doubles back into a contravariant occurrence of `PipelineDef`'s own covariant `O`,
   * which `+O` forbids. A fresh, method-scoped `O1` bounded '''above''' by `O` (rather than below, as `par`'s `I1` is)
   * sidesteps that: at the ordinary call site `O1` is inferred as exactly `O`, and `O1 >: O` makes the widening from
   * `chain: NodeChain[I, O, E, S]`'s own `O` sound wherever `O1` is expected.
   */
  def branch[O1 >: O, O2, E1, E2, S1, S2](pred: O1 => Boolean)(
      ifTrue: Pipeline[O1, O2, E1, S1],
      ifFalse: Pipeline[O1, O2, E2, S2]
  ): PipelineDef[I, O2, E | E1 | E2, S & S1 & S2] =
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
   *
   * These two derived wrapping stages are ordinary, unlabelled nodes like any other: they surface as anonymous `node-N`
   * entries in [[SealedPipeline#nodeIds]], in emitted [[PipelineEvent]]s, and in `toMermaid` diagrams.
   */
  def when[O1 >: O, O2, E2, S2](pred: O1 => Boolean)(arm: Pipeline[O1, O2, E2, S2])
      : PipelineDef[I, Maybe[O2], E | E2, S & S2] =
    val ifTrue: NodeChain[O1, Maybe[O2], E2, S2] =
      NodeChain.Append(arm.definitionChain, DefElem.StageElem(Absent, Stage.pure((o2: O2) => Present(o2))))
    val ifFalse: NodeChain[O1, Maybe[O2], Nothing, Any] =
      NodeChain.Single(DefElem.StageElem(Absent, Stage.pure((_: O1) => Absent)))
    new PipelineDef(
      NodeChain.Append(chain, DefElem.BranchElem(Absent, pred, ifTrue, ifFalse))
    )

  def seal: Result[SealErrors, SealedPipeline[I, O, E, S]] =
    Sealing.sealChain(chain).map(new SealedPipeline(_))

  def describe: String = chain.describe

  private[buildkit] def definitionChain: NodeChain[I, O, E, S] = chain
end PipelineDef

/**
 * How a report run reacts to a node that did not produce a value.
 *
 * The mode only decides whether work that has '''not yet started''' still runs; it never rewrites an outcome. A node
 * that failed reports `Failed` under either mode, and everything downstream of a failure in the same chain is `Blocked`
 * under either mode — there is nothing left to feed it. The two modes differ only where independent work exists:
 *
 *   - `FailFast` — a `par` sibling, or a fan-out element, that had not started when its peer failed is `Cancelled`.
 *   - `KeepGoing` — that same sibling or element runs, and reports its own outcome.
 */
enum RunMode derives CanEqual:
  case FailFast, KeepGoing

/**
 * A validated, immutable, shareable execution plan. Per-run state lives in the executor's handler scope, so one plan
 * may run concurrently.
 */
final class SealedPipeline[-I, +O, E, S] private[buildkit] (
    private[buildkit] val sealedChain: SealedChain[I, O, E, S]
) extends Pipeline[I, O, E, S]:

  /**
   * Node ids of this plan, in definition order. This lists the static plan's own nodes only — a `FanOutNode`
   * contributes its own id, not one per element of whatever chunk it runs against at execution time; those per-element
   * paths only appear in [[PipelineEvent]]s emitted by [[execute]], never here.
   */
  def nodeIds: Chunk[NodeId] = sealedChain.nodeIds

  /**
   * Each node's id paired with its ordinal — its 0-based position in the seal's own definition-order walk, the same
   * walk that assigned the ids. Ordinals are dense over [[nodeIds]] and stable for the life of the plan, so they order
   * a [[PipelineReport]]'s nodes independently of the order the executor happened to reach them in (a `branch` runs one
   * arm and skips the other, so execution order and definition order genuinely differ).
   *
   * [[runReport]] never looks an ordinal up here: it derives each node's own by the identical positional arithmetic the
   * sealer already uses to slice assigned ids per element (`base + init.size`, see
   * [[morphir.buildkit.internal.SealedChain#size]]), so an ordinal cannot desynchronize from the node it numbers. A
   * fan-out's per-element children are '''not''' listed here — they are not static plan nodes; see [[runReport]] for
   * the ordinal they report under.
   */
  def ordinals: Chunk[(NodeId, Int)] = sealedChain.nodeIds.zipWithIndex

  def seal: Result[SealErrors, SealedPipeline[I, O, E, S]] = Result.succeed(this)

  def describe: String = sealedChain.describe

  /**
   * Render this plan as Mermaid `flowchart TD` source: stages as labelled nodes, branches as decision diamonds with
   * both arms drawn, fan-outs as subgraphs annotated per element. See [[morphir.buildkit.internal.MermaidRenderer]] for
   * the full layout contract.
   */
  def toMermaid: String = MermaidRenderer.render(sealedChain)

  private[buildkit] def definitionChain: NodeChain[I, O, E, S] = SealedPipeline.toNodeChain(sealedChain)

  /**
   * Run the plan sequentially, emitting [[PipelineEvent]]s. Deterministic: nodes run in definition order — a fork runs
   * its left side fully, then its right side, then pairs the results — and events are emitted in that same order. A
   * `branch`/`when` node's untaken arm emits `NodeFinished(id, NodeStatus.Skipped)` for every static node it would have
   * run, with no matching `NodeStarted` (see [[PipelineEvent]]'s own contract).
   *
   * '''`RunStarted`/`RunFinished` bracket the whole stream.''' `RunStarted` is emitted before anything else; the run's
   * own outcome — `RunFinished(true)` on success, `RunFinished(false)` on a typed abort or a panic — is always the last
   * event.
   *
   * '''Every `NodeStarted` closes with exactly one `NodeFinished`, including on halt and panic.''' Two independent
   * mechanisms cooperate to guarantee this:
   *
   *   - '''Panics.''' Every bracketing node (a `StageNode`'s own run, a `FanOutNode`'s whole per-element loop, a
   *     `BranchNode`'s decision and taken arm) is wrapped with `kyo.kernel.Effect.catching`
   *     ([[SealedPipeline.bracketed]]): a raw thrown non-fatal `Throwable` (`scala.util.control.NonFatal`, which
   *     `Effect.catching` itself filters on) is caught, `NodeFinished(id, NodeStatus.Failed)` is emitted, and the
   *     exception is rethrown so the panic keeps propagating outward. Nesting composes: a panic deep inside a
   *     `BranchNode`'s taken arm unwinds through every enclosing bracket in turn, each one closing its own id on the
   *     way out.
   *   - '''A typed `Abort[E]`, or a panic once it reaches this boundary.''' Unlike a panic still inside a `bracketed`
   *     call, a typed abort is not a JVM `throw` — it resumes through Kyo's own suspend/resume protocol, so none of the
   *     nested `bracketed` calls close on it by themselves, and a *dynamically scoped* mechanism (a `Local`) can't
   *     stand in either: `Abort`'s own handling is a hard halt (it ignores the failing computation's continuation
   *     entirely — see [[SealedPipeline.closeOpenNodes]] for why that specifically defeats `Local`'s
   *     revert-on-completion scoping), so nothing downstream of the halt point ever gets to observe what a `Local` held
   *     there. Instead, the tracked open ids live in a `Var`, not a `Local`: its own updates are ordinary
   *     suspend/resume occurrences that a handler positioned outside the halt still processes one at a time as they
   *     occur, so its state survives a hard halt intact — the same reason this row's own `Emit[PipelineEvent]` already
   *     survives one (`Emit.run` outside `Abort.run` in every test in this suite). `execute` runs `Var.run` outermost,
   *     wrapping a single `Abort.tapError[E]` around the whole chain: at the moment either kind of failure reaches it,
   *     the var still holds every id that was pushed on entering a bracket and never popped on leaving one, and those
   *     are closed (innermost first, mirroring how nested panics already closed on their own way out) before re-raising
   *     the same failure unchanged — `Abort.tapError`'s own `onError` sees the full `Result.Error[E]` (a typed
   *     `Failure[E]` or a `Panic`), so one handler covers both without needing a second, JVM-`throw`-based wrapper
   *     here. A typed failure stays observable to the caller with its original declared type, the same way
   *     [[morphir.buildkit.Stage#run]] already surfaced it; a panic that reaches this boundary re-raises as a `Panic`
   *     the same way it always did.
   *
   * '''An `Abort` a stage hid inside its own `S` rather than its declared `E` is contained, not just observed.'''
   * `Stage.apply`'s own scaladoc explains why this cannot be forbidden statically: a caller can always pick `E :=
   * Nothing` and fold the real error into `S` instead, and Scala has no negative evidence to reject that. What used to
   * happen then was worse than a merely-untyped escape: `Abort.tapError[E]` only claims a failure its own
   * `ConcreteTag[E]` accepts, so a hidden failure sailed straight past it, leaving `closeOpenNodes` and
   * `RunFinished(false)` un-run — a `NodeStarted` with no matching `NodeFinished`/`RunFinished`, and this method
   * escaping instead of returning to its caller with a value. The single interception below is now
   * [[SealedPipeline.executeIntercepted]], which claims '''every''' `Abort` reaching it (Kyo dispatches every
   * `Abort[X]` through one erased tag, so a handler summoned at `ConcreteTag[Any]` sees a hidden failure exactly as it
   * would the declared one) and only then decides, against this plan's own `ConcreteTag[E]`, whether to re-raise it as
   * the declared `Abort[E]` (byte-identical to what `Abort.tapError[E]` used to do for a value it accepted) or convert
   * it to a panic — [[morphir.buildkit.UndeclaredAbortException]] — carrying the original value. Either way
   * `closeOpenNodes`/`RunFinished(false)` run first, so the event stream stays balanced; a hidden abort now surfaces as
   * a contained panic rather than an unhandled row effect.
   *
   * '''Known gap: a fatal `Throwable` still leaves its `NodeStarted` unclosed.''' `Effect.catching`, used by
   * `bracketed`'s own per-node panic handling, filters on `scala.util.control.NonFatal`, so `InterruptedException`,
   * `VirtualMachineError`, `LinkageError`, and `ControlThrowable` are not caught there, and they are not typed
   * `Abort[E]` failures either, so [[SealedPipeline.executeIntercepted]] does not observe them. See bead morphir-zdy.2
   * for the fuller history of this row's evolution.
   */
  def execute(input: I)(using ConcreteTag[E], Frame): O < (Abort[E] & S & Emit[PipelineEvent]) =
    Var.run(SealedPipeline.OpenNodes(Chunk.empty)) {
      for
        _   <- Emit.value(PipelineEvent.RunStarted)
        out <- SealedPipeline.executeIntercepted[E, O, S](
          SealedPipeline.executeChain(sealedChain, Chunk.empty, input)
        )
        _ <- Emit.value(PipelineEvent.RunFinished(true))
      yield out
    }

  /**
   * Run the plan sequentially and '''return''' what happened, rather than halting on it: every node contributes a
   * [[NodeReport]], and a failure becomes data instead of a short-circuit. `Abort[E]` is therefore absent from the
   * result row — this is where the typed interception happens, which is why `ConcreteTag[E]` (what `Abort.run` needs to
   * recognize an `E` at runtime) surfaces once, here.
   *
   * '''Per node.''' The node body runs under [[SealedPipeline.attempt]], wrapped in `kyo.kernel.Effect.catching` so a
   * raw non-fatal throw during the body's own eager evaluation is folded in too. The three-way `Result` folds to:
   * `Success` → `Succeeded(Provenance.Executed)`, `Failure(e)` → `Failed(Result.Failure(e))`, `Panic(t)` →
   * `Failed(Result.Panic(t))`. A fatal `Throwable` still tears the run — the same gap [[execute]] documents, unchanged
   * here. An `Abort` a stage hid inside its own `S` rather than its declared `E` is '''contained, not tearing the
   * run''': `attempt` claims every `Abort` reaching it (`ConcreteTag[Any]` accepts anything, and Kyo dispatches every
   * `Abort[X]` through one erased tag, so a hidden failure is offered to the same handler as a declared one) and
   * converts one this plan's own `ConcreteTag[E]` does not accept into `Result.Panic(UndeclaredAbortException(..))`
   * instead of leaving it to escape through the return row's own `S` — the gap this paragraph used to describe as
   * unintercepted. `reportStage` then folds that `Result.Panic` exactly like any other panic:
   * `Failed(Result.Panic(..))`.
   *
   * '''Downstream of a failure.''' Every later node in the same chain reports `Blocked(blockedBy, rootCauses)`, where
   * `blockedBy` names its ''immediate'' predecessor(s) and `rootCauses` names the node(s) that actually failed,
   * propagated by reference through the whole tail rather than re-derived: in `a andThen b andThen c andThen d` with
   * `b` failing, `c` is `Blocked(Chunk(b), Chunk(b))` and `d` is `Blocked(Chunk(c), Chunk(b))`.
   *
   * '''Composite nodes report their own work, not their children's.''' A `branch` node reports `Succeeded` once its
   * predicate evaluated (that is all the node itself does) even when the arm it chose then failed; the arm's own
   * failing node carries the `Failed`. A `fanOut` node whose element runs failed reports `Blocked(blockedBy,
   * rootCauses)` — `blockedBy` names the terminal node of each failed element chain (its immediate predecessor, the
   * same reading as anywhere else), `rootCauses` names the nodes that originated those failures; the two coincide only
   * when the child chain is a single node. It is not `Failed`: the typed cause lives on the child node that raised it,
   * and copying it up would report one failure twice. That keeps [[PipelineReport#failed]] pointing at origins and
   * [[PipelineReport#blocked]] at consequences.
   *
   * '''This is a deliberate divergence from [[execute]], and this reading is the intended one.''' Under `execute`, a
   * failure inside a branch arm closes the enclosing branch node's own bracket with `NodeFinished(id, Failed)` — it has
   * to, since the abort unwinds through that bracket and the executor knows nothing else about it. `runReport` sees the
   * whole tree and attributes precisely: the arm's failing node is `Failed`, the branch node that chose it is
   * `Succeeded`. A consumer comparing the two streams will see that difference and should trust the report.
   *
   * A composite reporting `Blocked` is also the one case where a `Blocked` node did start: the fan-out emitted
   * `NodeStarted`, bracketed its children's events, then closed `NodeFinished(id, Blocked)`. See [[PipelineEvent]] and
   * [[NodeOutcome]], both of which license this explicitly — elsewhere `Blocked` does arrive unpaired.
   *
   * '''Fan-out children.''' Each element's run contributes its own reports under `parent/<index>/<childId>`, in input
   * order, carrying '''the parent fan-out node's ordinal''' — every node of the child chain, not just its first. They
   * are not static plan nodes, so they have no ordinal of their own; pinning the whole subtree to the parent's, and
   * relying on the collating sort being stable, is what keeps each element's nodes contiguous, the elements in input
   * order, and the plan's own nodes after the fan-out unaffected. Identity here is positional; keyed identity is a
   * later task.
   *
   * '''Untaken branch arms''' report `Skipped(BranchNotTaken(branch))`, naming the branch node itself — the arm chains
   * have no single id to name, and the branch node is what the reader looks up. `SkipReason.ConditionFalse` is not
   * produced yet: `when` desugars to `branch` (see [[PipelineDef#when]]), so nothing distinguishes the two at this
   * layer.
   *
   * '''Collation and events.''' `nodes` is sorted by ordinal — stably, so a fan-out's children keep their input order
   * under their parent — and is therefore identical across two runs of the same plan on the same input. Events are
   * emitted as the executor walks (the same order [[execute]] uses, which is definition order except that a branch's
   * taken arm precedes the untaken arm's skips): `RunStarted` first, `RunFinished(result.isDefined)` last, one
   * `NodeStarted`/`NodeFinished` pair per node that actually ran, and a lone `NodeFinished` for a node that did not
   * (`Cancelled`, `Skipped`, and `Blocked` other than the composite case above) — exactly the [[PipelineEvent]]
   * contract [[execute]] already keeps, including that a composite node's own `NodeFinished` closes '''after''' every
   * event its children or arms produced. No open-node tracking is needed here, unlike [[execute]]'s own
   * `Var[OpenNodes]`: nothing escapes a node's own fold, so every `NodeStarted` is closed where it was opened.
   *
   * `result` is `Present` only when the terminal node produced a value.
   */
  def runReport(input: I, mode: RunMode = RunMode.FailFast)(using
      ConcreteTag[E],
      Frame
  ): PipelineReport[E, O] < (S & Emit[PipelineEvent]) =
    Emit.value(PipelineEvent.RunStarted).andThen {
      SealedPipeline
        .reportChain[I, O, E, S](sealedChain, Chunk.empty, 0, mode, SealedPipeline.Gate.Live(input, Chunk.empty))
        .map { (reports, gate) =>
          val result: Maybe[O] = gate match
            case SealedPipeline.Gate.Live(value, _) => Present(value)
            case _                                  => Absent
          Emit.value(PipelineEvent.RunFinished(result.isDefined)).andThen(
            PipelineReport(reports.sortBy(_.ordinal), result)
          )
        }
    }
end SealedPipeline

object SealedPipeline:

  private def toNodeChain[I, O, E, S](chain: SealedChain[I, O, E, S]): NodeChain[I, O, E, S] =
    chain match
      case SealedChain.Single(elem)       => NodeChain.Single(toDefElem(elem))
      case SealedChain.Append(init, last) =>
        widenNodeChain(NodeChain.Append(toNodeChain(init), toDefElem(last)))

  private def toDefElem[I, O, E, S](elem: SealedElem[I, O, E, S]): DefElem[I, O, E, S] =
    elem match
      case SealedElem.StageNode(id, stage)      => DefElem.StageElem(Present(id.render), stage)
      case SealedElem.ParNode(left, right, zip) =>
        widenDefElem(DefElem.ParElem(toNodeChain(left), toNodeChain(right), zip))
      case SealedElem.FanOutNode(id, each)           => DefElem.FanOutElem(Present(id.render), toNodeChain(each))
      case SealedElem.FanOutKeyedNode(id, key, each) =>
        widenDefElem(DefElem.FanOutKeyedElem(Present(id.render), key, toNodeChain(each)))
      case SealedElem.BranchNode(id, pred, ifTrue, ifFalse) =>
        widenDefElem(DefElem.BranchElem(Present(id.render), pred, toNodeChain(ifTrue), toNodeChain(ifFalse)))

  /**
   * Wraps the open-node id list so it has a name of its own for `Var`'s `Tag` derivation to key on.
   * `Var[Chunk[NodeId]]` directly crashes that macro at compile time: `NodeId` is `opaque type NodeId = Chunk[String]`,
   * transparent within this defining package, so from the macro's own perspective the payload is `Chunk[Chunk[String]]`
   * — a type constructor applied to itself one level down — which `kyo.internal.TagMacro`'s traversal does not handle.
   * A one-field wrapper case class breaks that self-nesting without changing anything about what's tracked.
   */
  private final case class OpenNodes(ids: Chunk[NodeId])

  /**
   * The ids of every node whose bracket is currently open: pushed by [[executeElem]] right after emitting its own
   * `NodeStarted`, popped either by [[executeElem]] itself (on a normal return) or by [[bracketed]] (on a panic) —
   * whichever of the two closes that id first. Anything still here once [[execute]]'s own `Abort.tapError[E]`
   * intercepts a failure was closed by neither, and gets closed from there.
   *
   * '''Why a `Var` and not a `Local`.''' A `Local` was the first thing tried here, and it does not work: `Local.let`/
   * `update` only restores the outer value on the wrapped computation's own normal completion, by resuming into it —
   * concretely, `ContextEffect.handle`'s own continuation, the code that would apply the restored value, runs only when
   * something calls back into it. `Abort`'s own handling (`ArrowEffect.handleCatching`, underlying every
   * `Abort.run`/`recover`/`tapError`) is a hard halt: on interception it returns a value directly and never invokes
   * that continuation. So a `Local.update` scope wrapping a node that then aborts never gets to hand its value onward —
   * `execute`'s own `onError` runs as a sibling to that scope, not inside it, and reading the local there observes its
   * default, empty value, not whatever was pushed. A `Var` sidesteps this entirely: `Var.update` is an ordinary
   * suspend/resume occurrence, so a `Var.run` handler positioned outside the halt (as `execute`'s own is) processes
   * every occurrence as it happens and keeps the resulting state regardless of how the wrapped computation ends — the
   * same reason this row's own `Emit[PipelineEvent]` already survives a halt (`Emit.run` outside `Abort.run` in every
   * test in this suite; see the pitfall this mirrors in
   * `kb/bundles/programming-language-tooling/kyo-effect-handlers-and-typed-halting.md`).
   */
  private def closeOpenNodes: Unit < (Emit[PipelineEvent] & Var[OpenNodes]) =
    Var.get[OpenNodes].map { openNodes =>
      val openIds = openNodes.ids
      Kyo.foreachDiscard(openIds.reverse) { id =>
        Emit.value(PipelineEvent.NodeFinished(id, NodeStatus.Failed))
      }
    }

  /**
   * [[SealedPipeline#execute]]'s own single interception boundary for `Abort`, replacing what used to be a direct
   * `Abort.tapError[E]` wrapping the whole chain. `Abort.run[Any]` — not `Abort.run[E]` — is what makes this claim
   * every `Abort` in `body`'s row rather than only the declared `E`: Kyo dispatches every `Abort[X]` through one erased
   * tag (`kyo.Abort`'s own `erasedTag`), so a handler is offered any raised failure regardless of which `Abort[X]`
   * produced it, and `ConcreteTag[Any].accepts` always answers `true`, so this handler never lets one pass by. That
   * closes the gap a hidden `Abort` used to open: one folded into a stage's declared `S` instead of its declared `E`
   * (`Stage.apply` cannot forbid that statically) used to sail straight past `Abort.tapError[E]`, whose own
   * `ConcreteTag[E]` rejected it, leaving `closeOpenNodes`/`RunFinished(false)` un-run and the failure escaping this
   * method's own row instead of being returned to the caller through the declared `Abort[E]`.
   *
   * The single `Var[OpenNodes]` this shares with every node bracket (see [[SealedPipeline.executeElem]] and this
   * class's own doc on why a `Var`, not a `Local`, survives a hard halt) is what makes one boundary enough even for a
   * hidden abort nested several levels deep — inside a `fanOut` child, say: every bracket still open when the failure
   * reaches here, at any depth, is in that `Var`, and `closeOpenNodes` closes all of them, innermost first, in one
   * pass.
   *
   * A `Result.Failure(e)` whose value this plan's own `ConcreteTag[E]` still accepts is the declared channel: it is
   * re-raised through `Abort.fail[E]`, byte-identical to what `Abort.tapError[E]` used to do for the only failures it
   * could see. `ConcreteTag[E].unapply` does the narrowing itself — an internal, already-tag-checked cast on kyo's own
   * side — so no cast is written here. One `ConcreteTag[E]` rejects is undeclared: it is converted to
   * `Abort.panic(UndeclaredAbortException(e))` instead, a contained panic in place of a silent escape. A `Result.Panic`
   * is repropagated unchanged, matching what `Abort.tapError`'s own `onError` already did for a panic that reached it.
   */
  private def executeIntercepted[E, A, S](
      body: => A < (Abort[E] & S & Emit[PipelineEvent] & Var[OpenNodes])
  )(using tag: ConcreteTag[E], frame: Frame): A < (Abort[E] & S & Emit[PipelineEvent] & Var[OpenNodes]) =
    Abort.run[Any](body).map {
      case Result.Success(a) => a
      case Result.Panic(ex)  =>
        closeOpenNodes.andThen(Emit.value(PipelineEvent.RunFinished(false))).andThen(Abort.panic[E](ex))
      case Result.Failure(e) =>
        e match
          case tag(typed) =>
            closeOpenNodes.andThen(Emit.value(PipelineEvent.RunFinished(false))).andThen(Abort.fail[E](typed))
          case _ =>
            closeOpenNodes.andThen(Emit.value(PipelineEvent.RunFinished(false))).andThen(
              Abort.panic[E](UndeclaredAbortException(e))
            )
    }

  /**
   * Run `v` — a bracketing node's own value-producing work — so that a raw non-fatal panic pops `eventId` from the
   * open-node `Var` (see [[SealedPipeline.closeOpenNodes]]) and closes its `NodeStarted` with
   * `NodeFinished(id, Failed)` before continuing to propagate. `kyo.kernel.Effect.catching` is the one Kyo RC6
   * primitive that observes an arbitrary, statically unknown effect row's panic without needing to know anything about
   * that row: its own `S2` is fixed here to `Emit[PipelineEvent] & Var[OpenNodes]`, which `v`'s row already carries, so
   * wrapping is row-neutral — no change to the caller's own declared type. It filters on `scala.util.control.NonFatal`
   * (a raw thrown non-fatal `Throwable`), so it does '''not''' see a fatal `Throwable` (`InterruptedException`,
   * `VirtualMachineError`, `LinkageError`, `ControlThrowable`) — that gap is documented on [[SealedPipeline#execute]].
   * A typed `Abort[E]` closes through a different mechanism entirely, also documented there — this method plays no part
   * in it.
   */
  private def bracketed[A, S3](eventId: NodeId)(
      v: => A < (S3 & Emit[PipelineEvent] & Var[OpenNodes])
  ): A < (S3 & Emit[PipelineEvent] & Var[OpenNodes]) =
    Effect.catching(v) { (ex: Throwable) =>
      Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids.filterNot(_ == eventId))).andThen(
        Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Failed)).map(_ => throw ex)
      )
    }

  /**
   * Validate every already-rendered key — the same rules an explicit node id already follows
   * ([[morphir.buildkit.NodeId#segment]]) — then check per-parent uniqueness in input order. Fails on the first
   * [[morphir.buildkit.FanOutKeyError]] encountered: a bad segment before a duplicate, both checked element-by-element
   * in input order, whichever comes first. Deliberately generic only over the rendered `String` keys, not the element
   * type `A` they came from: [[executeElem]]'s own `FanOutKeyedNode` case matches an existential element type it cannot
   * name (a fresh call to a function still parameterized over it fights inference the same way an un-widened
   * `SealedElem`/`NodeChain` value does elsewhere in this file — see the `widen*` helpers' own doc), where rendering
   * first via a plain `.map(key)` member call and validating the resulting `Chunk[String]` sidesteps that entirely.
   * Both [[executeElem]] and [[reportFanOutKeyed]] render then validate this same way, so the two executors can never
   * validate differently.
   */
  private def validateKeys(parent: NodeId, keys: Chunk[String]): Result[FanOutKeyError, Unit] =
    val start: Result[FanOutKeyError, Set[String]] = Result.succeed(Set.empty[String])
    keys.foldLeft(start) { (acc, rendered) =>
      acc match
        case Result.Success(seen) =>
          NodeId.segment(rendered) match
            case Result.Success(_) =>
              if seen.contains(rendered) then Result.fail(FanOutKeyError(parent, rendered, "duplicate key"))
              else Result.succeed(seen + rendered)
            case Result.Failure(SealError.InvalidSegment(_, reason)) =>
              Result.fail(FanOutKeyError(parent, rendered, reason))
            case Result.Failure(SealError.DuplicateNodeId(dupId)) =>
              Result.fail(FanOutKeyError(parent, rendered, s"duplicate node id: ${dupId.render}"))
            case Result.Panic(ex) => Result.panic(ex)
        case Result.Failure(err) => Result.fail(err)
        case Result.Panic(ex)    => Result.panic(ex)
    }.map(_ => ())

  /**
   * Render every element through `key` and validate the result via [[validateKeys]], as one step: `key` is an ordinary
   * Scala function supplied by a pipeline author, so a synchronous throw from it (a bug in the key function, not a Kyo
   * effect) must fold into `Result.Panic` here rather than escape as a raw exception. Rendering happens ''before'' any
   * Kyo effect is constructed, so neither executor's own per-node interception reaches it on its own — `bracketed`'s
   * `Effect.catching` in [[executeElem]] only wraps the computation `key` already finished producing, and `attempt`'s
   * `Effect.catching` in [[reportStage]] guards a stage body, not this fan-out's own key-rendering step. This is the
   * shared choke point both [[executeElem]] and [[reportFanOutKeyed]] route every key through instead, the same reason
   * [[validateKeys]] itself is shared: the two executors can never diverge on either step.
   */
  private def renderAndValidateKeys[A](
      parent: NodeId,
      key: A => String,
      elements: Chunk[A]
  ): Result[FanOutKeyError, Chunk[String]] =
    try
      val renderedKeys = elements.map(key)
      validateKeys(parent, renderedKeys).map(_ => renderedKeys)
    catch case NonFatal(ex) => Result.panic(ex)

  /**
   * Run `elem` on `input`, qualifying every event id it emits with `prefix` — the segments of every enclosing fan-out
   * node, outermost first. A plain top-level run passes `Chunk.empty`; a `FanOutNode` extends `prefix` with its own
   * segment and the running element's index before running `each` on that element, so a doubly-nested fan-out's event
   * ids carry the full path.
   *
   * A `FanOutNode` has an id of its own — unlike a `ParNode`'s two sides, which carry none — so it emits `NodeStarted`
   * and `NodeFinished` around the whole per-element loop, bracketing every child event: `NodeStarted(fo)`, then each
   * element's own `NodeStarted`/`NodeFinished` pair under `fo/<index>/...`, then `NodeFinished(fo)`. With zero elements
   * the bracket still fires — `NodeStarted(fo)` immediately followed by `NodeFinished(fo)`, with no child events
   * between — so a fan-out's own lifecycle is always observable even when it has nothing to iterate.
   */
  private def executeElem[A, B, E2, S2](
      elem: SealedElem[A, B, E2, S2],
      prefix: Chunk[String],
      input: A
  ): B < (Abort[E2] & S2 & Emit[PipelineEvent] & Var[OpenNodes]) =
    elem match
      case SealedElem.StageNode(id, stage) =>
        val eventId = NodeId.unsafe(prefix ++ id.segments)
        for
          _   <- Emit.value(PipelineEvent.NodeStarted(eventId, stage.meta))
          _   <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids :+ eventId))
          out <- bracketed(eventId) {
            stage.meta match
              case Present(meta) => Pipeline.provenanceLocal.update(_.append(meta))(stage.run(input))
              case Absent        => stage.run(input)
          }
          _ <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids.filterNot(_ == eventId)))
          _ <- Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded))
        yield out
      case SealedElem.ParNode(left, right, zip) =>
        executeChain(left, prefix, input).map(l => executeChain(right, prefix, input).map(r => zip(l, r)))
      case SealedElem.FanOutNode(id, each) =>
        val eventId     = NodeId.unsafe(prefix ++ id.segments)
        val childPrefix = prefix ++ id.segments
        for
          _      <- Emit.value(PipelineEvent.NodeStarted(eventId, Absent))
          _      <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids :+ eventId))
          result <- bracketed(eventId) {
            Kyo.foreach(input.zipWithIndex) { case (element, index) =>
              executeChain(each, childPrefix :+ index.toString, element)
            }
          }
          _ <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids.filterNot(_ == eventId)))
          _ <- Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded))
        yield result
      case SealedElem.FanOutKeyedNode(id, key, each) =>
        val eventId     = NodeId.unsafe(prefix ++ id.segments)
        val childPrefix = prefix ++ id.segments
        for
          _      <- Emit.value(PipelineEvent.NodeStarted(eventId, Absent))
          _      <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids :+ eventId))
          result <- bracketed(eventId) {
            renderAndValidateKeys(eventId, key, input) match
              case Result.Success(renderedKeys) =>
                Kyo.foreach(input.zip(renderedKeys)) { case (element, renderedKey) =>
                  executeChain(each, childPrefix :+ renderedKey, element)
                }
              case Result.Failure(err) => Abort.fail(err)
              case Result.Panic(ex)    => throw ex
          }
          _ <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids.filterNot(_ == eventId)))
          _ <- Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded))
        yield result
      case SealedElem.BranchNode(id, pred, ifTrue, ifFalse) =>
        val eventId                                                               = NodeId.unsafe(prefix ++ id.segments)
        val whenTrue: B < (Abort[E2] & S2 & Emit[PipelineEvent] & Var[OpenNodes]) =
          executeChain(ifTrue, prefix, input).map { (result: B) =>
            emitSkips(ifFalse, prefix).map(_ => result)
          }
        val whenFalse: B < (Abort[E2] & S2 & Emit[PipelineEvent] & Var[OpenNodes]) =
          executeChain(ifFalse, prefix, input).map { (result: B) =>
            emitSkips(ifTrue, prefix).map(_ => result)
          }
        for
          _   <- Emit.value(PipelineEvent.NodeStarted(eventId, Absent))
          _   <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids :+ eventId))
          out <- bracketed(eventId)(if pred(input) then whenTrue else whenFalse)
          _   <- Var.updateDiscard[OpenNodes](s => OpenNodes(s.ids.filterNot(_ == eventId)))
          _   <- Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded))
        yield out

  /**
   * Emit `NodeFinished(id, NodeStatus.Skipped)` for every static node reachable through `chain` — its own
   * [[SealedChain#nodeIds]], qualified with `prefix` the same way a live node's own event id would be, with no matching
   * `NodeStarted` (see [[PipelineEvent]]'s own contract). A nested fan-out contributes only its own id (not one per
   * element: an unrun fan-out has no elements to count), and a nested branch contributes its own id followed by both of
   * *its* arms' ids in turn, recursively — nothing under an untaken arm ever ran, all the way down, except through a
   * fan-out's per-element children, whose count doesn't exist unexecuted.
   */
  private def emitSkips[A, B, E2, S2](
      chain: SealedChain[A, B, E2, S2],
      prefix: Chunk[String]
  ): Unit < Emit[PipelineEvent] =
    Kyo.foreachDiscard(chain.nodeIds) { id =>
      Emit.value(PipelineEvent.NodeFinished(NodeId.unsafe(prefix ++ id.segments), NodeStatus.Skipped))
    }

  private def executeChain[A, B, E2, S2](
      chain: SealedChain[A, B, E2, S2],
      prefix: Chunk[String],
      input: A
  ): B < (Abort[E2] & S2 & Emit[PipelineEvent] & Var[OpenNodes]) =
    chain match
      case SealedChain.Single(elem) =>
        executeElem(elem, prefix, input)
      case SealedChain.Append(init, last) =>
        executeChain(init, prefix, input).map(mid => executeElem(last, prefix, mid))

  // ---------------------------------------------------------------------------------------------------------------
  // The report executor ([[SealedPipeline#runReport]]).
  //
  // Every helper below is typed at the *pipeline's* own `E`/`S`, never at a node's existential ones: a per-node
  // `ConcreteTag[E2]` is not summonable inside a GADT match, so each recursive step widens the node it is about to run
  // to the pipeline-level row first (the `widenSealed*` helpers, whose own doc explains why that widening is a proof
  // rather than an assumption) and only then intercepts it with `Abort.run[E]` under `runReport`'s single, concrete
  // `ConcreteTag[E]`.
  // ---------------------------------------------------------------------------------------------------------------

  /**
   * What flows from one node to the next during a report run — the executor's whole per-node state, threaded as a value
   * rather than held in a handler.
   *
   *   - `Live` carries the value produced so far and `exits`, the id(s) of the node(s) that produced it: the immediate
   *     predecessor(s) of whatever runs next.
   *   - `Blocked` carries what the next node's own `NodeOutcome.Blocked` must say — `blockedBy` (its immediate
   *     predecessor, re-pointed at each step) and `rootCauses` (the originating failure(s), carried unchanged the whole
   *     way down, never re-derived).
   *   - `Cancelled` marks work that a `RunMode.FailFast` peer failure withdrew before it started.
   */
  private enum Gate[+A]:
    case Live(value: A, exits: Chunk[NodeId])
    case Blocked(blockedBy: Chunk[NodeId], rootCauses: Chunk[NodeId])
    case Cancelled

  private object Gate:
    /** The id(s) a successor of this segment should name as its immediate predecessor(s). */
    def exitsOf(gate: Gate[Any]): Chunk[NodeId] =
      gate match
        case Live(_, exits) => exits
        case Blocked(by, _) => by
        case Gate.Cancelled => Chunk.empty

    /** The originating failure(s) behind this segment, if any. */
    def rootsOf(gate: Gate[Any]): Chunk[NodeId] =
      gate match
        case Blocked(_, roots) => roots
        case _                 => Chunk.empty

    /**
     * Join the two sides of a fork where at least one side produced no value: `Blocked` when either side traces back to
     * a real failure (a cancelled side contributes no predecessor of its own, having never run), `Cancelled` only when
     * neither does — which is exactly the case where the fork itself was withdrawn.
     */
    def join(left: Gate[Any], right: Gate[Any]): Gate[Nothing] =
      val roots = (rootsOf(left) ++ rootsOf(right)).distinct
      if roots.isEmpty then Cancelled
      else Blocked((exitsOf(left) ++ exitsOf(right)).distinct, roots)
  end Gate

  /** A fan-out's per-element accumulation: child reports and values so far, plus what failed, if anything. */
  private final case class FanOutAcc[E, B](
      reports: Chunk[NodeReport[E]],
      values: Chunk[B],
      blockedBy: Chunk[NodeId],
      rootCauses: Chunk[NodeId]
  )

  /**
   * Run one node's own body and fold every way it can end into a `Result`.
   *
   * `Abort.run[Any]` — not `Abort.run[E]` — intercepts the channel: Kyo dispatches every `Abort[X]` through one erased
   * tag (see `kyo.Abort`'s own `erasedTag`), so a handler is offered any failure reached, regardless of which
   * `Abort[X]` in the row raised it, and only its own `ConcreteTag` decides whether to claim it —
   * `ConcreteTag[Any].accepts` always answers `true`, so this handler claims everything, including a failure a stage
   * hid inside its declared `S` rather than its declared `E` (`Stage.apply` cannot forbid that statically; see
   * [[morphir.buildkit.UndeclaredAbortException]]). The result is folded once, right here, against the caller's own
   * `ConcreteTag[E]`: a value it still accepts is the declared channel, re-raised as `Result.Failure[E]` exactly as
   * `Abort.run[E]` used to render it directly; one it rejects came from a hidden `Abort` and becomes
   * `Result.Panic(UndeclaredAbortException(..))` instead of escaping through `S` unhandled — the gap
   * [[SealedPipeline#runReport]]'s own scaladoc used to document. `ConcreteTag[E].unapply` does the narrowing itself
   * (an internal, already-tag-checked cast on kyo's side), so no cast is needed here. The `Effect.catching` wrapper
   * covers what `Abort.run` alone cannot: `body` is forced ''while constructing'' the computation (a `Stage.Run`'s own
   * function is applied eagerly), so a raw non-fatal throw from that application would otherwise escape the handler
   * entirely and tear the run. Both paths land on the same `Result.Panic`, and the caller folds one shape. Fatal
   * throwables remain uncaught — `Effect.catching` filters on `scala.util.control.NonFatal`, the gap
   * [[SealedPipeline#execute]] documents.
   */
  private def attempt[A, E, S](body: => A < (Abort[E] & S))(using tag: ConcreteTag[E], frame: Frame): Result[E, A] < S =
    Effect.catching(Abort.run[Any](body))((ex: Throwable) => Result.Panic(ex): Result[Any, A]).map {
      case Result.Success(a) => Result.Success(a)
      case Result.Panic(ex)  => Result.Panic(ex)
      case Result.Failure(e) =>
        e match
          case tag(typed) => Result.Failure(typed)
          case _          => Result.Panic(UndeclaredAbortException(e))
    }

  /**
   * Assert that a [[morphir.buildkit.FanOutKeyError]] conforms to a `reportFanOutKeyed` call's own top-level `E` — true
   * by construction whenever this is reached, for the same reason the `widen*` family in [[morphir.buildkit.internal]]
   * is sound: [[morphir.buildkit.internal.DefElem.FanOutKeyedElem]] unions `FanOutKeyError` into its own declared error
   * the moment [[morphir.buildkit.PipelineDef#fanOutKeyed]] appends it, and that union folds into the pipeline's own
   * flattened `E` the same way every other node's declared error already does. The GADT match that proves it — inside
   * [[reportElem]], narrowing `SealedElem.FanOutKeyedNode`'s own `E2 | FanOutKeyError` against the outer `E` — does not
   * survive into [[reportFanOutKeyed]]'s own separately generic signature (its `E` is a fresh method type parameter,
   * not the narrowed one), so the proof is re-asserted here instead, the same "already established at construction, not
   * mechanically visible this many calls removed" shape `widenSealedChain`/`widenSealedElem` already document.
   */
  // Sound only so long as reportElem's `FanOutKeyedNode` arm is this method's one and only caller — that call
  // site is where the GADT match above actually proves the cast.
  private def widenFanOutKeyError[E](err: FanOutKeyError): E = err.asInstanceOf[E]

  /** Emit a node's closing event and pair its report with the gate it hands downstream. */
  private def closing[E, B](
      id: NodeId,
      ordinal: Int,
      outcome: NodeOutcome[E],
      out: Gate[B]
  )(using Frame): (Chunk[NodeReport[E]], Gate[B]) < Emit[PipelineEvent] =
    Emit.value(PipelineEvent.NodeFinished(id, outcome.status)).andThen(
      (Chunk(NodeReport(id, ordinal, outcome)), out)
    )

  private def reportChain[A, B, E, S](
      chain: SealedChain[A, B, E, S],
      prefix: Chunk[String],
      base: Int,
      mode: RunMode,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    chain match
      case SealedChain.Single(elem)       => reportElem(elem, prefix, base, mode, gate)
      case SealedChain.Append(init, last) => reportAppend(init, last, prefix, base, mode, gate)

  /**
   * The `Append` case, split out so the chain's existential middle type `M` can be named: `init` runs from `base`, and
   * `last` from `base + init.size` — the same positional split [[morphir.buildkit.internal.Sealing.pairChain]] uses to
   * hand each element its own assigned ids, which is what makes the ordinal derived here the one
   * [[SealedPipeline#ordinals]] reports.
   */
  private def reportAppend[A, M, B, E, S](
      init: SealedChain[A, M, ?, ?],
      last: SealedElem[M, B, ?, ?],
      prefix: Chunk[String],
      base: Int,
      mode: RunMode,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    val initChain = widenSealedChain[A, M, E, S](init)
    reportChain(initChain, prefix, base, mode, gate).map { (initReports, midGate) =>
      reportElem(widenSealedElem[M, B, E, S](last), prefix, base + initChain.size, mode, midGate).map {
        (lastReports, outGate) => (initReports ++ lastReports, outGate)
      }
    }
  end reportAppend

  private def reportElem[A, B, E, S](
      elem: SealedElem[A, B, E, S],
      prefix: Chunk[String],
      base: Int,
      mode: RunMode,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    elem match
      case SealedElem.StageNode(id, stage)           => reportStage(id, stage, prefix, base, gate)
      case SealedElem.ParNode(left, right, zip)      => reportPar(left, right, zip, prefix, base, mode, gate)
      case SealedElem.FanOutNode(id, each)           => reportFanOut(id, each, prefix, base, mode, gate)
      case SealedElem.FanOutKeyedNode(id, key, each) =>
        reportFanOutKeyed(id, key, each, prefix, base, mode, gate)
      case SealedElem.BranchNode(id, pred, ifTrue, ifFalse) =>
        reportBranch(id, pred, ifTrue, ifFalse, prefix, base, mode, gate)

  private def reportStage[A, B, E, S](
      id: NodeId,
      stage: Stage[A, B, E, S],
      prefix: Chunk[String],
      ordinal: Int,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    val eventId = NodeId.unsafe(prefix ++ id.segments)
    gate match
      case Gate.Live(value, _) =>
        Emit.value(PipelineEvent.NodeStarted(eventId, stage.meta)).andThen {
          attempt[B, E, S] {
            stage.meta match
              case Present(meta) => Pipeline.provenanceLocal.update(_.append(meta))(stage.run(value))
              case Absent        => stage.run(value)
          }.map { ran =>
            val (outcome, out): (NodeOutcome[E], Gate[B]) = ran match
              case Result.Success(produced) =>
                (NodeOutcome.Succeeded(Provenance.Executed), Gate.Live(produced, Chunk(eventId)))
              case Result.Failure(error) =>
                (NodeOutcome.Failed(Result.Failure(error)), Gate.Blocked(Chunk(eventId), Chunk(eventId)))
              case Result.Panic(ex) =>
                (NodeOutcome.Failed(Result.Panic(ex)), Gate.Blocked(Chunk(eventId), Chunk(eventId)))
            closing(eventId, ordinal, outcome, out)
          }
        }
      case Gate.Blocked(blockedBy, rootCauses) =>
        closing(
          eventId,
          ordinal,
          NodeOutcome.Blocked(Causes.unsafe(blockedBy), Causes.unsafe(rootCauses)),
          Gate.Blocked(Chunk(eventId), rootCauses)
        )
      case Gate.Cancelled =>
        closing(eventId, ordinal, NodeOutcome.Cancelled, Gate.Cancelled)
  end reportStage

  /**
   * Both sides of a fork see the same incoming gate — a fork feeds both branches the pipeline's own input, so the right
   * side never depends on the left's result. The one thing the left side's outcome decides is whether the right side
   * gets to run at all: under `FailFast`, a left failure withdraws a right side that had not started; under `KeepGoing`
   * it does not.
   */
  private def reportPar[A, O1, O2, B, E, S](
      left: SealedChain[A, O1, ?, ?],
      right: SealedChain[A, O2, ?, ?],
      zip: (O1, O2) => B,
      prefix: Chunk[String],
      base: Int,
      mode: RunMode,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    val leftChain = widenSealedChain[A, O1, E, S](left)
    reportChain(leftChain, prefix, base, mode, gate).map { (leftReports, leftGate) =>
      val rightGate: Gate[A] = (gate, leftGate) match
        case (Gate.Live(_, _), Gate.Live(_, _))               => gate
        case (Gate.Live(_, _), _) if mode == RunMode.FailFast => Gate.Cancelled
        case _                                                => gate
      reportChain(widenSealedChain[A, O2, E, S](right), prefix, base + leftChain.size, mode, rightGate).map {
        (rightReports, rightOut) =>
          val out: Gate[B] = (leftGate, rightOut) match
            case (Gate.Live(l, leftExits), Gate.Live(r, rightExits)) => Gate.Live(zip(l, r), leftExits ++ rightExits)
            case _                                                   => Gate.join(leftGate, rightOut)
          (leftReports ++ rightReports, out)
      }
    }
  end reportPar

  /**
   * The fan-out node's own report says whether it produced a chunk; each element's run reports separately, under
   * `parent/<index>/<childId>` and at the parent's own ordinal. Under `FailFast` the first element that fails withdraws
   * the elements after it; under `KeepGoing` every element runs and the parent collects them all.
   */
  private def reportFanOut[A, B, E, S](
      id: NodeId,
      each: SealedChain[A, B, ?, ?],
      prefix: Chunk[String],
      ordinal: Int,
      mode: RunMode,
      gate: Gate[Chunk[A]]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[Chunk[B]]) < (S & Emit[PipelineEvent]) =
    val eventId     = NodeId.unsafe(prefix ++ id.segments)
    val childPrefix = prefix ++ id.segments
    val childChain  = widenSealedChain[A, B, E, S](each)
    gate match
      case Gate.Live(elements, _) =>
        Emit.value(PipelineEvent.NodeStarted(eventId, Absent)).andThen {
          Kyo.foldLeft(elements.zipWithIndex)(FanOutAcc[E, B](Chunk.empty, Chunk.empty, Chunk.empty, Chunk.empty)) {
            (acc, indexed) =>
              val (element, index)   = indexed
              val childGate: Gate[A] =
                if acc.rootCauses.nonEmpty && mode == RunMode.FailFast then Gate.Cancelled
                else Gate.Live(element, Chunk(eventId))
              // The child chain walks its own id namespace from its own base, then every report it produced is
              // pinned to the parent's ordinal. Letting the child's own numbering survive would spread a multi-node
              // child across `ordinal, ordinal + 1, ...`: element 0's second node would then sort after element 1's
              // first (losing input order) and the tail would collide with the plan nodes that follow the fan-out.
              // One ordinal for the whole subtree plus a stable sort is what keeps children contiguous and in order.
              reportChain(childChain, childPrefix :+ index.toString, 0, mode, childGate).map {
                (childReports, childOut) =>
                  val merged = acc.copy(reports = acc.reports ++ childReports.map(_.copy(ordinal = ordinal)))
                  childOut match
                    case Gate.Live(produced, _)  => merged.copy(values = merged.values :+ produced)
                    case Gate.Blocked(by, roots) =>
                      merged.copy(
                        blockedBy = (merged.blockedBy ++ by).distinct,
                        rootCauses = (merged.rootCauses ++ roots).distinct
                      )
                    case Gate.Cancelled => merged
              }
          }.map { acc =>
            val outcome: NodeOutcome[E] =
              if acc.rootCauses.isEmpty then NodeOutcome.Succeeded(Provenance.Executed)
              else NodeOutcome.Blocked(Causes.unsafe(acc.blockedBy), Causes.unsafe(acc.rootCauses))
            val out: Gate[Chunk[B]] =
              if acc.rootCauses.isEmpty then Gate.Live(acc.values, Chunk(eventId))
              else Gate.Blocked(Chunk(eventId), acc.rootCauses)
            Emit.value(PipelineEvent.NodeFinished(eventId, outcome.status)).andThen(
              (Chunk(NodeReport(eventId, ordinal, outcome)) ++ acc.reports, out)
            )
          }
        }
      // An unrun fan-out has no elements to iterate, so only its own node is reported — the same reason an unrun
      // fan-out contributes exactly one `Skipped` under `execute`'s own skip contract.
      case Gate.Blocked(blockedBy, rootCauses) =>
        closing(
          eventId,
          ordinal,
          NodeOutcome.Blocked(Causes.unsafe(blockedBy), Causes.unsafe(rootCauses)),
          Gate.Blocked(Chunk(eventId), rootCauses)
        )
      case Gate.Cancelled =>
        closing(eventId, ordinal, NodeOutcome.Cancelled, Gate.Cancelled)
  end reportFanOut

  /**
   * `reportFanOut` with keyed child identity: every rendered key is validated up front, via [[validateKeys]] — the same
   * helper [[executeElem]]'s own `FanOutKeyedNode` case uses, so the two executors can never validate differently. A
   * bad or duplicate key fails the fan-out node itself, exactly like a `StageNode`'s own body failing: it reports
   * `Failed(Result.Failure(FanOutKeyError(...)))` and no child ever starts — no child reports, no child events. Only
   * once every key validates does the per-element loop run, under `parent/<key>/<childId>` in place of
   * `parent/<index>/<childId>`; element order (input position) still drives the loop and the report order, exactly as
   * in [[reportFanOut]] — `key` is identity, not order.
   */
  private def reportFanOutKeyed[A, B, E, S](
      id: NodeId,
      key: A => String,
      each: SealedChain[A, B, ?, ?],
      prefix: Chunk[String],
      ordinal: Int,
      mode: RunMode,
      gate: Gate[Chunk[A]]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[Chunk[B]]) < (S & Emit[PipelineEvent]) =
    val eventId     = NodeId.unsafe(prefix ++ id.segments)
    val childPrefix = prefix ++ id.segments
    val childChain  = widenSealedChain[A, B, E, S](each)
    gate match
      case Gate.Live(elements, _) =>
        Emit.value(PipelineEvent.NodeStarted(eventId, Absent)).andThen {
          renderAndValidateKeys(eventId, key, elements) match
            case Result.Failure(err) =>
              closing(
                eventId,
                ordinal,
                NodeOutcome.Failed(Result.Failure(widenFanOutKeyError[E](err))),
                Gate.Blocked(Chunk(eventId), Chunk(eventId))
              )
            case Result.Panic(ex) =>
              closing(
                eventId,
                ordinal,
                NodeOutcome.Failed(Result.Panic(ex)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId))
              )
            case Result.Success(renderedKeys) =>
              Kyo.foldLeft(elements.zip(renderedKeys))(FanOutAcc[E, B](
                Chunk.empty,
                Chunk.empty,
                Chunk.empty,
                Chunk.empty
              )) {
                (acc, pair) =>
                  val (element, renderedKey) = pair
                  val childGate: Gate[A]     =
                    if acc.rootCauses.nonEmpty && mode == RunMode.FailFast then Gate.Cancelled
                    else Gate.Live(element, Chunk(eventId))
                  // Same ordinal-pinning rationale as `reportFanOut`: one ordinal for the whole subtree, relying on
                  // the collating sort's stability to keep each element's nodes contiguous and in input order.
                  reportChain(childChain, childPrefix :+ renderedKey, 0, mode, childGate).map {
                    (childReports, childOut) =>
                      val merged = acc.copy(reports = acc.reports ++ childReports.map(_.copy(ordinal = ordinal)))
                      childOut match
                        case Gate.Live(produced, _)  => merged.copy(values = merged.values :+ produced)
                        case Gate.Blocked(by, roots) =>
                          merged.copy(
                            blockedBy = (merged.blockedBy ++ by).distinct,
                            rootCauses = (merged.rootCauses ++ roots).distinct
                          )
                        case Gate.Cancelled => merged
                  }
              }.map { acc =>
                val outcome: NodeOutcome[E] =
                  if acc.rootCauses.isEmpty then NodeOutcome.Succeeded(Provenance.Executed)
                  else NodeOutcome.Blocked(Causes.unsafe(acc.blockedBy), Causes.unsafe(acc.rootCauses))
                val out: Gate[Chunk[B]] =
                  if acc.rootCauses.isEmpty then Gate.Live(acc.values, Chunk(eventId))
                  else Gate.Blocked(Chunk(eventId), acc.rootCauses)
                Emit.value(PipelineEvent.NodeFinished(eventId, outcome.status)).andThen(
                  (Chunk(NodeReport(eventId, ordinal, outcome)) ++ acc.reports, out)
                )
              }
        }
      // An unrun fan-out has no elements to iterate — and so no keys to validate — so only its own node is reported,
      // mirroring `reportFanOut`'s own unrun case.
      case Gate.Blocked(blockedBy, rootCauses) =>
        closing(
          eventId,
          ordinal,
          NodeOutcome.Blocked(Causes.unsafe(blockedBy), Causes.unsafe(rootCauses)),
          Gate.Blocked(Chunk(eventId), rootCauses)
        )
      case Gate.Cancelled =>
        closing(eventId, ordinal, NodeOutcome.Cancelled, Gate.Cancelled)
  end reportFanOutKeyed

  /**
   * The branch node reports its own decision; the arm it picks reports for itself, and the arm it does not pick reports
   * `Skipped`. When the predicate itself fails, neither arm can be picked, so both report `Blocked` on this node.
   */
  private def reportBranch[A, B, E, S](
      id: NodeId,
      pred: A => Boolean,
      ifTrue: SealedChain[A, B, ?, ?],
      ifFalse: SealedChain[A, B, ?, ?],
      prefix: Chunk[String],
      base: Int,
      mode: RunMode,
      gate: Gate[A]
  )(using ConcreteTag[E], Frame): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
    val eventId    = NodeId.unsafe(prefix ++ id.segments)
    val trueChain  = widenSealedChain[A, B, E, S](ifTrue)
    val falseChain = widenSealedChain[A, B, E, S](ifFalse)
    val trueBase   = base + 1
    val falseBase  = trueBase + trueChain.size

    /**
     * Report the node itself and walk both arms under the same non-live gate — neither of them ever ran.
     *
     * `started` says whether this node emitted its own `NodeStarted`. When it did (its predicate ran and failed), its
     * `NodeFinished` closes '''after''' the arm events it brackets, as [[PipelineEvent]]'s own contract requires. When
     * it did not (this node was itself blocked or cancelled), there is no bracket to close and its lone `NodeFinished`
     * precedes the arms' — it is the cause, they are the consequence.
     */
    def bothArms(
        outcome: NodeOutcome[E],
        armGate: Gate[A],
        out: Gate[B],
        started: Boolean
    ): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
      val close = Emit.value(PipelineEvent.NodeFinished(eventId, outcome.status))
      val arms  =
        reportChain(trueChain, prefix, trueBase, mode, armGate).map { (trueReports, _) =>
          reportChain(falseChain, prefix, falseBase, mode, armGate).map { (falseReports, _) =>
            Chunk(NodeReport(eventId, base, outcome)) ++ trueReports ++ falseReports
          }
        }
      if started then arms.map(reports => close.andThen((reports, out)))
      else close.andThen(arms.map(reports => (reports, out)))
    end bothArms

    gate match
      case Gate.Live(value, _) =>
        Emit.value(PipelineEvent.NodeStarted(eventId, Absent)).andThen {
          attempt[Boolean, E, S](pred(value): Boolean < (Abort[E] & S)).map {
            case Result.Success(taken) =>
              val own     = NodeReport[E](eventId, base, NodeOutcome.Succeeded(Provenance.Executed))
              val armGate = Gate.Live(value, Chunk(eventId))
              val (armChain, armBase, untakenChain, untakenBase) =
                if taken then (trueChain, trueBase, falseChain, falseBase)
                else (falseChain, falseBase, trueChain, trueBase)
              // Reports are assembled in execution order — this node, the taken arm, then the untaken arm's skips —
              // exactly as the events are emitted. Collating them back into definition order is `runReport`'s own
              // single job, done once, by ordinal; doing it a second time here by hand is how the two would drift.
              // This node's own `NodeFinished` closes last: it brackets every event its arms produced, the shape
              // [[PipelineEvent]] documents and [[SealedPipeline#execute]] already emits.
              reportChain(armChain, prefix, armBase, mode, armGate).map { (armReports, armOut) =>
                reportSkipped[E](untakenChain, prefix, untakenBase, eventId).map { skipped =>
                  Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded)).andThen(
                    (Chunk(own) ++ armReports ++ skipped, armOut)
                  )
                }
              }
            case Result.Failure(error) =>
              bothArms(
                NodeOutcome.Failed(Result.Failure(error)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                started = true
              )
            case Result.Panic(ex) =>
              bothArms(
                NodeOutcome.Failed(Result.Panic(ex)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                started = true
              )
          }
        }
      case Gate.Blocked(blockedBy, rootCauses) =>
        bothArms(
          NodeOutcome.Blocked(Causes.unsafe(blockedBy), Causes.unsafe(rootCauses)),
          Gate.Blocked(Chunk(eventId), rootCauses),
          Gate.Blocked(Chunk(eventId), rootCauses),
          started = false
        )
      case Gate.Cancelled =>
        bothArms(NodeOutcome.Cancelled, Gate.Cancelled, Gate.Cancelled, started = false)
  end reportBranch

  /**
   * Report `Skipped(BranchNotTaken(branch))` for every static node reachable through an untaken arm, ordinal-numbered
   * from `base` the same way [[morphir.buildkit.internal.SealedChain#nodeIds]] orders them, and emit one
   * `NodeFinished(id, Skipped)` each with no matching `NodeStarted` — the same shape [[SealedPipeline.emitSkips]]
   * already produces for [[SealedPipeline#execute]]. A nested fan-out contributes only its own id: unexecuted, it has
   * no element count to expand.
   */
  private def reportSkipped[E](
      chain: SealedChain[?, ?, ?, ?],
      prefix: Chunk[String],
      base: Int,
      branch: NodeId
  )(using Frame): Chunk[NodeReport[E]] < Emit[PipelineEvent] =
    val reports = chain.nodeIds.zipWithIndex.map { (id, index) =>
      NodeReport[E](
        NodeId.unsafe(prefix ++ id.segments),
        base + index,
        NodeOutcome.Skipped(SkipReason.BranchNotTaken(branch))
      )
    }
    Kyo.foreachDiscard(reports)(report => Emit.value(PipelineEvent.NodeFinished(report.id, NodeStatus.Skipped)))
      .andThen(reports)
  end reportSkipped
end SealedPipeline
