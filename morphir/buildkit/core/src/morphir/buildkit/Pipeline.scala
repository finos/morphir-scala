package morphir.buildkit

import kyo.*
import kyo.kernel.Effect
import morphir.Zippable
import morphir.buildkit.internal.*

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
   * '''Known gap: a fatal `Throwable` still leaves its `NodeStarted` unclosed.''' `Effect.catching`, used by
   * `bracketed`'s own per-node panic handling, filters on `scala.util.control.NonFatal`, so `InterruptedException`,
   * `VirtualMachineError`, `LinkageError`, and `ControlThrowable` are not caught there, and they are not typed
   * `Abort[E]` failures either, so `Abort.tapError` does not observe them. See bead morphir-zdy.2 for the fuller
   * history of this row's evolution.
   */
  def execute(input: I)(using ConcreteTag[E]): O < (Abort[E] & S & Emit[PipelineEvent]) =
    Var.run(SealedPipeline.OpenNodes(Chunk.empty)) {
      for
        _   <- Emit.value(PipelineEvent.RunStarted)
        out <- Abort.tapError[E] { (_: Result.Error[E]) =>
          SealedPipeline.closeOpenNodes.andThen(Emit.value(PipelineEvent.RunFinished(false)))
        }(SealedPipeline.executeChain(sealedChain, Chunk.empty, input))
        _ <- Emit.value(PipelineEvent.RunFinished(true))
      yield out
    }

  /**
   * Run the plan sequentially and '''return''' what happened, rather than halting on it: every node contributes a
   * [[NodeReport]], and a failure becomes data instead of a short-circuit. `Abort[E]` is therefore absent from the
   * result row — this is where the typed interception happens, which is why `ConcreteTag[E]` (what `Abort.run` needs to
   * recognize an `E` at runtime) surfaces once, here.
   *
   * '''Per node.''' The node body runs under `Abort.run[E]`, wrapped in `kyo.kernel.Effect.catching` so a raw non-fatal
   * throw during the body's own eager evaluation is folded in too (see [[SealedPipeline.attempt]]). The three-way
   * `Result` folds to: `Success` → `Succeeded(Provenance.Executed)`, `Failure(e)` → `Failed(Result.Failure(e))`,
   * `Panic(t)` → `Failed(Result.Panic(t))`. A fatal `Throwable` still tears the run — the same gap [[execute]]
   * documents, unchanged here.
   *
   * '''Downstream of a failure.''' Every later node in the same chain reports `Blocked(blockedBy, rootCauses)`, where
   * `blockedBy` names its ''immediate'' predecessor(s) and `rootCauses` names the node(s) that actually failed,
   * propagated by reference through the whole tail rather than re-derived: in `a andThen b andThen c andThen d` with
   * `b` failing, `c` is `Blocked(Chunk(b), Chunk(b))` and `d` is `Blocked(Chunk(c), Chunk(b))`.
   *
   * '''Composite nodes report their own work, not their children's.''' A `branch` node reports `Succeeded` once its
   * predicate evaluated (that is all the node itself does) even when the arm it chose then failed; the arm's own
   * failing node carries the `Failed`. A `fanOut` node reports `Blocked` naming the child element runs that failed,
   * since it genuinely produced no chunk — the typed `Failed` again lives on the child node that raised it. That keeps
   * [[PipelineReport#failed]] pointing at origins and [[PipelineReport#blocked]] at consequences.
   *
   * '''Fan-out children.''' Each element's run contributes its own reports under `parent/<index>/<childId>`, in input
   * order, carrying '''the parent fan-out node's ordinal''' — they are not static plan nodes, so they have no ordinal
   * of their own, and sharing the parent's collates them immediately after it. Identity here is positional; keyed
   * identity is a later task.
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
   * (`Blocked`, `Cancelled`, `Skipped`) — exactly the [[PipelineEvent]] contract [[execute]] already keeps. No
   * open-node tracking is needed here, unlike [[execute]]'s own `Var[OpenNodes]`: nothing escapes a node's own fold, so
   * every `NodeStarted` is closed where it was opened.
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
      case SealedElem.FanOutNode(id, each)                  => DefElem.FanOutElem(Present(id.render), toNodeChain(each))
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
   * `Abort.run[E]` intercepts the typed channel and already renders a panic raised through it as `Result.Panic`. The
   * `Effect.catching` wrapper covers what `Abort.run` alone cannot: `body` is forced ''while constructing'' the
   * computation (a `Stage.Run`'s own function is applied eagerly), so a raw non-fatal throw from that application would
   * otherwise escape the handler entirely and tear the run. Both paths land on the same `Result.Panic`, and the caller
   * folds one shape. Fatal throwables remain uncaught — `Effect.catching` filters on `scala.util.control.NonFatal`, the
   * gap [[SealedPipeline#execute]] documents.
   */
  private def attempt[A, E, S](body: => A < (Abort[E] & S))(using ConcreteTag[E], Frame): Result[E, A] < S =
    Effect.catching(Abort.run[E](body))((ex: Throwable) => Result.Panic(ex): Result[E, A])

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
      case SealedElem.StageNode(id, stage)                  => reportStage(id, stage, prefix, base, gate)
      case SealedElem.ParNode(left, right, zip)             => reportPar(left, right, zip, prefix, base, mode, gate)
      case SealedElem.FanOutNode(id, each)                  => reportFanOut(id, each, prefix, base, mode, gate)
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
          NodeOutcome.Blocked(blockedBy, rootCauses),
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
              reportChain(childChain, childPrefix :+ index.toString, ordinal, mode, childGate).map {
                (childReports, childOut) =>
                  val merged = acc.copy(reports = acc.reports ++ childReports)
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
              else NodeOutcome.Blocked(acc.blockedBy, acc.rootCauses)
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
          NodeOutcome.Blocked(blockedBy, rootCauses),
          Gate.Blocked(Chunk(eventId), rootCauses)
        )
      case Gate.Cancelled =>
        closing(eventId, ordinal, NodeOutcome.Cancelled, Gate.Cancelled)
  end reportFanOut

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

    /** Report the node itself, then walk both arms under the same non-live gate — neither of them ever ran. */
    def bothArms(
        outcome: NodeOutcome[E],
        armGate: Gate[A],
        out: Gate[B]
    ): (Chunk[NodeReport[E]], Gate[B]) < (S & Emit[PipelineEvent]) =
      Emit.value(PipelineEvent.NodeFinished(eventId, outcome.status)).andThen {
        reportChain(trueChain, prefix, trueBase, mode, armGate).map { (trueReports, _) =>
          reportChain(falseChain, prefix, falseBase, mode, armGate).map { (falseReports, _) =>
            (Chunk(NodeReport(eventId, base, outcome)) ++ trueReports ++ falseReports, out)
          }
        }
      }

    gate match
      case Gate.Live(value, _) =>
        Emit.value(PipelineEvent.NodeStarted(eventId, Absent)).andThen {
          attempt[Boolean, E, S](pred(value): Boolean < (Abort[E] & S)).map {
            case Result.Success(taken) =>
              val own     = NodeReport[E](eventId, base, NodeOutcome.Succeeded(Provenance.Executed))
              val armGate = Gate.Live(value, Chunk(eventId))
              // Assembled in execution order — taken arm, then the untaken arm's skips — exactly as the events are
              // emitted. Collating them back into definition order is `runReport`'s own single job, done once, by
              // ordinal; doing it a second time here by hand is how the two would drift.
              Emit.value(PipelineEvent.NodeFinished(eventId, NodeStatus.Succeeded)).andThen {
                val (armChain, armBase, untakenChain, untakenBase) =
                  if taken then (trueChain, trueBase, falseChain, falseBase)
                  else (falseChain, falseBase, trueChain, trueBase)
                reportChain(armChain, prefix, armBase, mode, armGate).map { (armReports, armOut) =>
                  reportSkipped[E](untakenChain, prefix, untakenBase, eventId).map { skipped =>
                    (Chunk(own) ++ armReports ++ skipped, armOut)
                  }
                }
              }
            case Result.Failure(error) =>
              bothArms(
                NodeOutcome.Failed(Result.Failure(error)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId))
              )
            case Result.Panic(ex) =>
              bothArms(
                NodeOutcome.Failed(Result.Panic(ex)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId)),
                Gate.Blocked(Chunk(eventId), Chunk(eventId))
              )
          }
        }
      case Gate.Blocked(blockedBy, rootCauses) =>
        bothArms(
          NodeOutcome.Blocked(blockedBy, rootCauses),
          Gate.Blocked(Chunk(eventId), rootCauses),
          Gate.Blocked(Chunk(eventId), rootCauses)
        )
      case Gate.Cancelled =>
        bothArms(NodeOutcome.Cancelled, Gate.Cancelled, Gate.Cancelled)
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
