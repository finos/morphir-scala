package morphir.buildkit

import kyo.*
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

  def seal: Result[SealErrors, SealedPipeline[I, O, S]] =
    val summaries                                  = chain.summaries
    val assigned: Chunk[Result[SealError, NodeId]] =
      summaries.zipWithIndex.map { case ((explicit, meta, _), index) =>
        explicit match
          case Present(value) => NodeId.segment(value)
          case Absent         =>
            val slug: Maybe[String] = meta.map(_.label).flatMap(Sealing.slugify)
            slug match
              case Present(value) => Result.succeed(NodeId.unsafe(Chunk(value)))
              case Absent         => Result.succeed(NodeId.unsafe(Chunk(s"node-$index")))
      }
    val segmentErrors = assigned.collect { case Result.Failure(error) => error }
    val ids           = assigned.collect { case Result.Success(id) => id }
    val duplicates    =
      ids
        .groupBy(_.render)
        .toSeq
        .collect { case (rendered, group) if group.size > 1 => (rendered, group) }
        .sortBy(_._1)
        .map((_, group) => SealError.DuplicateNodeId(group.head))
    SealErrors(segmentErrors ++ Chunk.from(duplicates)) match
      case Present(errors) => Result.fail(errors)
      case Absent          => Result.succeed(new SealedPipeline(Sealing.seal(chain, ids)))

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
   * left side fully, then its right side, then pairs the results — and events are emitted in that same order. This
   * slice emits `Succeeded` outcomes only; `Failed`/`Halted`/`Skipped` become emittable with executor-owned halting and
   * conditional branches.
   */
  def execute(input: I): O < (S & Emit[StageEvent]) =
    SealedPipeline.executeChain(sealedChain, input)

object SealedPipeline:

  private def toNodeChain[I, O, S](chain: SealedChain[I, O, S]): NodeChain[I, O, S] =
    chain match
      case SealedChain.Single(elem)       => NodeChain.Single(toDefElem(elem))
      case SealedChain.Append(init, last) => NodeChain.Append(toNodeChain(init), toDefElem(last))

  private def toDefElem[I, O, S](elem: SealedElem[I, O, S]): DefElem[I, O, S] =
    elem match
      case SealedElem.StageNode(id, stage)      => DefElem.StageElem(Present(id.render), stage)
      case SealedElem.ParNode(left, right, zip) => DefElem.ParElem(toNodeChain(left), toNodeChain(right), zip)

  private def executeElem[A, B, S2](
      elem: SealedElem[A, B, S2],
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    elem match
      case SealedElem.StageNode(id, stage) =>
        for
          _   <- Emit.value(StageEvent.Entered(id, stage.meta))
          out <- stage.meta match
            case Present(meta) => Pipeline.provenanceLocal.update(_.append(meta))(stage.run(input))
            case Absent        => stage.run(input)
          _ <- Emit.value(StageEvent.Exited(id, StageOutcome.Succeeded))
        yield out
      case SealedElem.ParNode(left, right, zip) =>
        executeChain(left, input).map(l => executeChain(right, input).map(r => zip(l, r)))

  private def executeChain[A, B, S2](
      chain: SealedChain[A, B, S2],
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    chain match
      case SealedChain.Single(elem) =>
        executeElem(elem, input)
      case SealedChain.Append(init, last) =>
        executeChain(init, input).map(mid => executeElem(last, mid))
end SealedPipeline
