package morphir.buildkit

import kyo.*
import morphir.buildkit.internal.*

/**
 * A linear pipeline of stages: either a buildable definition ([[PipelineDef]]) or a validated, executable plan
 * ([[SealedPipeline]]). Construction starts at [[Pipeline.stage]]; validation happens once, at [[seal]].
 */
sealed trait Pipeline[-I, +O, S]:

  /** Validate into an executable plan, accumulating every failure. A [[SealedPipeline]] returns itself. */
  def seal: Result[SealErrors, SealedPipeline[I, O, S]]

  /** Render the node chain: stage descriptions joined with `andThen`. */
  def describe: String

object Pipeline:

  /** The provenance path of the currently executing pipeline run, outermost node first. */
  private[buildkit] val provenance: Local[Chunk[StageMeta]] = Local.init(Chunk.empty)

  /** Entry point: a single-node pipeline whose node id derives from the stage's label, or position. */
  def stage[I, O, S](s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(PipelineNode(Absent, s)))

  /** Entry point with an explicit node id, validated at seal. */
  def stage[I, O, S](id: String, s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(PipelineNode(Present(id), s)))

  /** Entry point with a pre-validated node id (typically a `nodeId"..."` literal). */
  def stage[I, O, S](id: NodeId, s: Stage[I, O, S]): PipelineDef[I, O, S] =
    new PipelineDef(NodeChain.Single(PipelineNode(Present(id.render), s)))
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
    new PipelineDef(NodeChain.Append(chain, PipelineNode(Absent, next)))

  /** Append a stage with an explicit node id, validated at seal. */
  infix def andThen[O2, S2](id: String, next: Stage[O, O2, S2]): PipelineDef[I, O2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, PipelineNode(Present(id), next)))

  /** Append a stage with a pre-validated node id (typically a `nodeId"..."` literal). */
  infix def andThen[O2, S2](id: NodeId, next: Stage[O, O2, S2]): PipelineDef[I, O2, S & S2] =
    new PipelineDef(NodeChain.Append(chain, PipelineNode(Present(id.render), next)))

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
      ids.groupBy(_.render).filter((_, group) => group.size > 1).keys.toSeq.sorted.map { rendered =>
        SealError.DuplicateNodeId(ids.find(_.render == rendered).get)
      }
    SealErrors(segmentErrors ++ Chunk.from(duplicates)) match
      case Present(errors) => Result.fail(errors)
      case Absent          => Result.succeed(new SealedPipeline(chain, ids))

  def describe: String =
    chain.summaries.map((_, _, description) => description).mkString(" andThen ")
end PipelineDef

/**
 * A validated, immutable, shareable execution plan. Per-run state lives in the executor's handler scope, so one plan
 * may run concurrently.
 */
final class SealedPipeline[-I, +O, S] private[buildkit] (
    private[buildkit] val chain: NodeChain[I, O, S],
    val nodeIds: Chunk[NodeId]
) extends Pipeline[I, O, S]:

  def seal: Result[SealErrors, SealedPipeline[I, O, S]] = Result.succeed(this)

  def describe: String =
    chain.summaries.map((_, _, description) => description).mkString(" andThen ")

  /**
   * Run the plan sequentially, emitting [[StageEvent]]s. Deterministic: nodes run in definition order and events are
   * emitted in order. This slice emits `Succeeded` outcomes only; `Failed`/`Halted`/`Skipped` become emittable with
   * executor-owned halting and conditional branches.
   */
  def execute(input: I): O < (S & Emit[StageEvent]) =
    SealedPipeline.executeChain(chain, nodeIds, nodeIds.size, input)

object SealedPipeline:

  private def executeNode[A, B, S2](
      node: PipelineNode[A, B, S2],
      id: NodeId,
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    for
      _   <- Emit.value(StageEvent.Entered(id, node.stage.meta))
      out <- node.stage.meta match
        case Present(meta) => Pipeline.provenance.update(_.append(meta))(node.stage.run(input))
        case Absent        => node.stage.run(input)
      _ <- Emit.value(StageEvent.Exited(id, StageOutcome.Succeeded))
    yield out

  private def executeChain[A, B, S2](
      chain: NodeChain[A, B, S2],
      ids: Chunk[NodeId],
      length: Int,
      input: A
  ): B < (S2 & Emit[StageEvent]) =
    chain match
      case NodeChain.Single(node) =>
        executeNode(node, ids(length - 1), input)
      case NodeChain.Append(init, last) =>
        executeChain(init, ids, length - 1, input).map(mid => executeNode(last, ids(length - 1), mid))
end SealedPipeline
