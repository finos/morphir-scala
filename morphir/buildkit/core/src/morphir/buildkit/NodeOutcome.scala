package morphir.buildkit

import kyo.*

/**
 * Indicates how a node's output was obtained.
 *
 * Only `Executed` is produced until incremental builds (intent 0015). `UpToDate` and `FromCache` exist so consumer
 * matches are total from the start.
 */
enum Provenance derives CanEqual:
  case Executed, UpToDate, FromCache

enum NodeStatus derives CanEqual:
  case Succeeded, Failed, Cancelled, Skipped, Blocked

enum SkipReason derives CanEqual:
  case BranchNotTaken(branch: NodeId)
  case ConditionFalse

/**
 * A non-empty chunk of node ids — the shape [[NodeOutcome.Blocked]] needs for both `blockedBy` and `rootCauses`, so the
 * invariant a `Blocked` outcome carries is structural rather than merely documented.
 *
 * [[SealedPipeline]] is the one intended producer: every `Blocked` it reports is built from a chunk it has already
 * established as non-empty — its own event id (`Chunk(eventId)`), or a chunk propagated from an upstream `Blocked` that
 * was itself built the same way — so it constructs through [[unsafe]], its own trusted path. [[from]] is the public,
 * validated path for anyone assembling a `Blocked` outcome by hand, tests included.
 */
sealed abstract case class Causes private (toChunk: Chunk[NodeId]) derives CanEqual

object Causes:
  /** Construct from at least one id; the arity carries the non-emptiness, so no validation is needed. */
  def apply(first: NodeId, rest: NodeId*): Causes = new Causes(Chunk(first) ++ Chunk.from(rest)) {}

  /** Validate `ids` as non-empty; `Absent` when it is not. */
  def from(ids: Chunk[NodeId]): Maybe[Causes] = if ids.isEmpty then Absent else Present(new Causes(ids) {})

  /** Trusted constructor for a chunk the caller has already established is non-empty. */
  private[buildkit] def unsafe(ids: Chunk[NodeId]): Causes = new Causes(ids) {}
end Causes

/**
 * Outcome of a single node's execution.
 *
 * Diagnostics are deliberately absent in this slice; when the diagnostics channel lands it extends outcomes, never
 * events (single-owner rule).
 *
 * The `Blocked` case's non-empty invariant on `blockedBy` and `rootCauses` is structural: both are [[Causes]], which
 * cannot be constructed empty.
 *
 * `Blocked` usually means "never started" — the ordinary case is a node downstream of a failure, which the executor
 * closes with a lone `NodeFinished` and no `NodeStarted`. One case deliberately differs: a '''composite''' node that
 * started, ran children, and produced nothing because they failed reports `Blocked` too, naming those children. A
 * fan-out whose element runs failed is the instance today — it emits `NodeStarted`, brackets its children's events, and
 * closes `NodeFinished(id, Blocked)`. It is not `Failed`: the typed cause belongs to the child that raised it, and
 * duplicating it onto the parent would report one failure twice. Read `Blocked` as "produced nothing, and here is what
 * is responsible", not as "never ran".
 */
enum NodeOutcome[+E] derives CanEqual:
  case Succeeded(provenance: Provenance)
  case Failed(cause: Result.Error[E])
  case Cancelled
  case Skipped(reason: SkipReason)
  case Blocked(blockedBy: Causes, rootCauses: Causes)

  def status: NodeStatus = this match
    case _: Succeeded[?] => NodeStatus.Succeeded
    case _: Failed[?]    => NodeStatus.Failed
    case Cancelled       => NodeStatus.Cancelled
    case _: Skipped[?]   => NodeStatus.Skipped
    case _: Blocked[?]   => NodeStatus.Blocked

final case class NodeReport[+E](id: NodeId, ordinal: Int, outcome: NodeOutcome[E]) derives CanEqual

final case class PipelineReport[+E, +O](nodes: Chunk[NodeReport[E]], result: Maybe[O]) derives CanEqual:
  def outcome(id: NodeId): Maybe[NodeOutcome[E]] = Maybe.fromOption(nodes.find(_.id == id)).map(_.outcome)
  def failed: Chunk[NodeReport[E]]               = nodes.filter(_.outcome.status == NodeStatus.Failed)
  def blocked: Chunk[NodeReport[E]]              = nodes.filter(_.outcome.status == NodeStatus.Blocked)
  def isSuccess: Boolean                         = result.isDefined
