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
 * Outcome of a single node's execution.
 *
 * Diagnostics are deliberately absent in this slice; when the diagnostics channel lands it extends outcomes, never
 * events (single-owner rule).
 *
 * The `Blocked` case maintains an invariant: both `blockedBy` and `rootCauses` are non-empty. The executor is
 * responsible for maintaining this invariant; it is documented here but not encoded in the type (kyo has no non-empty
 * chunk type).
 */
enum NodeOutcome[+E] derives CanEqual:
  case Succeeded(provenance: Provenance)
  case Failed(cause: Result.Error[E])
  case Cancelled
  case Skipped(reason: SkipReason)
  case Blocked(blockedBy: Chunk[NodeId], rootCauses: Chunk[NodeId])

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
