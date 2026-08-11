package morphir.buildkit

import kyo.*
import morphir.MorphirException

/** A single validation failure found while sealing a pipeline definition. */
enum SealError(message: String) extends MorphirException(message):
  case DuplicateNodeId(id: NodeId) extends SealError(s"duplicate node id: ${id.render}")
  case InvalidSegment(value: String, reason: String)
      extends SealError(s"invalid node id segment '$value': $reason")

/**
 * Every failure a seal found, together. Kept distinct from [[SealError]] so a nested aggregate is unrepresentable;
 * non-emptiness holds by construction.
 */
final class SealErrors private (val errors: Chunk[SealError])
    extends MorphirException(s"sealing failed with ${errors.size} error(s): ${errors.map(_.getMessage).mkString("; ")}")

object SealErrors:
  /** `Absent` when `errors` is empty. */
  def apply(errors: Chunk[SealError]): Maybe[SealErrors] =
    if errors.isEmpty then Absent else Present(new SealErrors(errors))
