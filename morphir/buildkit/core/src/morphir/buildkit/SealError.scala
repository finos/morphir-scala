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

  /**
   * Trusted constructor for a chunk already known to be non-empty — mirrors [[morphir.buildkit.NodeId.unsafe]]. Used
   * internally to combine and re-qualify errors that are already known to exist (a nested fan-out's own seal failure,
   * or the union of two sides that already failed), where re-deriving non-emptiness through [[apply]] would just be
   * re-proving what the caller already established.
   */
  private[buildkit] def unsafe(errors: Chunk[SealError]): SealErrors = new SealErrors(errors)
