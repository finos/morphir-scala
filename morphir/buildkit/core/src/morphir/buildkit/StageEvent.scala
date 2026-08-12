package morphir.buildkit

import kyo.*

/**
 * How a node's execution concluded. `Failed` is emitted when a bracketing node's own work panics (a raw thrown
 * exception); a typed `Abort[E]` short-circuit does not yet produce it — see [[SealedPipeline#execute]]. `Halted`
 * becomes emittable when executor-owned halting arrives.
 */
enum StageOutcome:
  case Succeeded, Failed, Halted

/**
 * Lifecycle events emitted by pipeline executors through `Emit[StageEvent]`.
 *
 * Events describe lifecycle, not values — results flow through the pipeline itself. `Skipped` is emitted for every
 * static node reachable through a `branch`/`when` arm the predicate did not take (intent 0008 names explicit skipped
 * nodes). `Exited(id, Failed)` closes a bracketing node's `Entered` when its own work panics; a typed `Abort[E]`
 * short-circuit is part of the same contract but is not yet emittable — see [[SealedPipeline#execute]].
 */
enum StageEvent:
  case Entered(id: NodeId, meta: Maybe[StageMeta])
  case Exited(id: NodeId, outcome: StageOutcome)
  case Skipped(id: NodeId, reason: String)
