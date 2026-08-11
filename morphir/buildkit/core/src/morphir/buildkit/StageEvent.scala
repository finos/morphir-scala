package morphir.buildkit

import kyo.*

/**
 * How a node's execution concluded. This slice emits only `Succeeded`; `Failed` and `Halted` become emittable when
 * executor-owned halting arrives.
 */
enum StageOutcome:
  case Succeeded, Failed, Halted

/**
 * Lifecycle events emitted by pipeline executors through `Emit[StageEvent]`.
 *
 * Events describe lifecycle, not values — results flow through the pipeline itself. `Skipped` is part of the contract
 * now (intent 0008 names explicit skipped nodes) and becomes emittable when conditional branches arrive.
 */
enum StageEvent:
  case Entered(id: NodeId, meta: Maybe[StageMeta])
  case Exited(id: NodeId, outcome: StageOutcome)
  case Skipped(id: NodeId, reason: String)
