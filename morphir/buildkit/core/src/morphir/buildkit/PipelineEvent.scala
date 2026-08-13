package morphir.buildkit

import kyo.*

/**
 * Lifecycle events emitted by pipeline executors through `Emit[PipelineEvent]`.
 *
 * Events describe lifecycle, not values — results flow through the pipeline itself.
 *
 * '''Contract.''' Every `NodeStarted` is closed by exactly one `NodeFinished` for the same id, including when the run
 * halts on a typed `Abort[E]` short-circuit or panics on a raw thrown exception — see [[SealedPipeline#execute]] for
 * how each closing case is produced. `RunStarted` is always the first event a consumer observes and `RunFinished`
 * always the last, bracketing the whole stream; a `Skipped` node folds into `NodeFinished`'s own `NodeStatus.Skipped`,
 * so a node that never ran still gets exactly one `NodeFinished`, never a `NodeStarted`. Consumers key on node ids,
 * never on event position or count: fan-out and branch nodes interleave their own per-element or per-arm events between
 * a parent node's own `NodeStarted` and `NodeFinished`.
 */
enum PipelineEvent derives CanEqual:
  case RunStarted
  case RunFinished(succeeded: Boolean)
  case NodeStarted(id: NodeId, meta: Maybe[StageMeta])
  case NodeFinished(id: NodeId, status: NodeStatus)
  case NodeProgress(id: NodeId, message: String)
