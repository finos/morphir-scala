package morphir.buildkit

import kyo.*
import kyo.test.*

class NodeOutcomeTests extends Test[Any]:

  "status is total over every outcome" in {
    val all: Chunk[NodeOutcome[String]] = Chunk(
      NodeOutcome.Succeeded(Provenance.Executed),
      NodeOutcome.Failed(Result.Failure("boom")),
      NodeOutcome.Cancelled,
      NodeOutcome.Skipped(SkipReason.ConditionFalse),
      NodeOutcome.Blocked(Chunk(nodeId"a"), Chunk(nodeId"a"))
    )
    assert(all.map(_.status).distinct.size == 5)
  }
