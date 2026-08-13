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
      NodeOutcome.Blocked(Causes(nodeId"a"), Causes(nodeId"a"))
    )
    assert(all.map(_.status).distinct.size == 5)
  }

  "Causes" - {
    "from an empty chunk is Absent" in
      assert(Causes.from(Chunk.empty) == Absent)
    "from a non-empty chunk is Present" in {
      Causes.from(Chunk(nodeId"a")) match
        case Present(causes) => assert(causes.toChunk == Chunk(nodeId"a"))
        case Absent          => assert(false, "expected Present")
    }
    "two Causes built from the same ids are equal" in
      assert(Causes.unsafe(Chunk(nodeId"a", nodeId"b")) == Causes.unsafe(Chunk(nodeId"a", nodeId"b")))
    "the varargs constructor carries non-emptiness by arity and matches the validated path" in
      assert(Causes(nodeId"a", nodeId"b") == Causes.unsafe(Chunk(nodeId"a", nodeId"b")))
  }
