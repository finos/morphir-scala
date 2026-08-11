package morphir.buildkit

import kyo.*
import kyo.test.*
import morphir.MorphirException

class NodeIdTests extends Test[Any]:

  "NodeId" - {
    "accepts a simple segment" in {
      NodeId.segment("parse") match
        case Result.Success(id) => assert(id.render == "parse")
        case _                  => assert(false)
    }
    "rejects a blank segment" in
      assert(NodeId.segment("  ").isFailure)
    "rejects a segment containing a slash" in {
      NodeId.segment("a/b") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "a/b")
        case _                                                  => assert(false)
    }
  }

  "nodeId interpolator" - {
    "builds a validated id from a literal" in
      assert(nodeId"parse".render == "parse")
    "rejects an invalid literal at compile time" in {
      assert(!scala.compiletime.testing.typeChecks(""" nodeId"a/b" """))
      assert(!scala.compiletime.testing.typeChecks(""" nodeId" " """))
    }
    "rejects interpolated arguments at compile time" in
      assert(!scala.compiletime.testing.typeChecks(""" val x = "p"; nodeId"$x" """))
  }

  "SealErrors" - {
    "is absent for an empty chunk" in
      assert(SealErrors(Chunk.empty[SealError]) == Absent)
    "aggregates and renders a message" in {
      val errors = Chunk[SealError](
        SealError.InvalidSegment("a/b", "contains '/'"),
        SealError.InvalidSegment("", "blank")
      )
      SealErrors(errors) match
        case Present(aggregate) =>
          assert(aggregate.errors.size == 2)
          assert(aggregate.getMessage.contains("2"))
        case Absent => assert(false)
    }
    "is catchable as MorphirException" in {
      val caught =
        try
          throw SealError.InvalidSegment("x/y", "contains '/'")
        catch case e: MorphirException => true
      assert(caught)
    }
  }
