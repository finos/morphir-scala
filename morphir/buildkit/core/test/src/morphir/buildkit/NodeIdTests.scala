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
    "rejects a segment equal to '.'" in {
      NodeId.segment(".") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == ".")
        case _                                                  => assert(false)
    }
    "rejects a segment equal to '..'" in {
      NodeId.segment("..") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "..")
        case _                                                  => assert(false)
    }
    "rejects a segment containing a backslash" in {
      NodeId.segment("a\\b") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "a\\b")
        case _                                                  => assert(false)
    }
    "rejects a segment containing a control character" in {
      NodeId.segment("a\u0007b") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "a\u0007b")
        case _                                                  => assert(false)
    }
    "rejects a segment containing DEL (0x7F)" in {
      NodeId.segment("a\u007Fb") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "a\u007Fb")
        case _                                                  => assert(false)
    }
    "rejects a segment containing a C1 control character (0x80–0x9F)" in {
      NodeId.segment("a\u0080b") match
        case Result.Failure(SealError.InvalidSegment(value, _)) => assert(value == "a\u0080b")
        case _                                                  => assert(false)
    }
  }

  "nodeId interpolator" - {
    "builds a validated id from a literal" in
      assert(nodeId"parse".render == "parse")
    "typechecks a valid literal (positive control)" in
      assert(scala.compiletime.testing.typeChecks(""" nodeId"ok" """))
    "rejects an invalid literal at compile time" in {
      assert(!scala.compiletime.testing.typeChecks(""" nodeId"a/b" """))
      assert(!scala.compiletime.testing.typeChecks(""" nodeId" " """))
      val errors: List[scala.compiletime.testing.Error] =
        scala.compiletime.testing.typeCheckErrors(""" nodeId"a/b" """)
      assert(errors.exists(_.message.contains("invalid node id segment")))
    }
    "rejects '.', '..' and a backslash at compile time" in {
      assert(!scala.compiletime.testing.typeChecks(""" nodeId"." """))
      assert(!scala.compiletime.testing.typeChecks(""" nodeId".." """))
      assert(!scala.compiletime.testing.typeChecks(""" nodeId"a\\b" """))
      val errors: List[scala.compiletime.testing.Error] =
        scala.compiletime.testing.typeCheckErrors(""" nodeId"." """)
      assert(errors.exists(_.message.contains("invalid node id segment")))
    }
    "rejects interpolated arguments at compile time" in {
      assert(!scala.compiletime.testing.typeChecks(""" val x = "p"; nodeId"$x" """))
      val errors: List[scala.compiletime.testing.Error] =
        scala.compiletime.testing.typeCheckErrors(""" val x = "p"; nodeId"$x" """)
      assert(errors.exists(_.message.contains("accepts no interpolated arguments")))
    }
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
