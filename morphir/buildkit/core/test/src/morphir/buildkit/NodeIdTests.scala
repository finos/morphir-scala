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

  "auto id ordinal suffixes" - {
    def stage(label: String)(f: Int => Int) = Stage.pure(f).named(label)

    "two unnamed nodes with the same label seal with ordinal suffixes" in {
      val p = Pipeline.stage(stage("normalize")(_ + 1)).andThen(stage("normalize")(_ + 2))
      p.seal match
        case Result.Success(sp) => assert(sp.nodeIds.map(_.render) == Chunk("normalize", "normalize-2"))
        case other              => assert(false, s"expected a successful seal, got $other")
    }

    "two explicit ids that collide are still a seal error" in {
      Pipeline.stage("dup", stage("a")(_ + 1)).andThen("dup", stage("b")(_ + 2)).seal match
        case Result.Failure(errors) =>
          assert(
            errors.errors.exists {
              case SealError.DuplicateNodeId(id) => id.render == "dup"
              case _                             => false
            },
            s"expected a DuplicateNodeId('dup') error, got $errors"
          )
        case other => assert(false, s"expected a failed seal, got $other")
    }

    "a derived slug colliding with an explicit id suffixes around it, never the other way" in {
      val p = Pipeline.stage("normalize", stage("first")(_ + 1)).andThen(stage("normalize")(_ + 2))
      p.seal match
        case Result.Success(sp) => assert(sp.nodeIds.map(_.render) == Chunk("normalize", "normalize-2"))
        case other              => assert(false, s"expected a successful seal, got $other")
    }

    "a derived id declared before a colliding explicit id still yields to it" in {
      val p = Pipeline.stage(stage("normalize")(_ + 1)).andThen("normalize", stage("second")(_ + 2))
      p.seal match
        case Result.Success(sp) => assert(sp.nodeIds.map(_.render) == Chunk("normalize-2", "normalize"))
        case other              => assert(false, s"expected a successful seal, got $other")
    }

    "the derived suffix skips an ordinal already taken by an explicit id" in {
      val p = Pipeline
        .stage(stage("normalize")(_ + 1))
        .andThen("normalize-2", stage("second")(_ + 2))
        .andThen(stage("normalize")(_ + 3))
      p.seal match
        case Result.Success(sp) =>
          assert(sp.nodeIds.map(_.render) == Chunk("normalize", "normalize-2", "normalize-3"))
        case other => assert(false, s"expected a successful seal, got $other")
    }
  }
