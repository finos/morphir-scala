package morphir.buildkit

import kyo.*
import kyo.test.*

class PipelineTests extends Test[Any]:

  private def inc    = Stage.pure((i: Int) => i + 1).named("inc")
  private def double = Stage.pure((i: Int) => i * 2).named("double")
  private def show   = Stage.pure((i: Int) => i.toString).named("show")

  private def sealOrFail[I, O, S](p: Pipeline[I, O, S]): SealedPipeline[I, O, S] =
    p.seal match
      case Result.Success(sealed_) => sealed_
      case other                   => throw new AssertionError(s"seal failed: $other")

  "sealing" - {
    "assigns label-derived ids in order" in {
      val plan = sealOrFail(Pipeline.stage(inc).andThen(show))
      assert(plan.nodeIds.map(_.render) == Chunk("inc", "show"))
    }
    "explicit id beats label beats position" in {
      val anon = Stage.pure((s: String) => s.length)
      val plan = sealOrFail(Pipeline.stage("first", inc).andThen(show).andThen(anon))
      assert(plan.nodeIds.map(_.render) == Chunk("first", "show", "node-2"))
    }
    "slugifies labels" in {
      val labelled = Stage.pure((i: Int) => i).named("Parse Elm Source")
      val plan     = sealOrFail(Pipeline.stage(labelled))
      assert(plan.nodeIds.map(_.render) == Chunk("parse-elm-source"))
    }
    "slugifies uppercase 'I' without locale-dependent case folding" in {
      val labelled = Stage.pure((i: Int) => i).named("Parse IR")
      val plan     = sealOrFail(Pipeline.stage(labelled))
      assert(plan.nodeIds.map(_.render) == Chunk("parse-ir"))
    }
    "rejects duplicate ids" in {
      Pipeline.stage(inc).andThen(Stage.pure((i: Int) => i * 2).named("inc")).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "inc"
            case _                             => false
          })
        case _ => assert(false)
    }
    "accumulates every failure" in {
      val dupA = Stage.pure((i: Int) => i).named("a")
      val dupB = Stage.pure((i: Int) => i).named("b")
      Pipeline
        .stage(dupA)
        .andThen(Stage.pure((i: Int) => i).named("a"))
        .andThen(dupB)
        .andThen(Stage.pure((i: Int) => i).named("b"))
        .andThen("bad/segment", Stage.pure((i: Int) => i))
        .seal match
        case Result.Failure(errors) => assert(errors.errors.size == 3)
        case _                      => assert(false)
    }
    "sealing a sealed pipeline returns it unchanged" in {
      val plan = sealOrFail(Pipeline.stage(inc))
      plan.seal match
        case Result.Success(same) => assert(same eq plan)
        case _                    => assert(false)
    }
  }

  "execution" - {
    "matches the equivalent Stage composition" in {
      val plan             = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (_, viaPipeline) = Emit.run(plan.execute(41)).eval
      val direct           = (inc andThen show).run(41).eval
      assert(viaPipeline == direct)
    }
    "emits Entered and Exited per node, in order" in {
      val plan        = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (events, _) = Emit.run(plan.execute(1)).eval
      val rendered    = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      assert(rendered == Chunk("enter:inc", "exit:inc:Succeeded", "enter:show", "exit:show:Succeeded"))
    }
    "a stage can read its provenance" in {
      val observing = Stage
        .fromKyo[Int, String, Any](i => Pipeline.provenance.map(path => path.map(_.label).mkString(",")))
        .named("observer")
      val plan        = sealOrFail(Pipeline.stage(observing))
      val (_, result) = Emit.run(plan.execute(0)).eval
      assert(result == "observer")
    }
    "a sealed plan is reusable across runs" in {
      val plan      = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (ev1, r1) = Emit.run(plan.execute(1)).eval
      val (ev2, r2) = Emit.run(plan.execute(41)).eval
      assert(r1 == "2" && r2 == "42" && ev1.size == 4 && ev2.size == 4)
    }
    "a three-node chain executes and emits in order, with correct ids" in {
      val plan             = sealOrFail(Pipeline.stage(inc).andThen(double).andThen(show))
      val (events, result) = Emit.run(plan.execute(3)).eval
      val rendered         = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      assert(result == "8")
      assert(
        rendered == Chunk(
          "enter:inc",
          "exit:inc:Succeeded",
          "enter:double",
          "exit:double:Succeeded",
          "enter:show",
          "exit:show:Succeeded"
        )
      )
    }
  }

  "nodeId overloads" - {
    "a literal id flows through stage and andThen" in {
      val plan = sealOrFail(Pipeline.stage(nodeId"first", inc).andThen(nodeId"second", show))
      assert(plan.nodeIds.map(_.render) == Chunk("first", "second"))
    }
  }

  "describe" - {
    "renders the chain" in
      assert(Pipeline.stage(inc).andThen(show).describe == "inc andThen show")
  }

  "par" - {
    "runs both sides on the same input and pairs results" in {
      val plan        = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(show)))
      val (_, result) = Emit.run(plan.execute(4)).eval
      assert(result == (5, "4"))
    }
    "flattens across chained pars" in {
      val len         = Stage.pure((i: Int) => i.toLong).named("long")
      val plan        = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(show)).par(Pipeline.stage(len)))
      val (_, result) = Emit.run(plan.execute(4)).eval
      assert(result == (5, "4", 4L))
    }
    "emits branch events left then right under the sequential executor" in {
      val plan        = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(show)))
      val (events, _) = Emit.run(plan.execute(1)).eval
      val entered     = events.collect { case StageEvent.Entered(id, _) => id.render }
      assert(entered == Chunk("inc", "show"))
    }
    "par2 pairs two peer pipelines" in {
      val plan        = sealOrFail(Pipeline.par2(Pipeline.stage(inc), Pipeline.stage(show)))
      val (_, result) = Emit.run(plan.execute(4)).eval
      assert(result == (5, "4"))
    }
    "par3 and par4 yield flat tuples" in {
      val len = Stage.pure((i: Int) => i.toLong).named("long")
      val neg = Stage.pure((i: Int) => -i).named("neg")
      val p3  = sealOrFail(Pipeline.par3(Pipeline.stage(inc), Pipeline.stage(show), Pipeline.stage(len)))
      val p4  =
        sealOrFail(Pipeline.par4(Pipeline.stage(inc), Pipeline.stage(show), Pipeline.stage(len), Pipeline.stage(neg)))
      val (_, r3) = Emit.run(p3.execute(4)).eval
      val (_, r4) = Emit.run(p4.execute(4)).eval
      assert(r3 == (5, "4", 4L) && r4 == (5, "4", 4L, -4))
    }
    "rejects duplicate ids across sides at seal" in {
      Pipeline.stage(inc).par(Pipeline.stage(Stage.pure((i: Int) => i).named("inc"))).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "inc"
            case _                             => false
          })
        case _ => assert(false)
    }
  }

  "fanOut" - {
    def sources = Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources")
    def parse   = Stage.pure((s: String) => s.length).named("parse")

    "maps every element through the child pipeline" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut(Pipeline.stage(parse)))
      val (_, result) = Emit.run(plan.execute(3)).eval
      assert(result == Chunk(1, 1, 1))
    }
    "zero elements yield an empty chunk and no child events" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut(Pipeline.stage(parse)))
      val (events, r) = Emit.run(plan.execute(0)).eval
      assert(r == Chunk.empty[Int])
      assert(!events.exists {
        case StageEvent.Entered(id, _) => id.render.contains("/")
        case _                         => false
      })
    }
    "child event ids carry the element path" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("parse-all", Pipeline.stage(parse)))
      val (events, _) = Emit.run(plan.execute(2)).eval
      val childIds    = events.collect { case StageEvent.Entered(id, _) if id.render.contains("/") => id.render }
      assert(childIds == Chunk("parse-all/0/parse", "parse-all/1/parse"))
    }
    "a child that fails to seal surfaces path-qualified in the parent aggregate" in {
      val dup = Pipeline.stage(parse).andThen(Stage.pure((i: Int) => i).named("parse"))
      Pipeline.stage(sources).fanOut("parse-all", dup).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "parse-all/parse"
            case _                             => false
          })
        case _ => assert(false)
    }
    "sibling errors do not mask a fan-out child's seal failure" in {
      val dup = Pipeline.stage(parse).andThen(Stage.pure((i: Int) => i).named("parse"))
      // "bad/segment" fails its own-level id validation; the fan-out's child independently fails to seal on its own
      // duplicate "parse" id. Both must surface from a single `seal` call — one sibling's failure must not swallow
      // the other, unrelated failure nested inside the fan-out.
      Pipeline.stage("bad/segment", sources).fanOut("parse-all", dup).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.InvalidSegment(value, _) => value == "bad/segment"
            case _                                  => false
          })
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "parse-all/parse"
            case _                             => false
          })
        case _ => assert(false)
    }
    "the fan-out node's own Entered/Exited bracket the per-element events" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("fo", Pipeline.stage(parse)))
      val (events, _) = Emit.run(plan.execute(2)).eval
      val rendered    = events.collect {
        case StageEvent.Entered(id, _) => s"enter:${id.render}"
        case StageEvent.Exited(id, _)  => s"exit:${id.render}"
      }
      assert(
        rendered == Chunk(
          "enter:sources",
          "exit:sources",
          "enter:fo",
          "enter:fo/0/parse",
          "exit:fo/0/parse",
          "enter:fo/1/parse",
          "exit:fo/1/parse",
          "exit:fo"
        )
      )
    }
    "the fan-out node's own bracket still fires with zero elements" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("fo", Pipeline.stage(parse)))
      val (events, _) = Emit.run(plan.execute(0)).eval
      val rendered    = events.collect {
        case StageEvent.Entered(id, _) => s"enter:${id.render}"
        case StageEvent.Exited(id, _)  => s"exit:${id.render}"
      }
      assert(rendered == Chunk("enter:sources", "exit:sources", "enter:fo", "exit:fo"))
    }
  }
