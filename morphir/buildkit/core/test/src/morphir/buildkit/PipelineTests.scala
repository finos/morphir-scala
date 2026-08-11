package morphir.buildkit

import kyo.*
import kyo.test.*

class PipelineTests extends Test[Any]:

  private def inc  = Stage.pure((i: Int) => i + 1).named("inc")
  private def show = Stage.pure((i: Int) => i.toString).named("show")

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
        .fromKyo[Int, String, Any](i => Pipeline.provenance.get.map(path => path.map(_.label).mkString(",")))
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
