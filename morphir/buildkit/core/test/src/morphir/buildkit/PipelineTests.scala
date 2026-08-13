package morphir.buildkit

import kyo.*
import kyo.test.*

class PipelineTests extends Test[Any]:

  private def inc    = Stage.pure((i: Int) => i + 1).named("inc")
  private def double = Stage.pure((i: Int) => i * 2).named("double")
  private def show   = Stage.pure((i: Int) => i.toString).named("show")

  private def sealOrFail[I, O, E, S](p: Pipeline[I, O, E, S]): SealedPipeline[I, O, E, S] =
    p.seal match
      case Result.Success(sealed_) => sealed_
      case other                   => throw new AssertionError(s"seal failed: $other")

  /**
   * Strip a proven-empty `Abort[Nothing]` so an infallible fixture's `execute` result can reach `.eval` directly —
   * every fixture stage in this file is pure, so `execute`'s own `E` infers as `Nothing`, and `Abort.run[Nothing]` is a
   * zero-cost, always-succeeding strip of that proven-empty channel. Mirrors `StageTests`'s own `runPure`.
   */
  private def runPure[A, S](v: A < (Abort[Nothing] & S)): A < S =
    Abort.run[Nothing](v).map(_.getOrThrow)

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
      val (_, viaPipeline) = runPure(Emit.run(plan.execute(41))).eval
      // `Stage.run` now returns `String < (Abort[Nothing] & Any)`: `inc`/`show` are pure fixtures pinned to
      // `E = Nothing`, so `Abort.run[Nothing]` is a zero-cost, always-succeeding strip of that proven-empty channel.
      val direct = Abort.run[Nothing]((inc andThen show).run(41)).eval.getOrThrow
      assert(viaPipeline == direct)
    }
    "emits Entered and Exited per node, in order" in {
      val plan        = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
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
      val (_, result) = runPure(Emit.run(plan.execute(0))).eval
      assert(result == "observer")
    }
    "a sealed plan is reusable across runs" in {
      val plan      = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (ev1, r1) = runPure(Emit.run(plan.execute(1))).eval
      val (ev2, r2) = runPure(Emit.run(plan.execute(41))).eval
      assert(r1 == "2" && r2 == "42" && ev1.size == 4 && ev2.size == 4)
    }
    "a three-node chain executes and emits in order, with correct ids" in {
      val plan             = sealOrFail(Pipeline.stage(inc).andThen(double).andThen(show))
      val (events, result) = runPure(Emit.run(plan.execute(3))).eval
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
      val (_, result) = runPure(Emit.run(plan.execute(4))).eval
      assert(result == (5, "4"))
    }
    "flattens across chained pars" in {
      val len         = Stage.pure((i: Int) => i.toLong).named("long")
      val plan        = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(show)).par(Pipeline.stage(len)))
      val (_, result) = runPure(Emit.run(plan.execute(4))).eval
      assert(result == (5, "4", 4L))
    }
    "emits branch events left then right under the sequential executor" in {
      val plan        = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(show)))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
      val entered     = events.collect { case StageEvent.Entered(id, _) => id.render }
      assert(entered == Chunk("inc", "show"))
    }
    "par2 pairs two peer pipelines" in {
      val plan        = sealOrFail(Pipeline.par2(Pipeline.stage(inc), Pipeline.stage(show)))
      val (_, result) = runPure(Emit.run(plan.execute(4))).eval
      assert(result == (5, "4"))
    }
    "par3 and par4 yield flat tuples" in {
      val len = Stage.pure((i: Int) => i.toLong).named("long")
      val neg = Stage.pure((i: Int) => -i).named("neg")
      val p3  = sealOrFail(Pipeline.par3(Pipeline.stage(inc), Pipeline.stage(show), Pipeline.stage(len)))
      val p4  =
        sealOrFail(Pipeline.par4(Pipeline.stage(inc), Pipeline.stage(show), Pipeline.stage(len), Pipeline.stage(neg)))
      val (_, r3) = runPure(Emit.run(p3.execute(4))).eval
      val (_, r4) = runPure(Emit.run(p4.execute(4))).eval
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
      val (_, result) = runPure(Emit.run(plan.execute(3))).eval
      assert(result == Chunk(1, 1, 1))
    }
    "zero elements yield an empty chunk and no child events" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut(Pipeline.stage(parse)))
      val (events, r) = runPure(Emit.run(plan.execute(0))).eval
      assert(r == Chunk.empty[Int])
      assert(!events.exists {
        case StageEvent.Entered(id, _) => id.render.contains("/")
        case _                         => false
      })
    }
    "child event ids carry the element path" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("parse-all", Pipeline.stage(parse)))
      val (events, _) = runPure(Emit.run(plan.execute(2))).eval
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
      val (events, _) = runPure(Emit.run(plan.execute(2))).eval
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
      val (events, _) = runPure(Emit.run(plan.execute(0))).eval
      val rendered    = events.collect {
        case StageEvent.Entered(id, _) => s"enter:${id.render}"
        case StageEvent.Exited(id, _)  => s"exit:${id.render}"
      }
      assert(rendered == Chunk("enter:sources", "exit:sources", "enter:fo", "exit:fo"))
    }
  }

  "branch" - {
    def big   = Pipeline.stage(Stage.pure((i: Int) => s"big:$i").named("big"))
    def small = Pipeline.stage(Stage.pure((i: Int) => s"small:$i").named("small"))

    "takes the true arm and skips the false arm's nodes" in {
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 10)(big, small))
      val (events, r) = runPure(Emit.run(plan.execute(100))).eval
      assert(r == "big:101")
      assert(events.contains(StageEvent.Skipped(
        events.collect {
          case StageEvent.Skipped(id, _) => id
        }.head,
        "predicate was true"
      )))
      assert(events.collect { case StageEvent.Skipped(id, _) => id.render } == Chunk("small"))
    }
    "takes the false arm and skips the true arm's nodes" in {
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 10)(big, small))
      val (events, r) = runPure(Emit.run(plan.execute(1))).eval
      assert(r == "small:2")
      assert(events.collect { case StageEvent.Skipped(id, _) => id.render } == Chunk("big"))
    }
    "when yields Present on the taken path and Absent otherwise" in {
      val plan         = sealOrFail(Pipeline.stage(inc).when(_ > 10)(big))
      val (_, taken)   = runPure(Emit.run(plan.execute(100))).eval
      val (_, untaken) = runPure(Emit.run(plan.execute(1))).eval
      assert(taken == Present("big:101") && untaken == Absent)
    }
    "duplicate ids across arms are rejected at seal" in {
      val dupArm = Pipeline.stage(Stage.pure((i: Int) => s"x:$i").named("big"))
      Pipeline.stage(inc).branch(_ > 10)(big, dupArm).seal match
        case Result.Failure(errors) => assert(errors.errors.nonEmpty)
        case _                      => assert(false)
    }
    "the branch node's own Entered/Exited bracket the taken arm and the skip events" in {
      val wrap        = Pipeline.stage(Stage.pure((i: Int) => i.toString).named("wrap"))
      val other       = Pipeline.stage(Stage.pure((i: Int) => i).named("other"))
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 0)(wrap, other))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
      val rendered    = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      // The branch node itself has no explicit id and no label (a pipeline construct, not a `Stage`), so it falls
      // back to its position — "node-1": index 0 is `inc`, index 1 is the branch's own slot (see `DefElem.BranchElem`
      // and `Sealing.sealChain`'s position-fallback, unchanged by this task).
      assert(
        rendered == Chunk(
          "enter:inc",
          "exit:inc:Succeeded",
          "enter:node-1",
          "enter:wrap",
          "exit:wrap:Succeeded",
          "skip:other",
          "exit:node-1:Succeeded"
        )
      )
    }
    "a nested fan-out inside an untaken arm emits exactly one Skipped for the fan-out node itself" in {
      def sources = Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources")
      def parse   = Stage.pure((s: String) => s.length).named("parse")

      val takenArm    = Pipeline.stage(Stage.pure((i: Int) => Chunk.empty[Int]).named("empty"))
      val untakenArm  = Pipeline.stage(sources).fanOut("parse-all", Pipeline.stage(parse))
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 0)(takenArm, untakenArm))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
      // Only the fan-out node's own id is skipped — its per-element child ("parse") lives in its own independent,
      // nested seal namespace and was never counted, since it never ran.
      assert(events.collect { case StageEvent.Skipped(id, _) => id.render } == Chunk("sources", "parse-all"))
    }
    "a fan-out nested in a branch arm still surfaces its own child seal failure, without a sibling error masking it" in {
      def sources = Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources")
      def parse   = Stage.pure((s: String) => s.length).named("parse")

      val dup           = Pipeline.stage(parse).andThen(Stage.pure((i: Int) => i).named("parse"))
      val armWithFanOut = Pipeline.stage(sources).fanOut("parse-all", dup)
      val otherArm      = Pipeline.stage(Stage.pure((i: Int) => Chunk.empty[Int]))
      // "bad/segment" fails its own-level id validation; the fan-out nested in the true arm independently fails to
      // seal on its own duplicate "parse" id. Both must surface from a single `seal` call, mirroring the top-level
      // "sibling errors do not mask a fan-out child's seal failure" fanOut test, now through a branch arm.
      Pipeline.stage("bad/segment", inc).branch(_ > 0)(armWithFanOut, otherArm).seal match
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
  }

  "event balance" - {
    "a panicking stage still closes its Entered with Exited(Failed), and the panic keeps propagating" in {
      val boom = Stage.pure((i: Int) => (throw new RuntimeException("boom")): Int).named("boom")
      val plan = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      // `Emit.run` outside `Abort.run[Nothing]`, same nesting as the typed-Abort case below: the inner boundary
      // absorbs the panic into a `Result.Panic` value before `Emit.run` ever needs to observe a short-circuit, so
      // the events accumulated up to the panic survive. `Abort.run` catches a raw panic regardless of the `E` it
      // was asked to run — that is what "the panic keeps propagating" means here: nothing between the stage's own
      // `bracketed` rethrow and this outer boundary silently swallowed it.
      val (events, outcome) = Emit.run(Abort.run[Nothing](plan.execute(1))).eval
      outcome match
        case Result.Panic(ex) => assert(ex.getMessage == "boom")
        case other            => assert(false, s"expected a panic, got $other")
      val rendered = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      assert(rendered == Chunk("enter:inc", "exit:inc:Succeeded", "enter:boom", "exit:boom:Failed"))
    }
    // The typed row now compiles and is observable at `execute`'s own public boundary: `Abort[E]` is a first-class
    // part of `execute`'s declared row (this task), so the failure surfaces to the caller with its original type,
    // and the events emitted before the short-circuit (both `inc`'s own pair and `boom`'s own `Entered`) survive —
    // `Emit.run` collects everything emitted up to the point Kyo's own suspend/resume `Abort` protocol unwinds the
    // computation, the same way a panic's pre-failure events already survived in the case above.
    "a stage whose typed Abort short-circuits surfaces the original error, with events up to that point intact" in {
      val boom              = Stage.fromKyo[Int, Int, Abort[String]](i => Abort.fail("boom")).named("boom")
      val plan              = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      val (events, outcome) = Emit.run(Abort.run[String](plan.execute(1))).eval
      assert(outcome == Result.Failure("boom"))
      val rendered = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      assert(rendered == Chunk("enter:inc", "exit:inc:Succeeded", "enter:boom"))
    }
    // A typed `Abort[E]` failure never reaches `kyo.kernel.Effect.catching`'s `catch` block — it propagates through
    // Kyo's own suspend/resume ArrowEffect protocol, not a JVM `throw`, so `bracketed` (which only non-fatal panics
    // close) does not see it, and `boom`'s own `Entered` above is left dangling (no matching `Exited`). Closing it
    // is by design out of scope for `execute`'s own value-mode row (see its scaladoc): there is no report to record
    // a partial run in. Task 5's report executor intercepts each node's own `Abort[E]` via `Abort.recover` and
    // re-raises it, closing `Entered` with `Exited(Failed)` first, inside `bracketed`. Left `pendingUntilFixed`
    // rather than dropped, so the moment that executor lands, the suite itself flags it.
    "a stage whose typed Abort short-circuits still closes its Entered with Exited(Failed)".pendingUntilFixed(
      "closing a node's Entered on a typed Abort[E] short-circuit needs the report executor (Task 5) to intercept " +
        "it via Abort.recover and re-raise; execute's own value-mode row has no report to record a partial run in"
    ) in {
      val boom              = Stage.fromKyo[Int, Int, Abort[String]](i => Abort.fail("boom")).named("boom")
      val plan              = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      val (events, outcome) = Emit.run(Abort.run[String](plan.execute(1))).eval
      assert(outcome == Result.Failure("boom"))
      val rendered = events.map {
        case StageEvent.Entered(id, _)      => s"enter:${id.render}"
        case StageEvent.Exited(id, outcome) => s"exit:${id.render}:$outcome"
        case StageEvent.Skipped(id, _)      => s"skip:${id.render}"
      }
      assert(rendered == Chunk("enter:inc", "exit:inc:Succeeded", "enter:boom", "exit:boom:Failed"))
    }
  }

  "typed pipelines" - {
    "execute exposes the union of node error types in its row" in {
      // Explicit type ascriptions pin `S` to `Any`: with no target type flowing into the bare `Stage(...)` calls,
      // Scala's own inference leaves `S` underconstrained and defaults it to `Nothing` rather than `Any` — a valid,
      // but unusable-by-`.eval` row (`.eval` requires exactly `Any`, and `Nothing` is not a subtype of it, since `S`
      // is contravariant).
      val double: Stage[Int, Int, Nothing, Any] = Stage((i: Int) => i * 2)
      val guard: Stage[Int, Int, Overflow, Any] = Stage((i: Int) => if i > 100 then Abort.fail(Overflow(i)) else i)
      val p                                     = Pipeline.stage(nodeId"double", double).andThen(nodeId"guard", guard)
      val sealed_                               = sealOrFail(p)
      val (_, outcome)                          = Emit.run(Abort.run[Overflow](sealed_.execute(60))).eval
      assert(outcome == Result.fail(Overflow(120)))
    }
  }

  "toMermaid" - {
    "renders a linear plan" in {
      val plan = sealOrFail(Pipeline.stage(inc).andThen(show))
      assert(plan.toMermaid ==
        """flowchart TD
          |  inc["inc"]
          |  show["show"]
          |  inc --> show""".stripMargin)
    }
    "renders a branch as a decision with both arms" in {
      val plan = sealOrFail(
        Pipeline.stage(inc).branch(_ > 10)(
          Pipeline.stage(Stage.pure((i: Int) => s"b:$i").named("big")),
          Pipeline.stage(Stage.pure((i: Int) => s"s:$i").named("small"))
        )
      )
      val src = plan.toMermaid
      assert(src.contains("{\"?\"}"))
      assert(src.contains("-->|true| big") && src.contains("-->|false| small"))
    }
    "renders a fanOut as a subgraph annotated per element" in {
      val plan = sealOrFail(
        Pipeline
          .stage(Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources"))
          .fanOut("parse-all", Pipeline.stage(Stage.pure((s: String) => s.length).named("parse")))
      )
      val src = plan.toMermaid
      assert(src.contains("subgraph parse-all"))
      assert(src.contains("|per element|"))
    }
    "escapes quotes in labels" in {
      val plan = sealOrFail(Pipeline.stage(Stage.pure((i: Int) => i).named("say \"hi\"")))
      assert(plan.toMermaid.contains("say #quot;hi#quot;"))
    }
    "is deterministic" in {
      val plan = sealOrFail(Pipeline.stage(inc).andThen(show))
      assert(plan.toMermaid == plan.toMermaid)
    }
    "sanitizes an explicit id with spaces and brackets into a valid mermaid id" in {
      val plan = sealOrFail(Pipeline.stage("weird id [x]", Stage.pure((i: Int) => i).named("w")))
      val src  = plan.toMermaid
      assert(src.contains("weird_id__x_[\"w\"]"))
      assert(!src.contains("weird id [x]"))
    }
    "disambiguates two distinct ids that sanitize to the same mermaid id" in {
      val plan = sealOrFail(
        Pipeline
          .stage("a[1]", Stage.pure((i: Int) => i).named("first"))
          .andThen("a 1 ", Stage.pure((i: Int) => i).named("second"))
      )
      val src = plan.toMermaid
      assert(src.contains("a_1_[\"first\"]"))
      assert(src.contains("a_1__2[\"second\"]"))
      assert(src.contains("a_1_ --> a_1__2"))
    }
    "a fan-out child's raw id does not collide with an unrelated explicit sibling id sharing its rendered prefix" in {
      // Regression for the `rawId` join: a fan-out child qualified `fo` + `parse` must not collide, at the raw-id
      // level, with an unrelated node explicitly id'd `fo_parse` — before the fix both joined on `_`, landing on the
      // exact same raw string and colliding in `buildIdMap`, so the two nodes rendered as one wrong, shared box.
      val plan = sealOrFail(
        Pipeline
          .stage(Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources"))
          .fanOut("fo", Pipeline.stage("parse", Stage.pure((s: String) => s.length).named("child")))
          .andThen("fo_parse", Stage.pure((cs: Chunk[Int]) => cs.sum).named("successor"))
      )
      val src         = plan.toMermaid
      val declaredIds = src.linesIterator.collect {
        case l if l.trim.startsWith("subgraph ")         => l.trim.stripPrefix("subgraph ").trim
        case l if l.contains("[\"") || l.contains("{\"") => l.trim.takeWhile(c => c != '[' && c != '{')
      }.toList
      assert(declaredIds.distinct.size == 4)
      assert(declaredIds.toSet == Set("sources", "fo", "fo_parse", "fo_parse_2"))
      assert(src.contains("fo_parse[\"child\"]"))
      assert(src.contains("fo_parse_2[\"successor\"]"))
    }
    "renders a par with reconverging edges into its successor" in {
      val plan = sealOrFail(
        Pipeline.stage(inc).par(Pipeline.stage(show)).andThen(Stage.pure((t: (Int, String)) => t._2).named("collect"))
      )
      val src = plan.toMermaid
      assert(src.contains("inc[\"inc\"]") && src.contains("show[\"show\"]") && src.contains("collect[\"collect\"]"))
      assert(src.contains("inc --> collect"))
      assert(src.contains("show --> collect"))
    }
  }

final case class Overflow(value: Int) derives CanEqual
