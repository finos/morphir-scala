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

  /** Render an event trace as short tags, for compact equality assertions across this whole file. */
  private def render(events: Chunk[PipelineEvent]): Chunk[String] =
    events.map {
      case PipelineEvent.RunStarted           => "run:started"
      case PipelineEvent.RunFinished(ok)      => s"run:finished:$ok"
      case PipelineEvent.NodeStarted(id, _)   => s"start:${id.render}"
      case PipelineEvent.NodeFinished(id, st) => s"finish:${id.render}:$st"
      case PipelineEvent.NodeProgress(id, m)  => s"progress:${id.render}:$m"
    }

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
    "two label-derived ids that collide suffix in declaration order instead of erroring" in {
      val plan = sealOrFail(Pipeline.stage(inc).andThen(Stage.pure((i: Int) => i * 2).named("inc")))
      assert(plan.nodeIds.map(_.render) == Chunk("inc", "inc-2"))
    }
    "rejects duplicate ids" in {
      // Both explicit: an explicit id is never renamed, so — unlike a label collision, which now suffixes — two
      // explicit ids that collide remain unresolvable and still surface as `SealError.DuplicateNodeId`.
      Pipeline.stage("inc", inc).andThen("inc", Stage.pure((i: Int) => i * 2)).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "inc"
            case _                             => false
          })
        case _ => assert(false)
    }
    "accumulates every failure" in {
      // Explicit-explicit collisions on "a" and "b", plus one invalid segment: label collisions alone would no
      // longer error under the ordinal-suffix rule, so this exercises the still-erroring explicit-id path instead.
      val dupA = Stage.pure((i: Int) => i).named("a")
      val dupB = Stage.pure((i: Int) => i).named("b")
      Pipeline
        .stage("a", dupA)
        .andThen("a", Stage.pure((i: Int) => i))
        .andThen("b", dupB)
        .andThen("b", Stage.pure((i: Int) => i))
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
    "emits NodeStarted and NodeFinished per node, bracketed by RunStarted/RunFinished, in order" in {
      val plan        = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
      assert(
        render(events) == Chunk(
          "run:started",
          "start:inc",
          "finish:inc:Succeeded",
          "start:show",
          "finish:show:Succeeded",
          "run:finished:true"
        )
      )
    }
    "a stage can read its provenance" in {
      val observing =
        Stage[Int, String, Nothing, Any](i => Pipeline.provenance.map(path => path.map(_.label).mkString(",")))
          .named("observer")
      val plan        = sealOrFail(Pipeline.stage(observing))
      val (_, result) = runPure(Emit.run(plan.execute(0))).eval
      assert(result == "observer")
    }
    "a sealed plan is reusable across runs" in {
      val plan      = sealOrFail(Pipeline.stage(inc).andThen(show))
      val (ev1, r1) = runPure(Emit.run(plan.execute(1))).eval
      val (ev2, r2) = runPure(Emit.run(plan.execute(41))).eval
      assert(r1 == "2" && r2 == "42" && ev1.size == 6 && ev2.size == 6)
    }
    "a three-node chain executes and emits in order, with correct ids" in {
      val plan             = sealOrFail(Pipeline.stage(inc).andThen(double).andThen(show))
      val (events, result) = runPure(Emit.run(plan.execute(3))).eval
      assert(result == "8")
      assert(
        render(events) == Chunk(
          "run:started",
          "start:inc",
          "finish:inc:Succeeded",
          "start:double",
          "finish:double:Succeeded",
          "start:show",
          "finish:show:Succeeded",
          "run:finished:true"
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
      val started     = events.collect { case PipelineEvent.NodeStarted(id, _) => id.render }
      assert(started == Chunk("inc", "show"))
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
    "a label collision across sides suffixes instead of erroring" in {
      val plan = sealOrFail(Pipeline.stage(inc).par(Pipeline.stage(Stage.pure((i: Int) => i).named("inc"))))
      assert(plan.nodeIds.map(_.render) == Chunk("inc", "inc-2"))
    }
    "rejects duplicate explicit ids across sides at seal" in {
      Pipeline.stage("inc", inc).par(Pipeline.stage("inc", Stage.pure((i: Int) => i))).seal match
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
        case PipelineEvent.NodeStarted(id, _) => id.render.contains("/")
        case _                                => false
      })
    }
    "child event ids carry the element path" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("parse-all", Pipeline.stage(parse)))
      val (events, _) = runPure(Emit.run(plan.execute(2))).eval
      val childIds    =
        events.collect { case PipelineEvent.NodeStarted(id, _) if id.render.contains("/") => id.render }
      assert(childIds == Chunk("parse-all/0/parse", "parse-all/1/parse"))
    }
    "a child that fails to seal surfaces path-qualified in the parent aggregate" in {
      // Explicit-explicit: a label collision alone would now suffix instead of erroring, so the child's own failure
      // is driven by an explicit id collision, which still errors.
      val dup = Pipeline.stage("parse", parse).andThen("parse", Stage.pure((i: Int) => i))
      Pipeline.stage(sources).fanOut("parse-all", dup).seal match
        case Result.Failure(errors) =>
          assert(errors.errors.exists {
            case SealError.DuplicateNodeId(id) => id.render == "parse-all/parse"
            case _                             => false
          })
        case _ => assert(false)
    }
    "sibling errors do not mask a fan-out child's seal failure" in {
      val dup = Pipeline.stage("parse", parse).andThen("parse", Stage.pure((i: Int) => i))
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
    "the fan-out node's own NodeStarted/NodeFinished bracket the per-element events" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("fo", Pipeline.stage(parse)))
      val (events, _) = runPure(Emit.run(plan.execute(2))).eval
      val rendered    = events.collect {
        case PipelineEvent.NodeStarted(id, _)  => s"start:${id.render}"
        case PipelineEvent.NodeFinished(id, _) => s"finish:${id.render}"
      }
      assert(
        rendered == Chunk(
          "start:sources",
          "finish:sources",
          "start:fo",
          "start:fo/0/parse",
          "finish:fo/0/parse",
          "start:fo/1/parse",
          "finish:fo/1/parse",
          "finish:fo"
        )
      )
    }
    "the fan-out node's own bracket still fires with zero elements" in {
      val plan        = sealOrFail(Pipeline.stage(sources).fanOut("fo", Pipeline.stage(parse)))
      val (events, _) = runPure(Emit.run(plan.execute(0))).eval
      val rendered    = events.collect {
        case PipelineEvent.NodeStarted(id, _)  => s"start:${id.render}"
        case PipelineEvent.NodeFinished(id, _) => s"finish:${id.render}"
      }
      assert(rendered == Chunk("start:sources", "finish:sources", "start:fo", "finish:fo"))
    }
  }

  "fanOutKeyed" - {
    def keyedSources = Stage.pure((n: Int) => Chunk.from(0 until n).map(i => s"item$i")).named("sources")
    def parse        = Stage.pure((s: String) => s.length).named("parse")

    // Currency codes, deliberately out of alphabetical/positional order: "eur" is element 0, "gbp" is element 1,
    // "usd" is element 2. A rendered key equal to `index.toString` would make an implementation that silently fell
    // back to `fanOut`'s own positional id indistinguishable from a correct keyed one; these never coincide with
    // position, so the child ids asserted below only hold if `key` actually drove identity.
    def currencySources = Stage.pure((_: Int) => Chunk("eur", "gbp", "usd")).named("sources")
    def currencyCode    = (s: String) => s

    "child event ids carry the rendered key instead of the index, and element order still drives the report" in {
      val plan = sealOrFail(
        Pipeline.stage(currencySources).fanOutKeyed("fo", currencyCode)(Pipeline.stage(parse))
      )
      val (events, outcome) = Emit.run(Abort.run[FanOutKeyError](plan.execute(3))).eval
      val childIds          =
        events.collect { case PipelineEvent.NodeStarted(id, _) if id.render.contains("/") => id.render }
      assert(childIds == Chunk("fo/eur/parse", "fo/gbp/parse", "fo/usd/parse"))
      assert(outcome == Result.Success(Chunk(3, 3, 3)))
    }
    "a duplicate rendered key fails the fan-out node itself, with no child events" in {
      val plan = sealOrFail(
        Pipeline.stage(keyedSources).fanOutKeyed("fo", (_: String) => "same")(Pipeline.stage(parse))
      )
      val (events, outcome) = Emit.run(Abort.run[FanOutKeyError](plan.execute(2))).eval
      outcome match
        case Result.Failure(FanOutKeyError(parent, key, reason)) =>
          assert(parent.render == "fo")
          assert(key == "same")
          assert(reason == "duplicate key")
        case other => assert(false, s"expected a FanOutKeyError failure, got $other")
      assert(!events.exists {
        case PipelineEvent.NodeStarted(id, _) => id.render.contains("/")
        case _                                => false
      })
      assert(events.contains(PipelineEvent.NodeFinished(nodeId"fo", NodeStatus.Failed)))
    }
    "a key containing '/' fails the fan-out node itself, with no child events" in {
      val plan = sealOrFail(
        Pipeline.stage(keyedSources).fanOutKeyed("fo", (s: String) => s"bad/$s")(Pipeline.stage(parse))
      )
      val (events, outcome) = Emit.run(Abort.run[FanOutKeyError](plan.execute(1))).eval
      outcome match
        case Result.Failure(FanOutKeyError(parent, key, reason)) =>
          assert(parent.render == "fo")
          assert(key == "bad/item0")
          assert(reason == "contains '/'")
        case other => assert(false, s"expected a FanOutKeyError failure, got $other")
      assert(!events.exists {
        case PipelineEvent.NodeStarted(id, _) => id.render.contains("/")
        case _                                => false
      })
      assert(events.contains(PipelineEvent.NodeFinished(nodeId"fo", NodeStatus.Failed)))
    }
    "a key function that throws on the second element still closes the fan-out node's own NodeStarted/NodeFinished, and the throwable propagates" in {
      val throwingKey: String => String = s => if s == "item1" then throw new RuntimeException("bad key") else s
      val plan                          = sealOrFail(
        Pipeline.stage(keyedSources).fanOutKeyed("fo", throwingKey)(Pipeline.stage(parse))
      )
      val (events, outcome) = Emit.run(Abort.run[FanOutKeyError](plan.execute(2))).eval
      outcome match
        case Result.Panic(ex) => assert(ex.getMessage == "bad key")
        case other            => assert(false, s"expected a panic, got $other")
      assert(!events.exists {
        case PipelineEvent.NodeStarted(id, _) => id.render.contains("/")
        case _                                => false
      })
      assert(events.contains(PipelineEvent.NodeFinished(nodeId"fo", NodeStatus.Failed)))
    }
  }

  "branch" - {
    def big   = Pipeline.stage(Stage.pure((i: Int) => s"big:$i").named("big"))
    def small = Pipeline.stage(Stage.pure((i: Int) => s"small:$i").named("small"))

    "takes the true arm and skips the false arm's nodes" in {
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 10)(big, small))
      val (events, r) = runPure(Emit.run(plan.execute(100))).eval
      assert(r == "big:101")
      assert(events.contains(PipelineEvent.NodeFinished(nodeId"small", NodeStatus.Skipped)))
      assert(
        events.collect { case PipelineEvent.NodeFinished(id, NodeStatus.Skipped) => id.render } == Chunk("small")
      )
    }
    "takes the false arm and skips the true arm's nodes" in {
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 10)(big, small))
      val (events, r) = runPure(Emit.run(plan.execute(1))).eval
      assert(r == "small:2")
      assert(events.collect { case PipelineEvent.NodeFinished(id, NodeStatus.Skipped) => id.render } == Chunk("big"))
    }
    "when yields Present on the taken path and Absent otherwise" in {
      val plan         = sealOrFail(Pipeline.stage(inc).when(_ > 10)(big))
      val (_, taken)   = runPure(Emit.run(plan.execute(100))).eval
      val (_, untaken) = runPure(Emit.run(plan.execute(1))).eval
      assert(taken == Present("big:101") && untaken == Absent)
    }
    "a label collision across arms suffixes instead of erroring" in {
      val dupArm = Pipeline.stage(Stage.pure((i: Int) => s"x:$i").named("big"))
      val plan   = sealOrFail(Pipeline.stage(inc).branch(_ > 10)(big, dupArm))
      // index 0 is `inc`, index 1 is the branch's own (label-less) slot — see the position-fallback comment below —
      // and indices 2/3 are the two arms, both labelled "big".
      assert(plan.nodeIds.map(_.render) == Chunk("inc", "node-1", "big", "big-2"))
    }
    "duplicate explicit ids across arms are rejected at seal" in {
      val armA = Pipeline.stage("dup", Stage.pure((i: Int) => s"a:$i"))
      val armB = Pipeline.stage("dup", Stage.pure((i: Int) => s"b:$i"))
      Pipeline.stage(inc).branch(_ > 10)(armA, armB).seal match
        case Result.Failure(errors) => assert(errors.errors.nonEmpty)
        case _                      => assert(false)
    }
    "the branch node's own NodeStarted/NodeFinished bracket the taken arm and the skip events" in {
      val wrap        = Pipeline.stage(Stage.pure((i: Int) => i.toString).named("wrap"))
      val other       = Pipeline.stage(Stage.pure((i: Int) => i).named("other"))
      val plan        = sealOrFail(Pipeline.stage(inc).branch(_ > 0)(wrap, other))
      val (events, _) = runPure(Emit.run(plan.execute(1))).eval
      // The branch node itself has no explicit id and no label (a pipeline construct, not a `Stage`), so it falls
      // back to its position — "node-1": index 0 is `inc`, index 1 is the branch's own slot (see `DefElem.BranchElem`
      // and `Sealing.sealChain`'s position-fallback, unchanged by this task).
      assert(
        render(events) == Chunk(
          "run:started",
          "start:inc",
          "finish:inc:Succeeded",
          "start:node-1",
          "start:wrap",
          "finish:wrap:Succeeded",
          "finish:other:Skipped",
          "finish:node-1:Succeeded",
          "run:finished:true"
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
      assert(
        events.collect { case PipelineEvent.NodeFinished(id, NodeStatus.Skipped) => id.render } ==
          Chunk("sources", "parse-all")
      )
    }
    "a fan-out nested in a branch arm still surfaces its own child seal failure, without a sibling error masking it" in {
      def sources = Stage.pure((n: Int) => Chunk.from(0 until n).map(_.toString)).named("sources")
      def parse   = Stage.pure((s: String) => s.length).named("parse")

      val dup           = Pipeline.stage("parse", parse).andThen("parse", Stage.pure((i: Int) => i))
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
    "a panicking stage still closes its NodeStarted with NodeFinished(Failed), and the panic keeps propagating" in {
      val boom = Stage.pure((i: Int) => (throw new RuntimeException("boom")): Int).named("boom")
      val plan = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      // `Emit.run` outside `Abort.run[Nothing]`, same nesting as the typed-Abort case below: the inner boundary
      // absorbs the panic into a `Result.Panic` value before `Emit.run` ever needs to observe a short-circuit, so
      // the events accumulated up to the panic survive. `Abort.run` catches a raw panic regardless of the `E` it
      // was asked to run — that is what "the panic keeps propagating" means here: nothing between the stage's own
      // `bracketed` rethrow, `execute`'s own outer `Effect.catching` rethrow, and this outer boundary silently
      // swallowed it.
      val (events, outcome) = Emit.run(Abort.run[Nothing](plan.execute(1))).eval
      outcome match
        case Result.Panic(ex) => assert(ex.getMessage == "boom")
        case other            => assert(false, s"expected a panic, got $other")
      assert(
        render(events) == Chunk(
          "run:started",
          "start:inc",
          "finish:inc:Succeeded",
          "start:boom",
          "finish:boom:Failed",
          "run:finished:false"
        )
      )
    }
    "a stage whose typed Abort short-circuits surfaces the original error" in {
      val boom: Stage[Int, Int, String, Any] = Stage((_: Int) => Abort.fail("boom")).named("boom")
      val plan                               = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      val (_, outcome)                       = Emit.run(Abort.run[String](plan.execute(1))).eval
      assert(outcome == Result.Failure("boom"))
    }
    // Closing `boom`'s own `NodeStarted` on a typed `Abort[E]` short-circuit cannot reuse the panic mechanism: a
    // typed abort is not a JVM `throw`, so `kyo.kernel.Effect.catching` (which `bracketed` wraps every node with)
    // never sees it — see `SealedPipeline#closeOpenNodes`'s own doc. `execute`'s single `Abort.tapError[E]`, wrapping
    // the whole chain, closes every id the tracking `Var` still holds open before re-raising the same failure.
    "a stage whose typed Abort short-circuits still closes its NodeStarted with NodeFinished(Failed)" in {
      val boom: Stage[Int, Int, String, Any] = Stage((_: Int) => Abort.fail("boom")).named("boom")
      val plan                               = sealOrFail(Pipeline.stage(inc).andThen(boom).andThen(show))
      val (events, outcome)                  = Emit.run(Abort.run[String](plan.execute(1))).eval
      assert(outcome == Result.Failure("boom"))
      assert(
        render(events) == Chunk(
          "run:started",
          "start:inc",
          "finish:inc:Succeeded",
          "start:boom",
          "finish:boom:Failed",
          "run:finished:false"
        )
      )
    }
    "every started node closes even when a later node aborts" in {
      val ok: Stage[Int, Int, Nothing, Any]  = Stage((i: Int) => i + 1)
      val boom: Stage[Int, Int, String, Any] = Stage((_: Int) => Abort.fail("halt"))
      val plan                               = sealOrFail(Pipeline.stage(nodeId"ok", ok).andThen(nodeId"boom", boom))
      val (events, _)                        = Emit.run(Abort.run[String](plan.execute(1))).eval
      val started                            = events.collect { case PipelineEvent.NodeStarted(id, _) => id }
      val finished                           = events.collect { case PipelineEvent.NodeFinished(id, _) => id }
      assert(started.sorted(using Ordering.by(_.render)) == finished.sorted(using Ordering.by(_.render)))
    }
    "every started node closes even when a node nested in a taken branch arm aborts" in {
      val boom: Stage[Int, Int, String, Any] = Stage((_: Int) => Abort.fail("halt"))
      val takenArm                           = Pipeline.stage(nodeId"boom", boom)
      val untakenArm                         = Pipeline.stage(Stage.pure((i: Int) => i).named("other"))
      val plan                               = sealOrFail(Pipeline.stage(inc).branch(_ > 0)(takenArm, untakenArm))
      val (events, outcome)                  = Emit.run(Abort.run[String](plan.execute(1))).eval
      assert(outcome == Result.Failure("halt"))
      val started  = events.collect { case PipelineEvent.NodeStarted(id, _) => id }
      val finished = events.collect { case PipelineEvent.NodeFinished(id, _) => id }
      assert(started.sorted(using Ordering.by(_.render)) == finished.sorted(using Ordering.by(_.render)))
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
