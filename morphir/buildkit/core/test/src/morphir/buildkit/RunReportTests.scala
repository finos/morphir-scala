package morphir.buildkit

import kyo.*
import kyo.test.*

class RunReportTests extends Test[Any]:

  private def sealOrFail[I, O, E, S](p: Pipeline[I, O, E, S]): SealedPipeline[I, O, E, S] =
    p.seal match
      case Result.Success(sealed_) => sealed_
      case other                   => throw new AssertionError(s"seal failed: $other")

  /** Render an event trace as short tags, for compact equality assertions. Mirrors `PipelineTests`'s own renderer. */
  private def render(events: Chunk[PipelineEvent]): Chunk[String] =
    events.map {
      case PipelineEvent.RunStarted           => "run:started"
      case PipelineEvent.RunFinished(ok)      => s"run:finished:$ok"
      case PipelineEvent.NodeStarted(id, _)   => s"start:${id.render}"
      case PipelineEvent.NodeFinished(id, st) => s"finish:${id.render}:$st"
      case PipelineEvent.NodeProgress(id, m)  => s"progress:${id.render}:$m"
    }

  // Every fixture stage is ascribed: a bare `Stage(...)` with no expected type infers `S = Nothing`, a valid but
  // unusable row (`.eval` needs exactly `Any`, and `S` is contravariant). See `PipelineTests`'s own note.
  private val inc: Stage[Int, Int, Nothing, Any]     = Stage((i: Int) => i + 1)
  private val double: Stage[Int, Int, Nothing, Any]  = Stage((i: Int) => i * 2)
  private val boom: Stage[Int, Int, RunBoom, Any]    = Stage((_: Int) => Abort.fail(RunBoom("b failed")))
  private val explode: Stage[Int, Int, Nothing, Any] = Stage((_: Int) => (throw new RuntimeException("kaboom")): Int)
  private val stringify: Stage[Int, String, Nothing, Any] = Stage((i: Int) => i.toString)

  /** `a` succeeds, `b` aborts with a typed error, `c` never runs. */
  private def failingChain =
    Pipeline.stage(nodeId"a", inc).andThen(nodeId"b", boom).andThen(nodeId"c", double)

  "typed halting" - {
    "a typed failure is folded into Failed, and everything downstream is Blocked with root causes" in {
      val plan             = sealOrFail(failingChain)
      val (events, report) = Emit.run(plan.runReport(1)).eval
      assert(report.outcome(nodeId"a") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(nodeId"b") == Present(NodeOutcome.Failed(Result.Failure(RunBoom("b failed")))))
      assert(report.outcome(nodeId"c") == Present(NodeOutcome.Blocked(Chunk(nodeId"b"), Chunk(nodeId"b"))))
      assert(report.result.isEmpty)
      assert(!report.isSuccess)
      assert(report.failed.map(_.id.render) == Chunk("b"))
      assert(report.blocked.map(_.id.render) == Chunk("c"))
      assert(
        render(events) == Chunk(
          "run:started",
          "start:a",
          "finish:a:Succeeded",
          "start:b",
          "finish:b:Failed",
          "finish:c:Blocked",
          "run:finished:false"
        )
      )
    }

    "transitive blocking names the immediate predecessor but keeps the originating root cause" in {
      val plan = sealOrFail(
        Pipeline.stage(nodeId"a", inc).andThen(nodeId"b", boom).andThen(nodeId"c", double).andThen(nodeId"d", double)
      )
      val (_, report) = Emit.run(plan.runReport(1)).eval
      assert(report.outcome(nodeId"c") == Present(NodeOutcome.Blocked(Chunk(nodeId"b"), Chunk(nodeId"b"))))
      assert(report.outcome(nodeId"d") == Present(NodeOutcome.Blocked(Chunk(nodeId"c"), Chunk(nodeId"b"))))
    }

    "a panic in a node body becomes Failed(Panic), not a torn run" in {
      val plan = sealOrFail(Pipeline.stage(nodeId"a", inc).andThen(nodeId"boom", explode).andThen(nodeId"c", double))
      val (events, report) = Emit.run(plan.runReport(1)).eval
      report.outcome(nodeId"boom") match
        case Present(NodeOutcome.Failed(Result.Panic(ex))) => assert(ex.getMessage == "kaboom")
        case other                                         => assert(false, s"expected Failed(Panic), got $other")
      assert(report.outcome(nodeId"c") == Present(NodeOutcome.Blocked(Chunk(nodeId"boom"), Chunk(nodeId"boom"))))
      assert(report.result.isEmpty)
      assert(render(events).last == "run:finished:false")
    }

    "Pipeline.halt inside a stage body yields Failed in the report" in {
      val halting: Stage[Int, Int, RunBoom, Any] = Stage((_: Int) => Pipeline.halt(RunBoom("halted")))
      val plan        = sealOrFail(Pipeline.stage(nodeId"a", inc).andThen(nodeId"h", halting))
      val (_, report) = Emit.run(plan.runReport(1)).eval
      assert(report.outcome(nodeId"h") == Present(NodeOutcome.Failed(Result.Failure(RunBoom("halted")))))
      assert(report.outcome(nodeId"a") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.result.isEmpty)
    }
  }

  "success" - {
    "carries the value and Executed provenance for every node" in {
      val plan = sealOrFail(Pipeline.stage(nodeId"a", inc).andThen(nodeId"b", double).andThen(nodeId"c", stringify))
      val (events, report) = Emit.run(plan.runReport(3)).eval
      assert(report.result == Present("8"))
      assert(report.isSuccess)
      assert(report.failed.isEmpty && report.blocked.isEmpty)
      assert(report.nodes.map(_.outcome) == Chunk.fill(3)(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.nodes.map(_.id.render) == Chunk("a", "b", "c"))
      assert(report.outcome(nodeId"b") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(nodeId"missing") == Absent)
      assert(
        render(events) == Chunk(
          "run:started",
          "start:a",
          "finish:a:Succeeded",
          "start:b",
          "finish:b:Succeeded",
          "start:c",
          "finish:c:Succeeded",
          "run:finished:true"
        )
      )
    }
  }

  "par" - {
    "both sides succeeding zips the value and reports both nodes Succeeded" in {
      val plan        = sealOrFail(Pipeline.stage(nodeId"l", inc).par(Pipeline.stage(nodeId"r", double)))
      val (_, report) = Emit.run(plan.runReport(3)).eval
      assert(report.result == Present((4, 6)))
      assert(report.isSuccess)
      assert(report.outcome(nodeId"l") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(nodeId"r") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
    }

    "one side failing blocks what follows, naming both par sides as the immediate predecessor" in {
      // Locks the `leftExits ++ rightExits` merge `reportPar` computes for its own outgoing gate: `l` succeeds (its
      // own exit is itself) and `r` fails (its own exit, on a `Blocked` gate, is also itself), so the node after the
      // par sees both as its immediate predecessor, not just the one that failed.
      val tail: Stage[(Int, Int), Int, Nothing, Any] = Stage.pure((t: (Int, Int)) => t._1 + t._2)
      val plan                                       = sealOrFail(
        Pipeline.stage(nodeId"l", inc).par(Pipeline.stage(nodeId"r", boom)).andThen(nodeId"tail", tail)
      )
      val (_, report) = Emit.run(plan.runReport(1)).eval
      assert(report.outcome(nodeId"l") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(nodeId"r") == Present(NodeOutcome.Failed(Result.Failure(RunBoom("b failed")))))
      assert(
        report.outcome(nodeId"tail") ==
          Present(NodeOutcome.Blocked(Chunk(nodeId"l", nodeId"r"), Chunk(nodeId"r")))
      )
      assert(report.result.isEmpty)
    }
  }

  "collation" - {
    "report order equals ordinal order and is identical across runs" in {
      // The false arm is taken, so execution order (branch, false arm, then the true arm's skips) differs from
      // definition order (branch, true arm, false arm) — collation must follow the seal-assigned ordinals.
      val plan = sealOrFail(
        Pipeline
          .stage(nodeId"a", inc)
          .branch(_ > 100)(
            Pipeline.stage(nodeId"big", double),
            Pipeline.stage(nodeId"small", double)
          )
          .andThen(nodeId"z", stringify)
      )
      assert(plan.ordinals.map((id, o) => s"${id.render}:$o") == Chunk("a:0", "node-1:1", "big:2", "small:3", "z:4"))
      val (ev1, r1) = Emit.run(plan.runReport(1)).eval
      val (ev2, r2) = Emit.run(plan.runReport(1)).eval
      assert(r1.nodes == r2.nodes)
      assert(render(ev1) == render(ev2))
      assert(r1.nodes.map(_.id) == plan.nodeIds)
      assert(r1.nodes.map(_.ordinal) == Chunk(0, 1, 2, 3, 4))
      assert(r1.nodes.map(n => (n.id, n.ordinal)) == plan.ordinals)
      assert(r1.outcome(nodeId"big") == Present(NodeOutcome.Skipped(SkipReason.BranchNotTaken(nodeId"node-1"))))
      assert(r1.outcome(nodeId"small") == Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(r1.result == Present("4"))
    }
  }

  "branch" - {
    "the branch node's own NodeFinished closes after its arm's events and the untaken arm's skips" in {
      val plan = sealOrFail(
        Pipeline
          .stage(nodeId"a", inc)
          .branch(_ > 100)(Pipeline.stage(nodeId"big", double), Pipeline.stage(nodeId"small", double))
      )
      val (events, _) = Emit.run(plan.runReport(1)).eval
      // The same bracket shape `execute` emits (see `PipelineTests`'s own branch event assertion): a composite node's
      // `NodeFinished` closes after every event its arms produced.
      assert(
        render(events) == Chunk(
          "run:started",
          "start:a",
          "finish:a:Succeeded",
          "start:node-1",
          "start:small",
          "finish:small:Succeeded",
          "finish:big:Skipped",
          "finish:node-1:Succeeded",
          "run:finished:true"
        )
      )
    }

    "a predicate that throws fails the branch node and blocks both arms" in {
      val plan = sealOrFail(
        Pipeline
          .stage(nodeId"a", inc)
          .branch(_ => throw new IllegalStateException("bad predicate"))(
            Pipeline.stage(nodeId"big", double),
            Pipeline.stage(nodeId"small", double)
          )
      )
      val (events, report) = Emit.run(plan.runReport(1)).eval
      report.outcome(nodeId"node-1") match
        case Present(NodeOutcome.Failed(Result.Panic(ex))) => assert(ex.getMessage == "bad predicate")
        case other                                         => assert(false, s"expected Failed(Panic), got $other")
      val blockedOnBranch = Present(NodeOutcome.Blocked(Chunk(nodeId"node-1"), Chunk(nodeId"node-1")))
      assert(report.outcome(nodeId"big") == blockedOnBranch)
      assert(report.outcome(nodeId"small") == blockedOnBranch)
      assert(report.result.isEmpty)
      // Locks the started-branch bracket shape: the node's own `NodeStarted` fires before the predicate runs, and
      // since it started, its own `NodeFinished` closes *after* both arms' — the same "close after every event the
      // arms produced" contract `reportBranch`'s own doc states for the `started = true` case.
      assert(
        render(events) == Chunk(
          "run:started",
          "start:a",
          "finish:a:Succeeded",
          "start:node-1",
          "finish:big:Blocked",
          "finish:small:Blocked",
          "finish:node-1:Failed",
          "run:finished:false"
        )
      )
    }
  }

  "run modes" - {
    "FailFast cancels the unstarted par sibling; KeepGoing runs it" in {
      var ran                                    = 0
      val tracked: Stage[Int, Int, Nothing, Any] = Stage { (i: Int) =>
        ran += 1
        i
      }
      val plan =
        sealOrFail(Pipeline.stage(nodeId"l", boom).par(Pipeline.stage(nodeId"r", tracked)))

      val (ffEvents, failFast) = Emit.run(plan.runReport(1, RunMode.FailFast)).eval
      assert(ran == 0)
      assert(failFast.outcome(nodeId"r") == Present(NodeOutcome.Cancelled))
      assert(render(ffEvents).contains("finish:r:Cancelled"))
      assert(!render(ffEvents).contains("start:r"))

      val (_, keepGoing) = Emit.run(plan.runReport(1, RunMode.KeepGoing)).eval
      assert(ran == 1)
      assert(keepGoing.outcome(nodeId"r") == Present(NodeOutcome.Succeeded(Provenance.Executed)))

      // Mode never rewrites an outcome: `l` failed identically under both.
      val failure = Present(NodeOutcome.Failed(Result.Failure(RunBoom("b failed"))))
      assert(failFast.outcome(nodeId"l") == failure && keepGoing.outcome(nodeId"l") == failure)
      assert(failFast.result.isEmpty && keepGoing.result.isEmpty)
    }

    "cancellation reaches every node of the withdrawn sibling, not just its first" in {
      val plan = sealOrFail(
        Pipeline
          .stage(nodeId"l", boom)
          .par(Pipeline.stage(nodeId"r1", inc).andThen(nodeId"r2", double))
      )
      val (_, report) = Emit.run(plan.runReport(1)).eval
      assert(report.outcome(nodeId"r1") == Present(NodeOutcome.Cancelled))
      assert(report.outcome(nodeId"r2") == Present(NodeOutcome.Cancelled))
      // Locks the ordinal arithmetic a fork uses to number its right side (`base + left.size`) against the seal's own.
      assert(report.nodes.map(n => (n.id, n.ordinal)) == plan.ordinals)
    }

    "a linear chain reports identically under both modes" in {
      val plan    = sealOrFail(failingChain)
      val (_, ff) = Emit.run(plan.runReport(1, RunMode.FailFast)).eval
      val (_, kg) = Emit.run(plan.runReport(1, RunMode.KeepGoing)).eval
      assert(ff.nodes == kg.nodes)
    }
  }

  "fan-out" - {
    "children report under parent/<index>/<childId>, and a child's typed failure blocks the parent" in {
      val sources: Stage[Int, Chunk[Int], Nothing, Any] = Stage((n: Int) => Chunk.from(0 until n))
      val childBoom: Stage[Int, Int, RunBoom, Any]      =
        Stage((i: Int) => if i == 1 then Abort.fail(RunBoom("child")) else i)
      val plan = sealOrFail(
        Pipeline.stage(nodeId"src", sources).fanOut("each", Pipeline.stage(nodeId"child", childBoom))
      )
      val (events, report) = Emit.run(plan.runReport(3, RunMode.KeepGoing)).eval
      // A composite that started and then produced nothing closes its own bracket with `Blocked` — the one Blocked
      // that is paired with a `NodeStarted`. `PipelineEvent` and `NodeOutcome` both license this shape explicitly.
      assert(
        render(events) == Chunk(
          "run:started",
          "start:src",
          "finish:src:Succeeded",
          "start:each",
          "start:each/0/child",
          "finish:each/0/child:Succeeded",
          "start:each/1/child",
          "finish:each/1/child:Failed",
          "start:each/2/child",
          "finish:each/2/child:Succeeded",
          "finish:each:Blocked",
          "run:finished:false"
        )
      )
      assert(report.nodes.map(_.id.render) == Chunk("src", "each", "each/0/child", "each/1/child", "each/2/child"))
      assert(report.outcome(NodeId.unsafe(Chunk("each", "0", "child"))) ==
        Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(NodeId.unsafe(Chunk("each", "1", "child"))) ==
        Present(NodeOutcome.Failed(Result.Failure(RunBoom("child")))))
      assert(report.outcome(NodeId.unsafe(Chunk("each", "2", "child"))) ==
        Present(NodeOutcome.Succeeded(Provenance.Executed)))
      assert(report.outcome(nodeId"each").map(_.status) == Present(NodeStatus.Blocked))
      assert(report.result.isEmpty)
      // Children have no ordinal of their own: they carry the fan-out node's, which collates them right after it.
      assert(report.nodes.map(_.ordinal) == Chunk(0, 1, 1, 1, 1))
    }

    "a multi-node child chain stays contiguous per element, ahead of the plan nodes that follow the fan-out" in {
      val chunked: Stage[Int, Chunk[Int], Nothing, Any] = Stage((n: Int) => Chunk.from(0 until n))
      val total: Stage[Chunk[Int], Int, Nothing, Any]   = Stage((cs: Chunk[Int]) => cs.sum)
      val plan                                          = sealOrFail(
        Pipeline
          .stage(nodeId"src", chunked)
          .fanOut("each", Pipeline.stage(nodeId"c1", inc).andThen(nodeId"c2", double))
          .andThen(nodeId"tail", total)
      )
      val (_, report) = Emit.run(plan.runReport(2)).eval
      // Every node of a child chain carries the parent's ordinal, so element 0's whole subtree precedes element 1's
      // and neither spills past the fan-out into `tail`'s own slot.
      assert(
        report.nodes.map(_.id.render) ==
          Chunk("src", "each", "each/0/c1", "each/0/c2", "each/1/c1", "each/1/c2", "tail")
      )
      assert(report.nodes.map(_.ordinal) == Chunk(0, 1, 1, 1, 1, 1, 2))
      assert(plan.ordinals.map((id, o) => s"${id.render}:$o") == Chunk("src:0", "each:1", "tail:2"))
      assert(report.result == Present(6))
    }

    "an unrun fan-out reports only its own node, blocked on its predecessor" in {
      val chunked: Stage[Int, Chunk[Int], Nothing, Any] = Stage((n: Int) => Chunk.from(0 until n))
      val failing: Stage[Int, Int, RunBoom, Any]        = Stage((_: Int) => Abort.fail(RunBoom("upstream")))
      val plan                                          = sealOrFail(
        Pipeline
          .stage(nodeId"a", failing)
          .andThen(nodeId"src", chunked)
          .fanOut("each", Pipeline.stage(nodeId"child", inc))
      )
      val (_, report) = Emit.run(plan.runReport(1)).eval
      assert(report.nodes.map(_.id.render) == Chunk("a", "src", "each"))
      assert(report.outcome(nodeId"each") == Present(NodeOutcome.Blocked(Chunk(nodeId"src"), Chunk(nodeId"a"))))
    }

    "FailFast cancels the elements after a failing one" in {
      val sources: Stage[Int, Chunk[Int], Nothing, Any] = Stage((n: Int) => Chunk.from(0 until n))
      val childBoom: Stage[Int, Int, RunBoom, Any]      =
        Stage((i: Int) => if i == 1 then Abort.fail(RunBoom("child")) else i)
      val plan = sealOrFail(
        Pipeline.stage(nodeId"src", sources).fanOut("each", Pipeline.stage(nodeId"child", childBoom))
      )
      val (_, report) = Emit.run(plan.runReport(3, RunMode.FailFast)).eval
      assert(report.outcome(NodeId.unsafe(Chunk("each", "2", "child"))) == Present(NodeOutcome.Cancelled))
    }
  }

  "fan-out keyed" - {
    val keyedSources: Stage[Int, Chunk[String], Nothing, Any] =
      Stage((n: Int) => Chunk.from(0 until n).map(i => s"item$i"))
    val childOf: Stage[String, Int, Nothing, Any] = Stage((s: String) => s.length)

    // Currency codes, deliberately out of alphabetical/positional order: "eur" is element 0, "gbp" is element 1,
    // "usd" is element 2. A rendered key equal to `index.toString` would make an implementation that silently fell
    // back to the unkeyed fan-out's own positional id indistinguishable from a correct keyed one; these never
    // coincide with position, so the ids asserted below only hold if `key` actually drove identity.
    val currencySources: Stage[Int, Chunk[String], Nothing, Any] = Stage((_: Int) => Chunk("eur", "gbp", "usd"))
    val currencyCode                                             = (s: String) => s

    "children report under parent/<key>/<childId>, in input order" in {
      val plan = sealOrFail(
        Pipeline.stage(nodeId"src", currencySources).fanOutKeyed("each", currencyCode)(
          Pipeline.stage(nodeId"child", childOf)
        )
      )
      val (events, report) = Emit.run(plan.runReport(3)).eval
      assert(
        render(events) == Chunk(
          "run:started",
          "start:src",
          "finish:src:Succeeded",
          "start:each",
          "start:each/eur/child",
          "finish:each/eur/child:Succeeded",
          "start:each/gbp/child",
          "finish:each/gbp/child:Succeeded",
          "start:each/usd/child",
          "finish:each/usd/child:Succeeded",
          "finish:each:Succeeded",
          "run:finished:true"
        )
      )
      assert(
        report.nodes.map(_.id.render) ==
          Chunk("src", "each", "each/eur/child", "each/gbp/child", "each/usd/child")
      )
      assert(report.result == Present(Chunk(3, 3, 3)))
    }

    "a duplicate rendered key fails the fan-out node itself, with children unstarted" in {
      val plan = sealOrFail(
        Pipeline.stage(nodeId"src", keyedSources).fanOutKeyed("each", (_: String) => "same")(
          Pipeline.stage(nodeId"child", childOf)
        )
      )
      val (events, report) = Emit.run(plan.runReport(2)).eval
      assert(
        render(events) == Chunk(
          "run:started",
          "start:src",
          "finish:src:Succeeded",
          "start:each",
          "finish:each:Failed",
          "run:finished:false"
        )
      )
      report.outcome(nodeId"each") match
        case Present(NodeOutcome.Failed(Result.Failure(FanOutKeyError(parent, key, reason)))) =>
          assert(parent.render == "each")
          assert(key == "same")
          assert(reason == "duplicate key")
        case other => assert(false, s"expected a FanOutKeyError failure, got $other")
      assert(report.nodes.map(_.id.render) == Chunk("src", "each"))
      assert(report.result.isEmpty)
    }

    "a key containing '/' fails the fan-out node itself, with children unstarted" in {
      val plan = sealOrFail(
        Pipeline.stage(nodeId"src", keyedSources).fanOutKeyed("each", (s: String) => s"bad/$s")(
          Pipeline.stage(nodeId"child", childOf)
        )
      )
      val (events, report) = Emit.run(plan.runReport(1)).eval
      assert(
        render(events) == Chunk(
          "run:started",
          "start:src",
          "finish:src:Succeeded",
          "start:each",
          "finish:each:Failed",
          "run:finished:false"
        )
      )
      report.outcome(nodeId"each") match
        case Present(NodeOutcome.Failed(Result.Failure(FanOutKeyError(parent, key, reason)))) =>
          assert(parent.render == "each")
          assert(key == "bad/item0")
          assert(reason == "contains '/'")
        case other => assert(false, s"expected a FanOutKeyError failure, got $other")
      assert(report.nodes.map(_.id.render) == Chunk("src", "each"))
      assert(report.result.isEmpty)
    }

    "a key function that throws on the second element reports the fan-out node Failed(Panic), not a torn run" in {
      val throwingKey: String => String = s => if s == "item1" then throw new RuntimeException("bad key") else s
      val plan                          = sealOrFail(
        Pipeline.stage(nodeId"src", keyedSources).fanOutKeyed("each", throwingKey)(
          Pipeline.stage(nodeId"child", childOf)
        )
      )
      val (events, report) = Emit.run(plan.runReport(2)).eval
      report.outcome(nodeId"each") match
        case Present(NodeOutcome.Failed(Result.Panic(ex))) => assert(ex.getMessage == "bad key")
        case other                                         => assert(false, s"expected Failed(Panic), got $other")
      assert(report.nodes.map(_.id.render) == Chunk("src", "each"))
      assert(report.result.isEmpty)
      assert(!report.isSuccess)
      // Every started node closes with a matching finish — the run is not torn.
      val started  = events.collect { case PipelineEvent.NodeStarted(id, _) => id }
      val finished = events.collect { case PipelineEvent.NodeFinished(id, _) => id }
      assert(started.sorted(using Ordering.by(_.render)) == finished.sorted(using Ordering.by(_.render)))
      assert(!events.exists {
        case PipelineEvent.NodeStarted(id, _) => id.render.contains("/")
        case _                                => false
      })
      assert(render(events).last == "run:finished:false")
    }
  }

final case class RunBoom(msg: String) derives CanEqual
