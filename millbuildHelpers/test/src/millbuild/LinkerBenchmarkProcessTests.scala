package millbuild

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.atomic.AtomicBoolean

import utest.*

object LinkerBenchmarkProcessTests extends TestSuite {
  import LinkerBenchmark.Outcome
  import LinkerBenchmarkProcess.*

  private val shell = os.Path("/bin/sh")

  private def shellUnavailable: Boolean = {
    val unavailable = !os.exists(shell)
    if unavailable then println("SKIPPED: /bin/sh is unavailable on this platform")
    unavailable
  }

  private def withWorkspace[A](run: os.Path => A): A = {
    val workspace = os.temp.dir(prefix = "linker-process-")
    try run(workspace)
    finally os.remove.all(workspace)
  }

  private def processAlive(pid: Long): Boolean =
    ProcessHandle.of(pid).toScala.exists(_.isAlive)

  private def forceDead(pid: Long): Unit =
    ProcessHandle.of(pid).toScala.filter(_.isAlive).foreach(_.destroyForcibly())

  private def forceAndAwaitDead(pid: Long): Unit = {
    forceDead(pid)
    awaitDead(pid)
  }

  private def awaitDead(pid: Long): Unit = {
    val deadline = System.nanoTime() + 5.seconds.toNanos
    @annotation.tailrec
    def loop(): Unit =
      if !processAlive(pid) || System.nanoTime() >= deadline then ()
      else {
        Thread.sleep(20L)
        loop()
      }
    loop()
    assert(!processAlive(pid))
  }

  private def awaitExists(path: os.Path): Unit = {
    val deadline = System.nanoTime() + 3.seconds.toNanos
    @annotation.tailrec
    def loop(): Unit =
      if os.exists(path) || System.nanoTime() >= deadline then ()
      else {
        Thread.sleep(20L)
        loop()
      }
    loop()
    assert(os.exists(path))
  }

  private def awaitPositiveSample(sampler: AggregateRssSampler): AggregateRssSnapshot = {
    val deadline = System.nanoTime() + 3.seconds.toNanos
    @annotation.tailrec
    def loop(): AggregateRssSnapshot = {
      val snapshot = sampler.snapshot()
      if snapshot.wholeRunPeakKiB > 0L && snapshot.strategyWindowPeakKiB > 0L then snapshot
      else if System.nanoTime() >= deadline then snapshot
      else {
        Thread.sleep(20L)
        loop()
      }
    }
    val snapshot = loop()
    assert(snapshot.wholeRunPeakKiB > 0L)
    assert(snapshot.strategyWindowPeakKiB > 0L)
    snapshot
  }

  private def destroyTree(process: Process): Unit = {
    val root        = process.toHandle
    val stream      = root.descendants()
    val descendants =
      try stream.iterator().asScala.toSeq
      finally stream.close()
    descendants.filter(_.isAlive).foreach(_.destroy())
    if root.isAlive then root.destroy()

    @annotation.tailrec
    def loop(handles: Seq[ProcessHandle], deadline: Long): Seq[ProcessHandle] = {
      val survivors = handles.filter(_.isAlive)
      if survivors.isEmpty || System.nanoTime() >= deadline then survivors
      else {
        Thread.sleep(20L)
        loop(survivors, deadline)
      }
    }
    val survivors = loop(descendants :+ root, System.nanoTime() + 3.seconds.toNanos)
    survivors.foreach(_.destroyForcibly())
    val afterForce = loop(survivors, System.nanoTime() + 1.second.toNanos)
    process.getInputStream.close()
    process.getErrorStream.close()
    process.getOutputStream.close()
    assert(afterForce.isEmpty)
  }

  private def runShell(
      workspace: os.Path,
      script: String,
      timeout: FiniteDuration = 5.seconds,
      environment: Map[String, String] = Map.empty
  ): ProcessResult =
    LinkerBenchmarkProcess.run(
      Seq(shell.toString, "-c", script),
      workspace,
      environment,
      timeout,
      workspace / "startup",
      workspace / "stdout.log",
      workspace / "stderr.log"
    )

  val tests = Tests {
    test("rejects invalid requests") {
      withWorkspace { workspace =>
        val empty = LinkerBenchmarkProcess.run(
          Seq.empty,
          workspace,
          Map.empty,
          1.second,
          workspace / "marker",
          workspace / "out",
          workspace / "err"
        )
        val nonpositive = LinkerBenchmarkProcess.run(
          Seq("ignored"),
          workspace,
          Map.empty,
          Duration.Zero,
          workspace / "marker",
          workspace / "out",
          workspace / "err"
        )
        assert(empty.outcome == Outcome.Failed)
        assert(empty.exitCode.isEmpty)
        assert(nonpositive.outcome == Outcome.Failed)
        assert(nonpositive.exitCode.isEmpty)
      }
    }

    test("successful process observes startup and redirects output") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result = runShell(workspace, ": > startup; echo done")
          assert(result.outcome == Outcome.Succeeded)
          assert(result.exitCode.contains(0))
          assert(result.startupMillis.nonEmpty)
          assert(result.wallMillis >= result.startupMillis.get)
          assert(result.peakRssKiB >= 0L)
          assert(os.read(workspace / "stdout.log").trim == "done")
        }
    }

    test("missing marker remains absent") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result = runShell(workspace, "echo done")
          assert(result.outcome == Outcome.Succeeded)
          assert(result.startupMillis.isEmpty)
        }
    }

    test("stale marker from an earlier run does not count as startup") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          os.write(workspace / "startup", "stale")
          val result = runShell(workspace, "echo done")
          assert(result.outcome == Outcome.Succeeded)
          assert(result.startupMillis.isEmpty)
        }
    }

    test("nonzero exit is failed") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result = runShell(workspace, "echo bad >&2; exit 7")
          assert(result.outcome == Outcome.Failed)
          assert(result.exitCode.contains(7))
          assert(result.detail.contains("exit 7"))
        }
    }

    test("launch failure is classified without environment disclosure") {
      withWorkspace { workspace =>
        val secret = "launch-secret-value"
        val result = LinkerBenchmarkProcess.run(
          Seq((workspace / "missing-command").toString),
          workspace,
          Map("LINKER_BENCHMARK_SECRET" -> secret),
          1.second,
          workspace / "startup",
          workspace / "stdout.log",
          workspace / "stderr.log"
        )
        assert(result.outcome == Outcome.Failed)
        assert(result.exitCode.isEmpty)
        assert(!result.detail.contains(secret))
      }
    }

    test("environment additions reach child but are not disclosed") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val secret = "benchmark-secret-value"
          val result = runShell(
            workspace,
            "test \"$LINKER_BENCHMARK_SECRET\" = benchmark-secret-value",
            environment = Map(
              "LINKER_BENCHMARK_SECRET" -> secret
            )
          )
          assert(result.outcome == Outcome.Succeeded)
          assert(!result.detail.contains(secret))
          assert(!os.read(workspace / "stdout.log").contains(secret))
          assert(!os.read(workspace / "stderr.log").contains(secret))
        }
    }

    test("environment removals keep inherited values out of the child") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result = LinkerBenchmarkProcess.run(
            Seq(shell.toString, "-c", "test -z \"${USER+x}\""),
            workspace,
            Map.empty,
            5.seconds,
            workspace / "startup",
            workspace / "stdout.log",
            workspace / "stderr.log",
            environmentRemovals = Set("USER")
          )
          assert(result.outcome == Outcome.Succeeded)
        }
    }

    test("smoke child environment opts out of Mill path relativization") {
      val inherited = Map(
        "JAVA_OPTS"                    -> "inherited-java-opts",
        "JDK_JAVA_OPTIONS"             -> "inherited-jdk-options",
        "JAVA_TOOL_OPTIONS"            -> "inherited-tool-options",
        "_JAVA_OPTIONS"                -> "inherited-underscore-options",
        "OS_LIB_PATH_RELATIVIZER_BASE" -> "inherited-relativizer-base",
        "MILL_WORKSPACE_ROOT"          -> "inherited-workspace-root",
        "UNCHANGED"                    -> "kept"
      )
      val composed = composeEnvironment(
        inherited,
        Map(
          "JAVA_OPTS"                    -> "-Xmx1g",
          "OS_LIB_PATH_RELATIVIZER_BASE" -> ""
        ),
        LinkerBenchmark.smokeEnvironmentRemovals
      )

      assert(composed.get("OS_LIB_PATH_RELATIVIZER_BASE").contains(""))
      assert(!composed.contains("MILL_WORKSPACE_ROOT"))
      assert(composed.get("JAVA_OPTS").contains("-Xmx1g"))
      assert(!composed.contains("JDK_JAVA_OPTIONS"))
      assert(!composed.contains("JAVA_TOOL_OPTIONS"))
      assert(!composed.contains("_JAVA_OPTIONS"))
      assert(composed.get("UNCHANGED").contains("kept"))
    }

    test("timeout kills root and descendant and permits replacement") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          var rootPid: Option[Long]  = None
          var childPid: Option[Long] = None
          try {
            val result = runShell(
              workspace,
              "echo $$ > root.pid; sleep 30 & echo $! > child.pid; echo ready > child.ready; wait",
              timeout = 700.millis
            )
            awaitExists(workspace / "child.ready")
            rootPid = Some(os.read(workspace / "root.pid").trim.toLong)
            childPid = Some(os.read(workspace / "child.pid").trim.toLong)
            assert(result.outcome == Outcome.TimedOut)
            rootPid.foreach(awaitDead)
            childPid.foreach(awaitDead)

            val replacement = runShell(workspace, ": > startup; echo replacement")
            assert(replacement.outcome == Outcome.Succeeded)
            assert(os.read(workspace / "stdout.log").trim == "replacement")
          } finally {
            childPid.foreach(forceAndAwaitDead)
            rootPid.foreach(forceAndAwaitDead)
          }
        }
    }

    test("successful root exit retires an observed background child") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          var childPid: Option[Long] = None
          try {
            val result = runShell(
              workspace,
              "sleep 30 & echo $! > child.pid; echo ready > child.ready; sleep 0.4; exit 0"
            )
            awaitExists(workspace / "child.ready")
            childPid = Some(os.read(workspace / "child.pid").trim.toLong)
            assert(result.outcome == Outcome.Succeeded)
            childPid.foreach(awaitDead)
          } finally childPid.foreach(forceAndAwaitDead)
        }
    }

    test("cleanup discovers and retires a descendant forked during termination") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          var childPid: Option[Long]      = None
          var grandchildPid: Option[Long] = None
          try {
            val script =
              """/bin/sh -c "trap 'sleep 30 & echo \$! > grandchild.pid; echo ready > grandchild.ready; sleep 0.4; exit 0' TERM; echo \$\$ > child.pid; echo ready > child.ready; while :; do sleep 1; done" & wait"""
            val result = runShell(workspace, script, timeout = 700.millis)
            awaitExists(workspace / "child.ready")
            awaitExists(workspace / "grandchild.ready")
            childPid = Some(os.read(workspace / "child.pid").trim.toLong)
            grandchildPid = Some(os.read(workspace / "grandchild.pid").trim.toLong)
            assert(result.outcome == Outcome.TimedOut)
            childPid.foreach(awaitDead)
            grandchildPid.foreach(awaitDead)
          } finally {
            grandchildPid.foreach(forceAndAwaitDead)
            childPid.foreach(forceAndAwaitDead)
          }
        }
    }

    test("RSS sampling does not grossly extend a short timeout") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result = runShell(workspace, "sleep 30", timeout = 80.millis)
          assert(result.outcome == Outcome.TimedOut)
          assert(result.wallMillis < 1000L)
        }
    }

    test("cleanup failure only overrides successful process outcome") {
      assert(outcomeAfterCleanup(Outcome.Succeeded, clean = true) == Outcome.Succeeded)
      assert(outcomeAfterCleanup(Outcome.Succeeded, clean = false) == Outcome.Failed)
      assert(outcomeAfterCleanup(Outcome.Failed, clean = false) == Outcome.Failed)
      assert(outcomeAfterCleanup(Outcome.TimedOut, clean = false) == Outcome.TimedOut)
      assert(outcomeAfterCleanup(Outcome.Cancelled, clean = false) == Outcome.Cancelled)
    }

    test("interrupt cancels and cleans up the process tree") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val result                 = new AtomicReference[ProcessResult]()
          val failure                = new AtomicReference[Throwable]()
          val interruptRestored      = new AtomicBoolean(false)
          var rootPid: Option[Long]  = None
          var childPid: Option[Long] = None
          val runner                 = new Thread(
            () =>
              try
                result.set(runShell(workspace, "echo $$ > root.pid; sleep 30 & echo $! > child.pid; wait", 20.seconds))
              catch case error: Throwable => failure.set(error)
              finally interruptRestored.set(Thread.currentThread().isInterrupted),
            "linker-process-cancellation-test"
          )
          try {
            runner.start()
            awaitExists(workspace / "child.pid")
            rootPid = Some(os.read(workspace / "root.pid").trim.toLong)
            childPid = Some(os.read(workspace / "child.pid").trim.toLong)
            runner.interrupt()
            runner.join(7000L)
            assert(!runner.isAlive)
            assert(failure.get() == null)
            assert(result.get() != null)
            assert(result.get().outcome == Outcome.Cancelled)
            assert(interruptRestored.get())
            rootPid.foreach(awaitDead)
            childPid.foreach(awaitDead)
          } finally {
            childPid.foreach(forceAndAwaitDead)
            rootPid.foreach(forceAndAwaitDead)
            runner.interrupt()
            runner.join(7000L)
            assert(!runner.isAlive)
          }
        }
    }

    test("aggregate peak deduplicates pids per instant") {
      val samples = Seq(
        Seq(1L -> 100L, 1L -> 100L, 2L -> 200L),
        Seq(1L -> 150L, 2L -> 250L, 2L -> 250L)
      )
      assert(peakAggregateRssKiB(samples) == 400L)
      assert(peakAggregateRssKiB(Seq.empty) == 0L)
    }

    test("aggregate sampler measures a live process tree and starts a fresh strategy window") {
      if shellUnavailable then ()
      else
        withWorkspace { workspace =>
          val process = new ProcessBuilder(
            Seq(shell.toString, "-c", "sleep 30 & echo $! > child.pid; wait").asJava
          ).directory(workspace.toIO)
            .redirectOutput(ProcessBuilder.Redirect.to((workspace / "aggregate.out").toIO))
            .redirectError(ProcessBuilder.Redirect.to((workspace / "aggregate.err").toIO))
            .start()
          var sampler: AggregateRssSampler = null
          try {
            awaitExists(workspace / "child.pid")
            val childPid       = os.read(workspace / "child.pid").trim.toLong
            val root           = process.toHandle
            val stream         = root.descendants()
            val descendantPids =
              try stream.iterator().asScala.map(_.pid()).toSet
              finally stream.close()
            assert(childPid != root.pid())
            assert(descendantPids.contains(childPid))

            sampler = AggregateRssSampler.start(root.pid(), () => Set(root), 20.millis)
            val firstWindow = awaitPositiveSample(sampler)
            val reset       = sampler.resetStrategyWindow()
            assert(reset.wholeRunPeakKiB >= firstWindow.wholeRunPeakKiB)
            assert(reset.strategyWindowPeakKiB >= firstWindow.strategyWindowPeakKiB)

            val secondWindow = awaitPositiveSample(sampler)
            assert(secondWindow.wholeRunPeakKiB >= firstWindow.wholeRunPeakKiB)
            assert(secondWindow.strategyWindowPeakKiB > 0L)
            assert(process.isAlive)
            assert(processAlive(childPid))
          } finally {
            if sampler != null then sampler.close()
            destroyTree(process)
          }
        }
    }

    test("aggregate sampler close is bounded and restores caller interruption") {
      val failure  = new AtomicReference[Throwable]()
      val restored = new AtomicBoolean(false)
      val runner   = new Thread(
        () =>
          try {
            val sampler = AggregateRssSampler.start(ProcessHandle.current().pid(), () => Set.empty, 20.millis)
            Thread.currentThread().interrupt()
            sampler.close()
            restored.set(Thread.currentThread().isInterrupted)
          } catch case error: Throwable => failure.set(error),
        "aggregate-sampler-close-test"
      )
      runner.start()
      runner.join(3000L)
      try {
        assert(!runner.isAlive)
        assert(failure.get() == null)
        assert(restored.get())
      } finally {
        runner.interrupt()
        runner.join(3000L)
        assert(!runner.isAlive)
      }
    }
  }
}
