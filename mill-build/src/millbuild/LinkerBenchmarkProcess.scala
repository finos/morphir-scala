package millbuild

import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong, AtomicReference}
import java.util.concurrent.{
  ConcurrentHashMap, Executors, ScheduledExecutorService, ScheduledFuture, ThreadFactory, TimeUnit
}

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.control.NonFatal

object LinkerBenchmarkProcess {
  final case class ProcessResult(
      outcome: LinkerBenchmark.Outcome,
      exitCode: Option[Int],
      wallMillis: Long,
      startupMillis: Option[Long],
      peakRssKiB: Long,
      detail: String
  )

  final case class AggregateRssSnapshot(
      wholeRunPeakKiB: Long,
      strategyWindowPeakKiB: Long,
      detail: String
  )

  private final case class RssProbe(valueKiB: Long, detail: Option[String])
  private final case class ChildSnapshot(
      startupMillis: Option[Long],
      peakRssKiB: Long,
      rssDetail: Option[String],
      observed: Set[ProcessHandle]
  )
  private final case class CleanupResult(clean: Boolean, detail: Option[String])

  private val CleanupPollMillis = 25L
  private val CleanupMillis     = 5000L
  // ps is a process itself; fixed-delay sampling avoids overlapping probes and 25 ms process churn.
  private val ChildSampleMillis = 150L

  private def elapsedMillis(startNanos: Long): Long =
    math.max(0L, TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - startNanos))

  private def saturatingSum(values: IterableOnce[Long]): Long = {
    val iterator                         = values.iterator
    @tailrec def loop(total: Long): Long =
      if !iterator.hasNext then total
      else {
        val value = math.max(0L, iterator.next())
        if total > Long.MaxValue - value then Long.MaxValue else loop(total + value)
      }
    loop(0L)
  }

  private def updateMax(target: AtomicLong, value: Long): Unit = {
    @tailrec def loop(): Unit = {
      val current = target.get()
      if value <= current then ()
      else if !target.compareAndSet(current, value) then loop()
    }
    loop()
  }

  def composeEnvironment(
      inherited: Map[String, String],
      additions: Map[String, String],
      removals: Set[String]
  ): Map[String, String] = (inherited -- removals) ++ additions

  def peakAggregateRssKiB(samples: Seq[Seq[(Long, Long)]]): Long =
    samples.iterator
      .map { sample =>
        val rssByPid = sample.iterator
          .filter { case (pid, rss) => pid >= 0L && rss >= 0L }
          .toSeq
          .groupMapReduce(_._1)(_._2)(math.max)
        saturatingSum(rssByPid.values)
      }
      .maxOption
      .getOrElse(0L)

  private[millbuild] def outcomeAfterCleanup(
      outcome: LinkerBenchmark.Outcome,
      clean: Boolean
  ): LinkerBenchmark.Outcome =
    if outcome == LinkerBenchmark.Outcome.Succeeded && !clean then LinkerBenchmark.Outcome.Failed else outcome

  private def normalizeDetail(value: String, environmentValues: Iterable[String] = Seq.empty): String = {
    val redacted = environmentValues.iterator
      .filter(_.nonEmpty)
      .foldLeft(value)((current, secret) => current.replace(secret, "<redacted>"))
    val normalized = redacted.replaceAll("[\\p{Cntrl}\\s]+", " ").trim
    if normalized.length <= 240 then normalized else normalized.take(237) + "..."
  }

  private def descendants(root: ProcessHandle): Seq[ProcessHandle] =
    try {
      val stream = root.descendants()
      try stream.iterator().asScala.toSeq
      finally stream.close()
    } catch {
      case NonFatal(_) => Seq.empty
    }

  private def liveTree(roots: IterableOnce[ProcessHandle]): Seq[ProcessHandle] =
    roots.iterator
      .flatMap(root => root +: descendants(root))
      .filter(_.isAlive)
      .toSeq
      .groupBy(_.pid())
      .valuesIterator
      .map(_.head)
      .toSeq

  private def sampleRss(handles: IterableOnce[ProcessHandle], waitMillis: Long = 250L): RssProbe = {
    val pids = handles.iterator.filter(_.isAlive).map(_.pid()).toSet.toSeq.sorted
    if pids.isEmpty then RssProbe(0L, None)
    else {
      val command = Seq("ps", "-o", "rss=", "-p", pids.mkString(","))
      try {
        val process = new ProcessBuilder(command.asJava).redirectErrorStream(true).start()
        try {
          val exited = process.waitFor(math.max(1L, waitMillis), TimeUnit.MILLISECONDS)
          if !exited then RssProbe(0L, Some("rss unavailable: ps timed out"))
          else if process.exitValue() != 0 then
            RssProbe(0L, Some(s"rss unavailable: ps exited ${process.exitValue()}"))
          else {
            val bytes  = process.getInputStream.readAllBytes()
            val lines  = String(bytes, StandardCharsets.UTF_8).linesIterator.map(_.trim).filter(_.nonEmpty).toSeq
            val parsed = lines.map(_.toLongOption).collect { case Some(value) if value >= 0L => value }
            if parsed.size != lines.size then RssProbe(0L, Some("rss unavailable: unparseable ps output"))
            else RssProbe(saturatingSum(parsed), None)
          }
        } finally {
          if process.isAlive then process.destroyForcibly()
          process.getInputStream.close()
          process.getErrorStream.close()
          process.getOutputStream.close()
        }
      } catch {
        case _: InterruptedException =>
          Thread.currentThread().interrupt()
          RssProbe(0L, Some("rss unavailable: ps interrupted"))
        case NonFatal(error) =>
          RssProbe(0L, Some(s"rss unavailable: ${error.getClass.getSimpleName}"))
      }
    }
  }

  private def expandOwned(root: ProcessHandle, observed: java.util.Set[ProcessHandle]): Seq[ProcessHandle] = {
    val owners     = root +: observed.asScala.toSeq
    val discovered = owners.flatMap(descendants)
    discovered.foreach(observed.add)
    observed.asScala.toSeq
  }

  private def awaitExecutorTermination(executor: ScheduledExecutorService, timeoutMillis: Long): Unit = {
    val deadline              = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(timeoutMillis)
    var interrupted           = false
    @tailrec def loop(): Unit = {
      val remaining = deadline - System.nanoTime()
      if remaining <= 0L then ()
      else
        try
          if !executor.awaitTermination(remaining, TimeUnit.NANOSECONDS) then ()
        catch {
          case _: InterruptedException =>
            interrupted = true
            loop()
        }
    }
    try loop()
    finally if interrupted then Thread.currentThread().interrupt()
  }

  private def cleanup(
      root: ProcessHandle,
      observed: java.util.Set[ProcessHandle],
      terminateRoot: Boolean
  ): CleanupResult = {
    val start         = System.nanoTime()
    val graceDeadline = start + TimeUnit.MILLISECONDS.toNanos(CleanupMillis - 1000L)
    val forceDeadline = start + TimeUnit.MILLISECONDS.toNanos(CleanupMillis)
    val requested     = ConcurrentHashMap.newKeySet[ProcessHandle]()
    var interrupted   = false

    def targets(): Seq[ProcessHandle] = {
      val owned = expandOwned(root, observed).filter(_.isAlive)
      if terminateRoot && root.isAlive then owned :+ root else owned
    }

    def pause(): Unit =
      try Thread.sleep(CleanupPollMillis)
      catch {
        case _: InterruptedException => interrupted = true
      }

    @tailrec def graceful(): Seq[ProcessHandle] = {
      val live = targets()
      live.filterNot(handle => requested.contains(handle)).foreach { handle =>
        handle.destroy()
        requested.add(handle)
      }
      if live.isEmpty || System.nanoTime() >= graceDeadline then live
      else {
        pause()
        graceful()
      }
    }

    @tailrec def force(): Seq[ProcessHandle] = {
      val live = targets()
      live.foreach(_.destroyForcibly())
      if live.isEmpty || System.nanoTime() >= forceDeadline then live
      else {
        pause()
        force()
      }
    }

    try {
      val afterGrace = graceful()
      val survivors  = if afterGrace.isEmpty then Seq.empty else force()
      if survivors.isEmpty then CleanupResult(clean = true, None)
      else CleanupResult(clean = false, Some(s"cleanup failed: ${survivors.size} owned process(es) survived"))
    } finally if interrupted then Thread.currentThread().interrupt()
  }

  private final class ProcessSampler(
      root: ProcessHandle,
      startNanos: Long,
      startupMarker: os.Path
  ) extends AutoCloseable {
    private val stopped   = new AtomicBoolean(false)
    private val peakRss   = new AtomicLong(0L)
    private val startup   = new AtomicReference(Option.empty[Long])
    private val rssError  = new AtomicReference(Option.empty[String])
    private val observed  = ConcurrentHashMap.newKeySet[ProcessHandle]()
    private val scheduler = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
      override def newThread(runnable: Runnable): Thread = {
        val thread = new Thread(runnable, "linker-benchmark-child-sampler")
        thread.setDaemon(true)
        thread
      }
    })

    private def observeStartup(): Unit =
      if startup.get().isEmpty && os.exists(startupMarker) then
        startup.compareAndSet(None, Some(elapsedMillis(startNanos)))

    private def tick(): Unit =
      if !stopped.get() then {
        try {
          observeStartup()
          val owned = expandOwned(root, observed)
          val probe = sampleRss(root +: owned)
          updateMax(peakRss, probe.valueKiB)
          probe.detail.foreach(value => rssError.compareAndSet(None, Some(value)))
        } catch {
          case NonFatal(error) =>
            rssError.compareAndSet(None, Some(s"rss unavailable: ${error.getClass.getSimpleName}"))
        }
      }

    private val scheduled = scheduler.scheduleWithFixedDelay(
      () => tick(),
      0L,
      ChildSampleMillis,
      TimeUnit.MILLISECONDS
    )

    def snapshot(): ChildSnapshot = {
      observeStartup()
      ChildSnapshot(startup.get(), peakRss.get(), rssError.get(), observed.asScala.toSet)
    }

    def observedHandles: java.util.Set[ProcessHandle] = observed

    override def close(): Unit =
      if stopped.compareAndSet(false, true) then {
        scheduled.cancel(true)
        scheduler.shutdownNow()
        awaitExecutorTermination(scheduler, 1000L)
      }
  }

  def run(
      command: Seq[String],
      workingDirectory: os.Path,
      environment: Map[String, String],
      timeout: FiniteDuration,
      startupMarker: os.Path,
      stdout: os.Path,
      stderr: os.Path,
      environmentRemovals: Set[String] = Set.empty
  ): ProcessResult = {
    val startNanos = System.nanoTime()
    if command.isEmpty then
      ProcessResult(LinkerBenchmark.Outcome.Failed, None, elapsedMillis(startNanos), None, 0L, "empty command")
    else if timeout <= Duration.Zero then
      ProcessResult(
        LinkerBenchmark.Outcome.Failed,
        None,
        elapsedMillis(startNanos),
        None,
        0L,
        "timeout must be positive"
      )
    else {
      var process: Process        = null
      var sampler: ProcessSampler = null
      var interrupted             = false

      def finish(
          outcome: LinkerBenchmark.Outcome,
          exitCode: Option[Int],
          baseDetail: String,
          terminateRoot: Boolean
      ): ProcessResult = {
        if sampler != null then sampler.close()
        val snapshot =
          if sampler == null then ChildSnapshot(None, 0L, None, Set.empty)
          else sampler.snapshot()
        val retired =
          if process == null then CleanupResult(clean = true, None)
          else {
            val observed =
              if sampler == null then ConcurrentHashMap.newKeySet[ProcessHandle]() else sampler.observedHandles
            cleanup(process.toHandle, observed, terminateRoot)
          }
        val finalOutcome = outcomeAfterCleanup(outcome, retired.clean)
        val detail       = normalizeDetail(
          (Seq(baseDetail) ++ snapshot.rssDetail ++ retired.detail).mkString("; "),
          environment.values
        )
        ProcessResult(
          finalOutcome,
          exitCode,
          elapsedMillis(startNanos),
          snapshot.startupMillis,
          snapshot.peakRssKiB,
          detail
        )
      }

      try {
        java.nio.file.Files.deleteIfExists(startupMarker.toNIO)
        val builder = new ProcessBuilder(command.asJava)
          .directory(workingDirectory.toIO)
          .redirectOutput(ProcessBuilder.Redirect.to(stdout.toIO))
          .redirectError(ProcessBuilder.Redirect.to(stderr.toIO))
        val childEnvironment = builder.environment()
        val composed         = composeEnvironment(childEnvironment.asScala.toMap, environment, environmentRemovals)
        childEnvironment.clear()
        childEnvironment.putAll(composed.asJava)
        process = builder.start()
        sampler = new ProcessSampler(process.toHandle, startNanos, startupMarker)
        val remaining = timeout.toNanos - (System.nanoTime() - startNanos)
        val exited    = remaining > 0L && process.waitFor(remaining, TimeUnit.NANOSECONDS)
        if exited then {
          val exitCode = process.exitValue()
          val outcome  = if exitCode == 0 then LinkerBenchmark.Outcome.Succeeded else LinkerBenchmark.Outcome.Failed
          val base     = if exitCode == 0 then "completed" else s"exit $exitCode"
          finish(outcome, Some(exitCode), base, terminateRoot = false)
        } else finish(LinkerBenchmark.Outcome.TimedOut, None, "timed out", terminateRoot = true)
      } catch {
        case _: InterruptedException =>
          interrupted = true
          finish(LinkerBenchmark.Outcome.Cancelled, None, "cancelled", terminateRoot = true)
        case NonFatal(error) =>
          finish(
            LinkerBenchmark.Outcome.Failed,
            None,
            s"launch failed: ${error.getClass.getSimpleName}",
            terminateRoot = process != null && process.isAlive
          )
      } finally {
        if sampler != null then sampler.close()
        if process != null then {
          process.getInputStream.close()
          process.getErrorStream.close()
          process.getOutputStream.close()
        }
        if interrupted then Thread.currentThread().interrupt()
      }
    }
  }

  final class AggregateRssSampler private (
      orchestratorPid: Long,
      activeChildRoots: () => Iterable[ProcessHandle],
      interval: FiniteDuration
  ) extends AutoCloseable {
    private val stopped                             = new AtomicBoolean(false)
    private val lastDetail                          = new AtomicReference(Option.empty[String])
    private val scheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor(new ThreadFactory {
      override def newThread(runnable: Runnable): Thread = {
        val thread = new Thread(runnable, "linker-benchmark-rss")
        thread.setDaemon(true)
        thread
      }
    })
    private var wholeRunPeakKiB       = 0L
    private var strategyWindowPeakKiB = 0L

    private def tick(): Unit = synchronized {
      if !stopped.get() then {
        try {
          val orchestrator = ProcessHandle.of(orchestratorPid).toScala.toSeq
          val roots        = orchestrator ++ activeChildRoots().iterator.toSeq
          val probe        = sampleRss(liveTree(roots))
          wholeRunPeakKiB = math.max(wholeRunPeakKiB, probe.valueKiB)
          strategyWindowPeakKiB = math.max(strategyWindowPeakKiB, probe.valueKiB)
          probe.detail.foreach(value => lastDetail.compareAndSet(None, Some(value)))
        } catch {
          case NonFatal(error) =>
            lastDetail.compareAndSet(None, Some(s"rss unavailable: ${error.getClass.getSimpleName}"))
        }
      }
    }

    private val scheduled: ScheduledFuture[?] = scheduler.scheduleAtFixedRate(
      () => tick(),
      0L,
      math.max(1L, interval.toMillis),
      TimeUnit.MILLISECONDS
    )

    def snapshot(): AggregateRssSnapshot = synchronized {
      AggregateRssSnapshot(wholeRunPeakKiB, strategyWindowPeakKiB, lastDetail.get().getOrElse(""))
    }

    def resetStrategyWindow(): AggregateRssSnapshot = synchronized {
      val previous = snapshot()
      strategyWindowPeakKiB = 0L
      previous
    }

    def stop(): Unit =
      if stopped.compareAndSet(false, true) then {
        scheduled.cancel(true)
        scheduler.shutdownNow()
        awaitExecutorTermination(scheduler, 1000L)
      }

    override def close(): Unit = stop()
  }

  object AggregateRssSampler {
    def start(
        orchestratorPid: Long,
        activeChildRoots: () => Iterable[ProcessHandle],
        interval: FiniteDuration = 100.millis
    ): AggregateRssSampler = {
      require(interval > Duration.Zero, "RSS sample interval must be positive")
      new AggregateRssSampler(orchestratorPid, activeChildRoots, interval)
    }
  }
}
