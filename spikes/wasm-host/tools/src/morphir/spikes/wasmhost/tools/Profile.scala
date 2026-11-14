package morphir.spikes.wasmhost.tools

import java.nio.file.Path
import java.util.concurrent.TimeUnit
import scala.jdk.CollectionConverters.*

/** Samples only the launched host and its descendants, never Mill, a compiler or this profiler. */
object Profile:
  def parseRss(text: String, requested: Set[Long]): Map[Long, Long] =
    text.linesIterator.filter(_.trim.nonEmpty).map { line =>
      val parts = line.trim.split("\\s+")
      require(parts.length == 2, "invalid ps sample")
      val pid = parts(0).toLong
      val kib = parts(1).toLong
      require(kib >= 0, "negative RSS")
      pid -> Math.multiplyExact(kib, 1024L)
    }.filter((pid, _) => requested(pid)).toVector.groupMapReduce(_._1)(_._2)(math.max)

  def aggregate(requested: Set[Long], measured: Map[Long, Long]): ujson.Value =
    if requested.isEmpty || !requested.subsetOf(measured.keySet) then ujson.Null
    else ujson.Num(requested.toVector.map(measured).sum.toDouble)

  private def sample(root: ProcessHandle, start: Long): ujson.Obj =
    val children = root.descendants()
    val handles  = try
      (root +: children.iterator().asScala.toVector).filter(_.isAlive).groupBy(_.pid()).values.map(_.head).toVector
    finally children.close()
    val pids         = handles.map(_.pid()).toSet
    val parent       = Set(root.pid()).intersect(pids)
    val worker       = pids -- parent
    val begin        = System.nanoTime()
    val measurements = if pids.isEmpty then Map.empty[Long, Long]
    else
      val command = new ProcessBuilder("ps", "-o", "pid=,rss=", "-p", pids.toVector.sorted.mkString(",")).start()
      try
        if command.waitFor(250, TimeUnit.MILLISECONDS) && command.exitValue() == 0 then
          parseRss(String(command.getInputStream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8), pids)
        else Map.empty[Long, Long]
      finally
        if command.isAlive then { val _ = command.destroyForcibly() }
        command.getInputStream.close()
        command.getErrorStream.close()
        command.getOutputStream.close()
    ujson.Obj(
      "elapsedNanos"    -> ujson.Num((begin - start).toDouble),
      "sampleCostNanos" -> ujson.Num((System.nanoTime() - begin).toDouble),
      "parentBytes"     -> aggregate(parent, measurements),
      "workerBytes"     -> (if worker.isEmpty then ujson.Num(0) else aggregate(worker, measurements)),
      "aggregateBytes"  -> aggregate(pids, measurements),
      "processes"       -> ujson.Arr.from(handles.map(handle =>
        ujson.Obj(
          "pid"      -> handle.pid(),
          "role"     -> (if handle.pid() == root.pid() then "parent" else "worker"),
          "started"  -> handle.info().startInstant().map[String](_.toString).orElse("unknown"),
          "rssBytes" -> measurements.get(handle.pid()).fold[ujson.Value](ujson.Null)(value => ujson.Num(value.toDouble))
        )
      ))
    )

  private[tools] def run(
      config: ujson.Value,
      settings: ujson.Value,
      output: os.Path,
      name: String,
      arguments: Seq[String],
      sampleMemory: Boolean,
      environment: Map[String, String] = Map.empty
  ): ujson.Obj =
    val stdout  = output / s"$name.stdout.json"
    val stderr  = output / s"$name.stderr.jsonl"
    val command = config("command").arr.map(_.str).toVector ++ arguments
    val builder = new ProcessBuilder(command.asJava).redirectOutput(stdout.toIO).redirectError(stderr.toIO)
    val _       = builder.environment().put("JAVA_HOME", settings("javaHome").str)
    if config("backend").str == "extism-ffm" then
      val library = os.Path(config("nativeLibrary").str)
      val _       = builder.environment().put("MORPHIR_EXTISM_LIBRARY", library.toString)
      val _       = builder.environment().put("MORPHIR_EXTISM_SHA256", Distribution.hash(library))
    environment.foreach { (key, value) =>
      val _ = builder.environment().put(key, value)
    }
    val start   = System.nanoTime()
    val process = builder.start()
    val samples = Vector.newBuilder[ujson.Obj]
    @annotation.tailrec
    def loop(): Unit =
      if process.isAlive && System.nanoTime() - start < TimeUnit.MINUTES.toNanos(3) then
        if sampleMemory then samples += sample(process.toHandle, start)
        val _ = process.waitFor(if sampleMemory then 50L else 100L, TimeUnit.MILLISECONDS)
        loop()
    try
      loop()
      if process.isAlive then
        val descendants = process.descendants()
        try descendants.iterator().asScala.foreach(_.destroyForcibly())
        finally descendants.close()
        process.destroyForcibly()
        require(process.waitFor(5000, TimeUnit.MILLISECONDS), "profile parent survived termination")
      val elapsed  = System.nanoTime() - start
      val captured = samples.result()
      val phases   = os.read.lines(stderr).flatMap { line =>
        try
          val event = ujson.read(line)
          if event.obj.contains("phase") then Some(event) else None
        catch case scala.util.control.NonFatal(_) => None
      }
      val result = ujson.Obj(
        "configuration" -> config("id"),
        "scope"         -> "synthetic-probe-only",
        "name"          -> name,
        "command"       -> ujson.Arr.from(command),
        "exit"          -> process.exitValue(),
        "elapsedNanos"  -> ujson.Num(elapsed.toDouble),
        "startNanos"    -> ujson.Num(start.toDouble),
        "sampleMemory"  -> sampleMemory,
        "intervalMs"    -> 50,
        "samples"       -> ujson.Arr.from(captured),
        "phases"        -> ujson.Arr.from(phases),
        "stdout"        -> stdout.last,
        "stderr"        -> stderr.last
      )
      result("scope") =
        (if arguments.contains("--mep") then "Python legacy MEP compile" else "synthetic byte callbacks")
      val guestIndex = arguments.indexOf("--guest")
      if guestIndex >= 0 && os.isFile(os.Path(arguments(guestIndex + 1))) then
        result("guestSha256") = Distribution.hash(os.Path(arguments(guestIndex + 1)))
      val first = phases.find(_("phase").str == "first-result").map { phase =>
        val instant = phase("nanoTime") match
          case ujson.Str(text) => text.toLong
          case value           => value.num.toLong
        instant - start
      }.filter(value => value >= 0 && value <= elapsed)
      result("startupToFirstResultNanos") = first.fold[ujson.Value](ujson.Null)(value => ujson.Num(value.toDouble))
      os.write(output / s"$name.json", result.render(indent = 2))
      result
    finally
      if process.isAlive then { val _ = process.destroyForcibly() }
      process.getInputStream.close()
      process.getErrorStream.close()
      process.getOutputStream.close()

  def main(args: Array[String]): Unit =
    val settings = ujson.read(os.read(os.Path(Path.of(args(0)).toAbsolutePath)))
    val output   = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val full     = args.contains("full")
    val receipts = settings("configurations").arr.flatMap { config =>
      Seq("synthetic", "python").flatMap { workload =>
        val guest = if workload == "synthetic" then os.Path(settings("fixtures").str) / "echo.wasm"
        else os.Path(settings("guests").str) / "python.wasm"
        val common = Seq("--guest", guest.toString) ++
          (if workload == "python" then Seq("--mep", "--guest-kind", "python") else Seq.empty)
        val prefix = s"${config("id").str}-$workload"
        val trials = (1 to (if full then 5 else 1)).map { trial =>
          run(
            config,
            settings,
            output,
            s"$prefix-trial-$trial",
            Seq("supervise") ++ common ++
              Seq("--requests", "100", "--warmup", "10", "--idle-ms", "250", "--quiet-ms", "2000"),
            true
          )
        }
        val timings = (1 to (if full then 5 else 1)).map { trial =>
          run(
            config,
            settings,
            output,
            s"$prefix-timing-$trial",
            Seq("probe") ++ common ++ Seq("--requests", "100", "--warmup", "10", "--output-limit", "1048576"),
            false
          )
        }
        val extra = if !full then Seq.empty
        else
          Seq(
            run(
              config,
              settings,
              output,
              s"$prefix-retention",
              Seq("supervise") ++ common ++
                Seq("--cycles", "20", "--idle-ms", "0", "--quiet-ms", "2000"),
              true
            ),
            run(
              config,
              settings,
              output,
              s"$prefix-concurrency-2",
              Seq("supervise") ++ common ++
                Seq("--concurrency", "2", "--quiet-ms", "2000"),
              true
            )
          )
        trials ++ timings ++ extra
      }
    }
    os.write(
      output / "profiles.json",
      ujson.Obj(
        "scope"                -> "synthetic byte callback and pinned Python legacy MEP compile",
        "fullSamplingProtocol" -> full,
        "hardware"             -> ujson.Obj(
          "os"                  -> System.getProperty("os.name"),
          "arch"                -> System.getProperty("os.arch"),
          "osVersion"           -> System.getProperty("os.version"),
          "logicalProcessors"   -> Runtime.getRuntime().availableProcessors(),
          "physicalMemoryBytes" ->
            (if System.getProperty("os.name") == "Mac OS X" then
               os.proc("sysctl", "-n", "hw.memsize").call().out.text().trim
             else "unavailable")
        ),
        "rssDefinition" ->
          "Per-process resident bytes. Concurrent RSS sum double-counts shared pages; it is not physical footprint. Missing samples are null.",
        "heapNativeGuestMetrics" ->
          "runtime heap and GC counters at phase markers; total RSS includes third-party native allocations; guest-linear-memory counters and native allocation attribution not collected; NMT cannot account for arbitrary libextism malloc",
        "forcedGc"   -> false,
        "cacheReset" -> false,
        "profiles"   -> ujson.Arr.from(receipts)
      ).render(indent = 2)
    )
