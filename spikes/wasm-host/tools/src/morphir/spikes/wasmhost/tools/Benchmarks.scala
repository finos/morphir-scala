package morphir.spikes.wasmhost.tools

import java.nio.file.Path

/** Serial, checked comparisons of the delivered host stacks; no RSS sampler or observation holds. */
object Benchmarks:
  private[tools] def number(value: ujson.Value): Double = value match
    case ujson.Str(text) => text.toDouble
    case _               => value.num

  private[tools] def percentile(values: Vector[Double], fraction: Double): Double =
    require(values.nonEmpty && fraction > 0 && fraction <= 1, "invalid percentile input")
    values.sorted.apply(math.max(0, math.ceil(values.size * fraction).toInt - 1))

  private[tools] def computeChecksum: Int =
    @annotation.tailrec
    def loop(remaining: Int, state: Int): Int =
      if remaining == 0 then state else loop(remaining - 1, state * 1664525 + 1013904223)
    loop(100000, 0x12345678)

  private[tools] def memoryChecksum: Long = 8L * (0 until 65536).map(_ & 255).sum

  private def verifyDelivery(config: ujson.Value): Unit =
    val root = os.Path(config("command")(0).str) / os.up
    config("files").arr.foreach { file =>
      val path = root / os.RelPath(file("path").str)
      require(
        os.size(path).toDouble == number(file("bytes")) && Distribution.hash(path) == file("sha256").str,
        s"delivery identity changed: $path"
      )
    }

  def main(args: Array[String]): Unit =
    val input    = os.Path(Path.of(args(0)).toRealPath())
    val settings = ujson.read(os.read(input))
    val fixtures = os.Path(Path.of(args(1)).toRealPath())
    val output   = os.Path(Path.of(args(2)).toAbsolutePath)
    os.makeDir.all(output)
    require(computeChecksum == -2115620968 && memoryChecksum == 66846720L, "benchmark golden checksum differs")
    val configs = settings("configurations").arr.toVector
    configs.foreach(verifyDelivery)
    val workloads = Vector("boundary", "compute", "memory", "python")
    val runs      = Vector.newBuilder[ujson.Value]
    (1 to 5).foreach { trial =>
      val ordered = configs.drop(trial - 1) ++ configs.take(trial - 1)
      workloads.zipWithIndex.foreach { (workload, index) =>
        val rotated = ordered.drop(index) ++ ordered.take(index)
        rotated.foreach { config =>
          val guest = workload match
            case "python"   => os.Path(settings("guests").str) / "python.wasm"
            case "boundary" => fixtures / "echo.wasm"
            case name       => fixtures / s"benchmark-$name.wasm"
          val command =
            Seq("probe", "--guest", guest.toString, "--requests", "100", "--warmup", "20", "--idle-ms", "0") ++
              (if workload == "python" then Seq("--mep", "--guest-kind", "python", "--output-limit", "1048576")
               else Seq.empty)
          val run = Profile.run(config, settings, output, s"${config("id").str}-$workload-speed-$trial", command, false)
          run("trial") = trial
          run("workload") = workload
          run("scope") = "execution-speed comparison of host adapters and engines"
          if run("exit").num == 0 then
            val result = ujson.read(os.read(output / run("stdout").str))
            require(result("requests").num == 100 && result("warmup").num == 20, "incorrect timing protocol")
            val times = result("callNanosIncludingWarmup").arr.toVector.map(number)
            require(times.size == 120 && times.forall(_ > 0), "missing invocation samples")
            run("warmupNanos") = ujson.Arr.from(times.take(20))
            run("measuredNanos") = ujson.Arr.from(times.drop(20))
            run("metering") = result("instructionMetering")
            run("correctness") = "checked checksum/byte round-trip or real Python IR semantics"
          runs += run
          os.write.over(output / "partial-runs.json", ujson.Arr.from(runs.result()).render(indent = 2))
        }
      }
    }
    val observations = runs.result()
    configs.foreach(verifyDelivery)
    val summaries =
      for
        workload <- workloads
        config   <- configs
      yield
        val matching     = observations.filter(r => r("configuration") == config("id") && r("workload").str == workload)
        val passed       = matching.filter(_("exit").num == 0)
        val samples      = passed.flatMap(_("measuredNanos").arr.toVector.map(number))
        val trialMedians = passed.map(r => percentile(r("measuredNanos").arr.toVector.map(number), 0.5))
        val startup      =
          passed.filter(_("startupToFirstResultNanos") != ujson.Null).map(r => number(r("startupToFirstResultNanos")))
        def observed(value: => Double): ujson.Value = if samples.isEmpty then ujson.Null else ujson.Num(value)
        ujson.Obj(
          "configuration" -> config("id"),
          "workload"      -> workload,
          "status" -> (if passed.size == 5 then "pass" else if passed.isEmpty then "unavailable" else "incomplete"),
          "successfulTrials"          -> passed.size,
          "failedTrials"              -> (matching.size - passed.size),
          "measuredCalls"             -> samples.size,
          "p50Nanos"                  -> observed(percentile(samples, 0.5)),
          "p95Nanos"                  -> observed(percentile(samples, 0.95)),
          "p99Nanos"                  -> observed(percentile(samples, 0.99)),
          "medianOfTrialMediansNanos" -> observed(percentile(trialMedians, 0.5)),
          "minimumTrialMedianNanos"   -> observed(trialMedians.min),
          "maximumTrialMedianNanos"   -> observed(trialMedians.max),
          "sequentialCallsPerSecond"  -> observed(samples.size.toDouble * 1e9 / samples.sum),
          "earlyCallMedianNanos"      ->
            observed(percentile(passed.flatMap(_("measuredNanos").arr.take(25).map(number)), 0.5)),
          "lateCallMedianNanos" ->
            observed(percentile(passed.flatMap(_("measuredNanos").arr.takeRight(25).map(number)), 0.5)),
          "startupMedianNanos" -> (if startup.isEmpty then ujson.Null else ujson.Num(percentile(startup, 0.5))),
          "metering"           -> ujson.Arr.from(passed.map(_("metering")).distinct)
        )
    val receipt = ujson.Obj(
      "distributionInputSha256" -> Distribution.hash(input),
      "protocol" -> "5 fresh processes; 20 recorded warm-up + 100 measured calls; serial rotated configuration order",
      "timingScope" ->
        "System.nanoTime around synchronous host invocation, including adapters, callbacks/copies and runtime safety mechanisms",
      "throughputDefinition" ->
        "successful measured calls / sum of their measured invocation seconds; sequential, excludes startup and warm-up",
      "instrumentation" ->
        "No RSS sampling or artificial holds. Per-call clock overhead and phase/heap/GC events remain. Fully uninstrumented timing unavailable.",
      "warmup"      -> "Fixed count, not proof of JIT stabilization; compare early/late measured-call trends",
      "cachePolicy" ->
        "No OS/runtime cache flush; optional Extism compilation-cache configuration not separately instrumented",
      "hardware" -> ujson.Obj(
        "os"                -> System.getProperty("os.name"),
        "arch"              -> System.getProperty("os.arch"),
        "logicalProcessors" -> Runtime.getRuntime().availableProcessors()
      ),
      "workUnits" -> ujson.Obj(
        "boundary" -> "fixed Unicode/binary echo with byte callbacks",
        "compute"  -> "100000 dependent wrapping i32 LCG steps plus checked byte echo",
        "memory"   -> "8 x 65536 byte stores and reads, checksum consumed, plus checked byte echo",
        "python"   -> "same pinned frozen dataclass to Morphir IR v3 compile request"
      ),
      "goldenChecksums" -> ujson.Obj("compute" -> computeChecksum, "memory" -> memoryChecksum),
      "moonbitCompiler" -> "unavailable until compiler-hosting gate passes; not substituted by native moonc or Node",
      "runs"            -> ujson.Arr.from(observations),
      "summaries"       -> ujson.Arr.from(summaries)
    )
    os.write(output / "benchmarks.json", receipt.render(indent = 2))
    val csv = "configuration,workload,trial,call,latency_nanos\n" +
      observations.filter(_("exit").num == 0).flatMap { run =>
        run("measuredNanos").arr.zipWithIndex.map((value, index) =>
          s"${run("configuration").str},${run("workload").str},${run("trial").num.toInt},${index + 1},${number(value).toLong}"
        )
      }.mkString("\n")
    os.write(output / "calls.csv", csv)
    def ms(value: ujson.Value): String = if value == ujson.Null then "unavailable" else f"${number(value) / 1e6}%.4f"
    val charts                         = workloads.map { workload =>
      val values  = summaries.filter(_("workload").str == workload)
      val maximum =
        values.filter(_("medianOfTrialMediansNanos") != ujson.Null).map(r => number(r("medianOfTrialMediansNanos"))).max
      "<section><h2>" + workload +
        "</h2><p>Median of five trial medians, milliseconds per checked invocation. Lower is faster.</p>" +
        values.map { value =>
          val median = value("medianOfTrialMediansNanos")
          val bar    = if median == ujson.Null then ""
          else
            s"<div class='bar' style='width:${number(median) / maximum * 100}%'></div>"
          s"<div class='row'><span>${value("configuration").str}</span><div class='track'>$bar</div><span>${ms(median)}${
              if median == ujson.Null then "" else " ms"
            }</span></div>"
        }.mkString + "</section>"
    }.mkString
    val table = summaries.map { value =>
      val rate = if value("sequentialCallsPerSecond") == ujson.Null then "unavailable"
      else f"${number(value("sequentialCallsPerSecond"))}%.1f"
      s"<tr><td>${value("configuration").str} · ${value("workload").str}</td><td>${value("status").str}</td><td>${ms(value("p50Nanos"))} / ${ms(value("p95Nanos"))} / ${ms(value("p99Nanos"))}</td><td>${ms(value("minimumTrialMedianNanos"))} to ${ms(value("maximumTrialMedianNanos"))}</td><td>$rate</td><td>${ms(value("earlyCallMedianNanos"))} / ${ms(value("lateCallMedianNanos"))}</td><td>${ms(value("startupMedianNanos"))}</td></tr>"
    }.mkString
    val throughputCharts = workloads.map { workload =>
      val values  = summaries.filter(_("workload").str == workload)
      val maximum =
        values.filter(_("sequentialCallsPerSecond") != ujson.Null).map(r => number(r("sequentialCallsPerSecond"))).max
      s"<section><h2>$workload throughput</h2><p>Sequential checked invocations per second, excluding startup and warm-up. Higher is faster.</p>" +
        values.map { value =>
          val rate = value("sequentialCallsPerSecond")
          val bar  =
            if rate == ujson.Null then "" else s"<div class='bar' style='width:${number(rate) / maximum * 100}%'></div>"
          val label = if rate == ujson.Null then "unavailable" else f"${number(rate)}%.1f calls/s"
          s"<div class='row'><span>${value("configuration").str}</span><div class='track'>$bar</div><span>$label</span></div>"
        }.mkString + "</section>"
    }.mkString
    val head =
      """<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Wasm host execution speed</title><style>
body{font-family:var(--font-sans,system-ui);color:var(--foreground,#18212f);margin:0}p{line-height:1.5}.row{display:grid;grid-template-columns:110px 1fr 130px;gap:8px;align-items:center;margin:12px 0;font-size:13px}.track{background:var(--secondary,#e2e8f0);height:14px}.bar{background:var(--chart-1,#3b82f6);height:14px;min-width:1px}section{margin:24px 0}table{border-collapse:collapse;width:100%;font-size:12px}td,th{padding:8px;text-align:left;border-bottom:1px solid var(--border,#cbd5e1)}a{color:var(--primary,#2563eb)}@media(max-width:450px){.row{grid-template-columns:95px 1fr 90px;font-size:11px}}</style>"""
    val introduction =
      "<p>Checked execution-speed comparisons across JVM and Native Image. Each workload uses identical guest bytes and inputs. Measurements include the current host adapters and runtime safety mechanisms. No single score combines these workloads.</p>"
    os.write(
      output / "summary.html",
      head + introduction + charts +
        "<p>Five fresh processes per cell, 20 warm-up and 100 measured calls per process. Fixed warm-up does not establish JIT stabilization. No RSS sampler or observation holds; clocks and phase/heap/GC logging remain. GraalWasm's real Python guest is unavailable under the current memory-admission policy. MoonBit compiler hosting is a separate blocked experiment.</p></html>"
    )
    os.write(
      output / "index.html",
      head + introduction + charts + throughputCharts +
        "<h2>Latency and throughput details</h2><p>Nearest-rank percentiles pool 500 successful measured calls per complete cell. Trial ranges and early/late medians show variability and tiering; they are not confidence intervals. Throughput divides calls by the sum of measured invocation durations, excluding startup and warm-up. Startup includes launch, loading and one checked result. Independent launches do not flush caches. FFM retains its 5-billion-fuel budget; Endive and GraalWasm do not claim equivalent instruction metering.</p>" +
        "<table><tr><th>Configuration / workload</th><th>Status</th><th>p50 / p95 / p99 ms</th><th>Trial median range ms</th><th>Sequential calls/s</th><th>Early / late ms</th><th>Startup ms</th></tr>" +
        table +
        "</table><p><a href='benchmarks.json'>Protocol, identities, raw runs and summaries</a> · <a href='calls.csv'>Measured calls CSV</a></p><p>Compiler-hosting timings remain unavailable until the released Wasm compiler executes inside a qualified compiler adapter. Existing footprint evidence uses a different warm-up protocol and is retained separately.</p></html>"
    )
