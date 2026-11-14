package morphir.spikes.wasmhost.tools

import java.nio.file.Path

/** Offline charts use only retained observations. An absent value never becomes a zero-height bar. */
object Report:
  private def number(value: ujson.Value): Double = value match
    case ujson.Str(text) => text.toDouble
    case _               => value.num
  private def escaped(text: String): String = text.replace("&", "&amp;").replace("<", "&lt;").replace("\"", "&quot;")
  private def mib(bytes: Double): String    = f"${bytes / 1048576}%.2f"
  private def size(bytes: Double): String   = if bytes < 1024 then s"${bytes.toLong} B" else mib(bytes) + " MiB"

  def main(args: Array[String]): Unit =
    val distribution = os.Path(Path.of(args(0)).toRealPath())
    val profiles     = os.Path(Path.of(args(1)).toRealPath())
    val output       = os.Path(Path.of(args(2)).toAbsolutePath)
    os.makeDir.all(output)
    val settings    = ujson.read(os.read(distribution / "configurations.json"))
    val raw         = ujson.read(os.read(profiles / "profiles.json"))
    val supplements =
      Seq(11 -> "speed", 12 -> "moonbit-compiler", 13 -> "polyglot-compiler").filter((index, _) => args.length > index)
    supplements.foreach { (index, label) =>
      val source = os.Path(Path.of(args(index)).toRealPath())
      os.makeDir.all(output / label)
      if label == "polyglot-compiler" then
        Distribution.files(source).foreach(file =>
          os.copy.over(file, output / label / file.relativeTo(source), createFolders = true)
        )
      else os.list(source).filter(os.isFile(_)).foreach(file => os.copy.over(file, output / label / file.last))
    }
    val supplementLinks =
      supplements.map((_, label) => s"<a href='$label/index.html'>$label experiment</a>").mkString(" · ")
    raw("reportClarifications") = ujson.Obj(
      "heapCommittedBytes" ->
        "Historical field name: Runtime.totalMemory(), runtime-reported capacity, not OS committed or resident bytes",
      "heapUsedBytes"           -> "Runtime.totalMemory() minus Runtime.freeMemory()",
      "heapMaxBytes"            -> "Runtime.maxMemory()",
      "guestLinearMemoryBytes"  -> "Available for Endive phase markers only; unavailable values remain null",
      "profileInputSha256"      -> Distribution.hash(profiles / "profiles.json"),
      "distributionInputSha256" -> Distribution.hash(distribution / "configurations.json")
    )
    os.list(profiles).filter(os.isFile(_)).foreach(file => os.copy.over(file, output / file.last))
    Seq(
      3  -> "qualification",
      4  -> "mck",
      5  -> "checks",
      7  -> "abi",
      8  -> "guests",
      9  -> "ffi-generation",
      10 -> "native-runtime"
    ).foreach {
      (index, label) =>
        if args.length > index then
          val source = os.Path(Path.of(args(index)).toRealPath())
          os.makeDir.all(output / label)
          os.list(source).filter(file =>
            os.isFile(file) &&
              (Set("json", "jsonl", "txt", "stdout", "stderr", "h")(file.ext) || file.last == "LICENSE")
          )
            .foreach(file => os.copy.over(file, output / label / file.last))
    }
    if args.length > 6 then
      val source = os.Path(Path.of(args(6)).toRealPath())
      os.makeDir.all(output / "moonbit")
      Seq("receipt.json", "build.log", "toolchain.txt", "workspace-original.txt", "workspace-build.txt").foreach {
        name =>
          if os.isFile(source / name) then os.copy.over(source / name, output / "moonbit" / name)
      }
    os.list(distribution).filter(file => os.isFile(file) && file.last.endsWith("-build.log"))
      .foreach(file => os.copy.over(file, output / file.last))
    val configs     = settings("configurations").arr.toVector
    val comparisons =
      for
        config   <- configs
        workload <- Seq("synthetic", "python")
      yield (config, workload)
    val runs = raw("profiles").arr.toVector
    runs.foreach { run =>
      run("samples").arr.foreach { sample =>
        if sample("aggregateBytes") != ujson.Null && sample("parentBytes") != ujson.Null &&
          sample("workerBytes") != ujson.Null
        then
          require(
            number(sample("aggregateBytes")) == number(sample("parentBytes")) + number(sample("workerBytes")),
            "inconsistent RSS aggregate"
          )
      }
    }
    def readArray(path: os.Path): Vector[ujson.Value] =
      if os.isFile(path) then ujson.read(os.read(path)).arr.toVector else Vector.empty
    val functional     = readArray(output / "qualification" / "qualification.json")
    val mck            = readArray(output / "mck" / "mck.json")
    val checks         = readArray(output / "checks" / "checks.json")
    val functionalRows = configs.map { config =>
      val id                                                         = config("id").str
      def status(values: Vector[ujson.Value], guest: String): String = values.find(v =>
        v.obj.get("configuration").contains(ujson.Str(id)) && v.obj.get("guest").contains(ujson.Str(guest))
      )
        .fold("not run")(_("status").str)
      val safety = checks.find(_("id").str == id).fold("not run")(_("status").str)
      s"<tr><td>$id</td><td>${status(functional, "sdk")}</td><td>${status(functional, "python")}</td><td>$safety</td><td>${status(mck, "sdk")} / ${status(mck, "python")}</td></tr>"
    }.mkString
    val buildRows = configs.filter(_("target").str == "native").map { config =>
      val id = config("id").str
      s"<tr><td>$id</td><td>${escaped(config.obj.get("nativeImageBuildTime").fold("unavailable")(_.str))}</td><td>${escaped(config.obj.get("nativeImageBuildPeakRss").fold("unavailable")(_.str))}</td><td><a href='$id-build.log'>Build log</a></td></tr>"
    }.mkString
    def peak(run: ujson.Value, key: String): String =
      val values = run("samples").arr.filter(_(key) != ujson.Null).map(sample => number(sample(key)))
      if values.isEmpty then "unavailable" else mib(values.max) + " MiB"
    def phaseSamples(run: ujson.Value, startName: String, endName: String, key: String): Vector[Double] =
      val phases  = run("phases").arr
      val windows = phases.filter(_("phase").str == startName).flatMap { start =>
        phases.find(end =>
          end("phase").str == endName && end("pid") == start("pid") &&
            number(end("nanoTime")) >= number(start("nanoTime"))
        ).map { end =>
          (number(start("nanoTime")) - number(run("startNanos")), number(end("nanoTime")) - number(run("startNanos")))
        }
      }
      run("samples").arr.filter(sample =>
        sample(key) != ujson.Null && windows.exists((start, end) =>
          number(sample("elapsedNanos")) >= start && number(sample("elapsedNanos")) <= end
        )
      ).map(sample => number(sample(key))).toVector
    val phaseRows = comparisons.map { (config, workload) =>
      val id     = config("id").str
      val trials = runs.filter(r =>
        r("configuration").str == id && r("name").str.contains(s"-$workload-trial-") && r("exit").num == 0
      )
      def summary(start: String, end: String, key: String, median: Boolean): String =
        val values = trials.flatMap(phaseSamples(_, start, end, key)).sorted
        if values.isEmpty then "unsampled / unavailable"
        else mib(if median then values(values.size / 2) else values.last) + " MiB"
      s"<tr><td>$id · $workload</td><td>${summary("loaded-idle", "warmup", "workerBytes", true)}</td><td>${summary("calls", "calls-complete", "aggregateBytes", false)}</td><td>${summary("workers-reaped", "parent-cleanup-idle", "parentBytes", true)}</td></tr>"
    }.mkString
    val concurrentRows = comparisons.map { (config, workload) =>
      val id  = config("id").str
      val run = runs.find(r =>
        r("configuration").str == id && r("name").str.endsWith(s"$workload-concurrency-2") && r("exit").num == 0
      )
      s"<tr><td>$id · $workload</td><td>${run.fold("unavailable")(peak(_, "parentBytes"))}</td><td>${run.fold("unavailable")(peak(_, "workerBytes"))}</td><td>${run.fold("unavailable")(peak(_, "aggregateBytes"))}</td></tr>"
    }.mkString
    val overhead     = runs.flatMap(_("samples").arr.map(s => number(s("sampleCostNanos")) / 1e6)).sorted
    val overheadText = if overhead.isEmpty then "unavailable"
    else f"median ${overhead(overhead.size / 2)}%.2f ms, maximum ${overhead.last}%.2f ms"
    val csv = "configuration,run,elapsed_nanos,parent_rss_bytes,worker_rss_bytes,aggregate_rss_bytes\n" +
      runs.flatMap { run =>
        run("samples").arr.map { sample =>
          Seq(
            run("configuration").str,
            run("name").str,
            number(sample("elapsedNanos")).toLong.toString,
            "parentBytes",
            "workerBytes",
            "aggregateBytes"
          ).zipWithIndex.map { (value, index) =>
            if index < 3 then value
            else if sample(value) == ujson.Null then "" else number(sample(value)).toLong.toString
          }.mkString(",")
        }
      }.mkString("\n")
    os.write.over(output / "samples.csv", csv)
    os.write.over(output / "profiles.json", raw.render(indent = 2))
    os.write.over(output / "sizes.json", settings.render(indent = 2))
    val maxSize  = configs.map(c => number(c("installedBytes"))).max
    val sizeBars = configs.map { config =>
      val id     = config("id").str
      val groups = Seq(
        "installedBytes"            -> "Installed",
        "packageBytes"              -> "tar.gz",
        "executableOrLauncherBytes" -> "Executable/launcher"
      )
      s"<div class='sizes'><strong>$id</strong>" + groups.zipWithIndex.map { case ((key, label), index) =>
        val value = number(config(key))
        s"<div class='barrow'><span>$label</span><div class='track'><div class='bar color$index' style='width:${value /
            maxSize * 100}%'></div></div><span>${size(value)}</span></div>"
      }.mkString + "</div>"
    }.mkString

    def points(run: ujson.Value, key: String, maximum: Double): String =
      val duration = number(run("elapsedNanos"))
      run("samples").arr.filter(_(key) != ujson.Null).map { sample =>
        f"${40 + number(sample("elapsedNanos")) / duration * 640}%.2f,${180 - number(sample(key)) / maximum * 150}%.2f"
      }.mkString(" ")

    def timeline(run: ujson.Value): String =
      val samples = run("samples").arr
      val valid   = samples.filter(_("aggregateBytes") != ujson.Null)
      if valid.isEmpty then "<p>Memory unavailable. See the retained command and stderr.</p>"
      else
        val maximum = valid.map(s => number(s("aggregateBytes"))).max * 1.1
        val series  = Seq("parentBytes", "workerBytes", "aggregateBytes").zipWithIndex.map { (key, index) =>
          s"<polyline class='line color$index' points='${points(run, key, maximum)}'/>"
        }.mkString
        val markers = run("phases").arr.filter(p =>
          Set("load-start", "loaded-idle", "calls", "closed", "workers-reaped")(p("phase").str)
        ).map { phase =>
          val x = 40 + (number(phase("nanoTime")) - number(run("startNanos"))) / number(run("elapsedNanos")) * 640
          s"<line class='marker' x1='$x' x2='$x' y1='25' y2='180'><title>${escaped(phase("phase").str)}</title></line>"
        }.mkString
        s"<svg viewBox='0 0 720 210' role='img' aria-label='Parent, worker and aggregate resident memory over process lifetime'><text x='40' y='15'>${mib(maximum)} MiB</text><path class='axis' d='M40 25V180H680'/>$markers$series<text x='40' y='202'>0 seconds</text><text x='590' y='202'>${f"${number(run("elapsedNanos")) /
              1e9}%.2f"} seconds</text></svg>"

    val timelines = comparisons.map { (config, workload) =>
      val id    = config("id").str
      val trial = runs.find(r => r("configuration").str == id && r("name").str.endsWith(s"$workload-trial-1"))
      s"<article><h3>$id · $workload</h3>" + trial.fold("<p>Not run.</p>")(run =>
        if run("exit").num == 0 then timeline(run)
        else "<p>Unavailable: this workload failed. Partial process observations remain in the raw evidence.</p>"
      ) + "</article>"
    }.mkString
    val summaries = comparisons.map { (config, workload) =>
      val id       = config("id").str
      val matching = runs.filter(r => r("configuration").str == id && r("name").str.contains(s"-$workload-"))
      val trials   = matching.filter(_("name").str.contains("-trial-"))
      val peaks    = trials.filter(_("exit").num == 0).flatMap(r =>
        r("samples").arr.filter(_("aggregateBytes") != ujson.Null).map(s => number(s("aggregateBytes")))
      )
      val timings = matching.filter(r =>
        !r("sampleMemory").bool && r("exit").num == 0 && r("startupToFirstResultNanos") != ujson.Null
      )
        .map(r => number(r("startupToFirstResultNanos")) / 1e6).sorted
      val elapsed = if timings.isEmpty then "unavailable"
      else f"${timings(timings.size / 2)}%.1f ms [${timings.head}%.1f, ${timings.last}%.1f]"
      val warmCalls = matching.filter(r => !r("sampleMemory").bool && r("exit").num == 0).flatMap { run =>
        val result = ujson.read(os.read(profiles / run("stdout").str))
        result("callNanosIncludingWarmup").arr.drop(number(result("warmup")).toInt).map(number(_) / 1e6)
      }.sorted
      val warm = if warmCalls.isEmpty then "unavailable"
      else f"${warmCalls(warmCalls.size / 2)}%.4f ms [${warmCalls.head}%.4f, ${warmCalls.last}%.4f]"
      s"<tr><td>$id · $workload</td><td>${trials.size}</td><td>${
          if peaks.isEmpty then "unavailable" else mib(peaks.max) + " MiB"
        }</td><td>$elapsed</td><td>$warm</td><td>${matching.count(_("exit").num != 0)}</td></tr>"
    }.mkString
    val retention = comparisons.map { (config, workload) =>
      val id  = config("id").str
      val run = runs.find(r =>
        r("configuration").str == id && r("name").str.endsWith(s"$workload-retention") && r("exit").num == 0
      )
      val values = run.toVector.flatMap { r =>
        r("phases").arr.filter(_("phase").str == "parent-cleanup-idle").flatMap { phase =>
          val elapsed = number(phase("nanoTime")) - number(r("startNanos"))
          r("samples").arr.filter(s =>
            number(s("elapsedNanos")) <= elapsed && s("parentBytes") != ujson.Null
          ).lastOption.map { sample =>
            (number(phase("cycle")), number(sample("parentBytes")))
          }
        }
      }
      if values.isEmpty then s"<p>$id · $workload: retention run not available.</p>"
      else
        val maximum = values.map(_._2).max * 1.1
        val points  = values.map((cycle, value) =>
          f"${40 + (cycle - 1) / 19 * 640}%.2f,${180 - value / maximum * 150}%.2f"
        ).mkString(" ")
        s"<article><h3>$id · $workload parent after worker reaping</h3><svg viewBox='0 0 720 210'><text x='40' y='15'>${mib(maximum)} MiB</text><path class='axis' d='M40 25V180H680'/><polyline class='line color0' points='$points'/><text x='40' y='202'>Cycle 1</text><text x='620' y='202'>Cycle 20</text></svg></article>"
    }.mkString
    val html =
      s"""<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Scala Wasm host checkpoint</title>
<style>:root{--background:#10141c;--foreground:#e6edf5;--muted:#aab9ca;--card:#1b2430;--border:#405063;--chart-1:#72b7ff;--chart-2:#f3b25f;--chart-3:#80d6a1;--radius:10px;--font-sans:system-ui,sans-serif}body{font-family:var(--font-sans);color:var(--foreground);background:var(--background);margin:0;padding:20px;box-sizing:border-box}p,li{line-height:1.55}h2{margin-top:32px}article,.sizes,.note{background:var(--card);border-radius:var(--radius);padding:16px;margin:12px 0}.barrow{display:grid;grid-template-columns:145px 1fr 95px;gap:8px;align-items:center;margin-top:8px;font-size:13px}.track{height:14px;background:var(--background)}.bar{height:14px;min-width:1px}.color0{background:var(--chart-1);stroke:var(--chart-1)}.color1{background:var(--chart-2);stroke:var(--chart-2)}.color2{background:var(--chart-3);stroke:var(--chart-3)}svg{display:block;width:100%;height:210px}svg text{fill:var(--muted);font-size:12px}.line{fill:none;stroke-width:2}.axis,.marker{fill:none;stroke:var(--border)}.marker{stroke-dasharray:3 4}table{border-collapse:collapse;width:100%;font-size:13px}th,td{padding:9px;border-bottom:1px solid var(--border);text-align:left}a{color:var(--chart-1)}.legend{font-size:13px;color:var(--muted)}@media(max-width:500px){.barrow{grid-template-columns:95px 1fr 75px;font-size:11px}table{font-size:11px}}</style>
<h1>Scala Wasm host: first checkpoint</h1><p>All three candidates have passed synthetic export and callback probes on JVM and Native Image. This supports continuing the Scala-host experiment. Full Morphir host qualification is still open.</p><p>$supplementLinks</p>
<div class="note">Compare matching workloads: identical synthetic byte-callback Wasm, then identical pinned Python legacy MEP compile input. Native executables load guest bytes from disk after compilation. The Extism candidate also ships a native library. Failed workload measurements are unavailable, with partial process data retained separately.</div>
<h2>Functional coverage</h2><table><thead><tr><th>Configuration</th><th>SDK legacy</th><th>Python legacy</th><th>Safety/limits</th><th>MCK SDK / Python</th></tr></thead><tbody>$functionalRows</tbody></table>
<p>Endive's interpreter is the leading pure Scala-host path from these experiments. Direct FFM passes the basic native ABI gate and real legacy workloads but fails the strict absolute-memory-ceiling test. GraalWasm passes core callbacks/native loading but refuses these unbounded-memory guests under this experiment's admission policy. A host verdict and a CLI replacement verdict are separate: this is not a qualified replacement Morphir CLI.</p>
<p>Libextism 1.41.0's <a href="https://github.com/extism/extism/blob/051b765bb6abe4fe95d5e4f489f03ad7b137edbc/runtime/src/current_plugin.rs">MemoryLimiter</a> tracks growth bytes and resets them per call. Its manifest max_pages therefore does not establish an absolute per-instance ceiling. The failed bounded-growth receipts remain in the checks directory. FFM's functional and footprint observations do not override that failed requirement.</p>
<h2>Application sizes</h2><p>Logical file bytes, MiB = 1,048,576 bytes. JVM executable values are launchers. JVM distributions require an externally provided GraalVM 25.0.1 installation, measured separately at ${mib(
          number(settings("externalJavaRuntimeBytes"))
        )} MiB. The full JDK is not a minimal runtime image. Archives use normalized ustar plus JDK GZIP default deflate level 6 and preserve executable modes. Debug symbols are not produced separately; no explicit post-link stripping is used. Guests are external inputs, not bundled into these packages. Native-library bytes are included in FFM installed/package totals.</p>$sizeBars
<h2>Observed runtime profiles</h2><p>The external sampler polls at 50 ms plus measurement cost. It includes the host parent and its workers, excluding Mill, the compiler and sampler. Aggregate RSS is the simultaneous process sum, which double-counts shared resident pages. It is not physical memory. Missing samples remain null. Short call bursts can fall between polls.</p>
<p>RSS probe cost per sample: $overheadText. This is sampling work duration, not a measured total application slowdown. The separate no-sampler runs have no observation holds. Phase events still record heap/GC counters, so no claim of zero instrumentation overhead is made. Fresh-process trials do not flush OS or runtime caches; cache-cold machine startup was not measured. Extism's optional system compilation-cache configuration was not separately instrumented.</p>
<table><thead><tr><th>Configuration / workload</th><th>Fresh trials</th><th>Highest aggregate RSS</th><th>Startup to first result without RSS sampler, median [range]</th><th>Warm call without RSS sampler, median [range]</th><th>Failed runs</th></tr></thead><tbody>$summaries</tbody></table>
<p>Warm-call summaries pool 500 measured calls across five fresh processes after discarding each process's ten recorded warm-up calls. Per-call ranges show observations, not confidence intervals.</p>
<p>First-result timestamps follow one successful export/byte exchange or one real MEP compile, before the recorded 10 warm-up and 100 measured calls. Per-call nanoseconds are retained in worker results. Native builds use O2, compatibility CPU and default Serial GC. JVM runs use the pinned JDK's defaults; collector names/counts/times and runtime heap counters are in phase events. The historical raw field heapCommittedBytes records Runtime.totalMemory(), a runtime-reported heap capacity, not measured OS committed or resident memory. Heap used is totalMemory minus freeMemory; heapMaxBytes is Runtime.maxMemory(). Native Image can report a large capacity while RSS remains small. Endive's available linear-memory size is retained at loaded and calls-complete markers. GraalWasm includes Truffle runtime compilation. No forced GC or NMT is enabled in baseline profiles. Heap counters exclude third-party native allocation; reserved virtual memory and exact native allocation attribution were not collected. Fully instrumentation-free startup timing is unavailable; the no-RSS-sampler timings retain phase events.</p>
<h2>Two-worker costs</h2><p>Each column is its own observed concurrent peak. The aggregate column is computed from aligned process sums, not by adding the other two column maxima.</p><table><thead><tr><th>Configuration / workload</th><th>Parent peak</th><th>Workers peak</th><th>Concurrent aggregate peak</th></tr></thead><tbody>$concurrentRows</tbody></table>
<h2>Idle, active and cleanup phases</h2><p>Loaded-idle and cleanup columns are medians of phase-window samples across fresh trials. Active peak uses only samples inside the measured-call window. A short burst with no poll remains unsampled.</p><table><thead><tr><th>Configuration / workload</th><th>Worker loaded idle</th><th>Active aggregate peak</th><th>Parent after reaping</th></tr></thead><tbody>$phaseRows</tbody></table>
<h2>Lifecycle timelines</h2><p class="legend">Blue: parent. Orange: workers. Green: aggregate. Vertical markers identify loading, loaded idle, active calls, closed and reaped phases; hover for the phase name. Each trial holds observable worker phases for 250 ms and the surviving parent for two seconds after reaping.</p>$timelines
<h2>Parent retention</h2><p>Last parent sample before each end-of-quiet marker, after a two-second post-reap interval. RSS growth alone does not establish a leak. No forced GC or cache reset is used. The experimental parent retains audit receipts, including call timings, across cycles; that bookkeeping is part of the observed parent memory.</p>$retention
<h2>Native build costs</h2><p>Native Image's own build duration and peak RSS, separate from runtime measurements. These exclude Scala compilation, artifact acquisition and packaging. Raw logs retain the compiler/runtime settings and their reported units.</p><table><thead><tr><th>Configuration</th><th>Build duration</th><th>Build peak RSS</th><th>Evidence</th></tr></thead><tbody>$buildRows</tbody></table>
<h2>Component and CLI verdicts</h2><p>None of the six configurations is component-qualified. <a href="https://github.com/bytecodealliance/endive/blob/7c46a4754ec6bfba8e038cef0220aee5813770ae/docs/blog/2026-10-30-endive-1.2.0.md">Endive 1.2.0's release account</a> places Component Model, WIT bindgen and Canonical ABI work in the companion endive-cm repository. Companion APIs and async components remain unverified here. <a href="https://github.com/oracle/graal/blob/vm-25.0.1/wasm/src/org.graalvm.wasm/src/org/graalvm/wasm/WasmOptions.java">Pinned GraalWasm options</a> expose core-Wasm/WASI P1 facilities, not a component result from this experiment. Extism's selected C plugin ABI is a core-Wasm interface; no WIT/component adapter was exercised.</p>
<h2>Remaining blockers</h2><p>The pinned portable MoonBit source build is blocked by an installed compiler/core mismatch. Its receipt and build log are retained without substituting a synthetic guest. FFM needs an actual absolute memory-ceiling solution. GraalWasm needs a bounded loading path for these guests. MCK pass/fail and independent report-check results are retained per ABI and guest; guest-corpus success is narrower than full independent host conformance. Production discovery, installation and CLI migration require a separate change.</p>
<p>Kyo FFI RC7 generation rejected pointer-bearing callback arguments. The experiment therefore uses direct JDK FFM. C layout evidence is retained separately. Native packaging is target-specific to macOS arm64.</p>
<h2>Raw evidence</h2><p><a href="samples.csv">RSS CSV</a> · <a href="profiles.json">Profiles, phase events and command receipts</a> · <a href="sizes.json">Distribution file hashes and size inventories</a></p>
<p><a href="qualification/qualification.json">Real guest exchanges</a> · <a href="mck/mck.json">MCK run/check receipts</a> · <a href="checks/checks.json">Packaging, callback, cancellation and limit checks</a> · <a href="abi/layout.json">C layout proof</a> · <a href="guests/surfaces.json">Complete guest imports/exports</a> · <a href="moonbit/receipt.json">MoonBit build blocker</a></p>
<p>Candidate documentation: <a href="https://endive.run/">Endive</a>, <a href="https://www.graalvm.org/webassembly/docs/">GraalWasm</a>, <a href="https://github.com/extism/extism/tree/v1.41.0">Extism v1.41.0</a>.</p></html>"""
    os.write.over(output / "index.html", html)
    val compactSizes = configs.map { config =>
      val bytes = number(config("installedBytes"))
      s"<div class='barrow'><span>${config("id").str}</span><div class='track'><div class='bar color0' style='width:${bytes /
          maxSize * 100}%'></div></div><span>${size(bytes)}</span></div>"
    }.mkString
    val memoryPeaks = comparisons.map { (config, workload) =>
      val values = runs.filter(r =>
        r("configuration") == config("id") &&
          r("name").str.contains(s"-$workload-trial-") && r("exit").num == 0
      )
        .flatMap(_("samples").arr.filter(_("aggregateBytes") != ujson.Null).map(s => number(s("aggregateBytes"))))
      (config("id").str, workload, values.maxOption)
    }
    val maxMemory  = memoryPeaks.flatMap(_._3).max
    val memoryBars = memoryPeaks.map { (id, workload, bytes) =>
      val color = if workload == "python" then "color2" else "color1"
      s"<div class='barrow'><span>$id · $workload</span><div class='track'>${bytes.fold("")(value =>
          s"<div class='bar $color' style='width:${value / maxMemory * 100}%'></div>"
        )}</div><span>${bytes.fold("unavailable")(size)}</span></div>"
    }.mkString
    val head =
      """<!doctype html><html lang="en"><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1"><title>Scala host footprint comparison</title>
<style>body{margin:0;font-family:var(--font-sans,system-ui);color:var(--foreground)}p{line-height:1.5}.barrow{display:grid;grid-template-columns:minmax(130px,1fr) 2fr 90px;gap:8px;align-items:center;margin:12px 0;font-size:13px}.track{height:14px;background:var(--secondary)}.bar{height:14px;min-width:1px}.color0{background:var(--chart-1)}.color1{background:var(--chart-2)}.color2{background:var(--chart-3)}@media(max-width:450px){.barrow{grid-template-columns:115px 1fr 75px;font-size:11px}}</style>"""
    os.write.over(
      output / "summary.html",
      head +
        "<p>Scala hosting works in the basic JVM and Native Image probes. Endive is the leading bounded host candidate; FFM's absolute memory ceiling fails, and GraalWasm rejects the real unbounded-memory guests.</p>" +
        "<h2>Installed application sizes</h2>" + compactSizes +
        "<p>FFM totals include libextism. JVM totals exclude the separately provisioned Java runtime. MiB = 1,048,576 bytes.</p>" +
        "<h2>Parent + worker RSS peaks</h2>" + memoryBars +
        "<p>Highest simultaneous RSS sum across five fresh trials per workload. Each row identifies synthetic byte callbacks or real Python legacy MEP compile. Shared pages may be counted twice. Unavailable measurements are not zero. Full lifecycle timelines, package sizes, timings, retention plots and raw receipts are in the accompanying report.</p></html>"
    )
