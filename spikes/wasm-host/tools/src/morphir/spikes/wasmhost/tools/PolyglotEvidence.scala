package morphir.spikes.wasmhost.tools

import java.nio.file.Path
import java.io.{BufferedOutputStream, FileOutputStream}
import java.util.zip.GZIPOutputStream

/** Separate compiler proof, deliberately not a replacement for the six-host qualification matrix. */
object PolyglotEvidence:
  private val protocol =
    "five fresh compile/run trials without RSS sampling or holds and five separate RSS-sampled trials per target; phase-clock and heap observations retained in both; 200ms phase holds only in memory trials; no warm compiler benchmark"
  private def number(value: ujson.Value): Double            = Benchmarks.number(value)
  private def median(values: Seq[Double]): Double           = values.sorted.apply(values.size / 2)
  private def archive(root: os.Path, output: os.Path): Unit =
    (os.walk(root) :+ root).foreach(path =>
      java.nio.file.Files.setLastModifiedTime(path.toNIO, java.nio.file.attribute.FileTime.fromMillis(0L)): Unit
    )
    val tar = output / os.up / "payload.tar"
    os.proc(
      "tar",
      "--format=ustar",
      "--uid=0",
      "--gid=0",
      "--uname=root",
      "--gname=root",
      "-cf",
      tar,
      "-C",
      root,
      "."
    ).call(env = Map("COPYFILE_DISABLE" -> "1"))
    val zip = GZIPOutputStream(BufferedOutputStream(FileOutputStream(output.toIO)))
    try
      val _ = java.nio.file.Files.copy(tar.toNIO, zip)
    finally zip.close()
    val _ = os.remove(tar)

  def main(args: Array[String]): Unit =
    if args.headOption.contains("report") then
      val root = os.Path(Path.of(args(1)).toRealPath())
      val data = ujson.read(os.read(root / "receipt.json"))
      data("protocol") = protocol
      (data("checks").arr ++ data("runs").arr).foreach { run =>
        run("sourceSha256") = Distribution.hash(root / "runs" / s"${run("name").str}-workspace" / "answer.mbt")
      }
      require(data("checks").arr.size == 12 && data("runs").arr.size == 20, "incomplete experiment")
      data("checks").arr.filter(_("name").str.endsWith("arity-original")).foreach(check =>
        require(
          check("compiler")("error").str.contains("ArrayIndexOutOfBoundsException"),
          "different import regression"
        )
      )
      val distributions = Seq(root / "distribution") ++ args.drop(2).map(path => os.Path(Path.of(path).toRealPath()))
      distributions.foreach { directory =>
        val inventory = ujson.read(os.read(directory / "configurations.json"))
        inventory("configurations").arr.foreach { config =>
          config("files").arr.foreach { file =>
            val delivered = directory / config("id").str / os.RelPath(file("path").str)
            require(
              os.size(delivered).toDouble == number(file("bytes")) &&
                Distribution.hash(delivered) == file("sha256").str,
              s"delivery identity changed: $delivered"
            )
          }
          require(
            Distribution.hash(directory / s"${config("id").str}.tar.gz") == config("packageSha256").str,
            "package identity changed"
          )
        }
      }
      os.write.over(root / "receipt.json", data.render(indent = 2))
      report(root, data)
      println("Verified compiler evidence and retained distribution inventories; regenerated reports only")
      return
    val input  = os.Path(Path.of(args(0)).toRealPath())
    val output = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val settings   = ujson.read(os.read(input))
    val sources    = os.Path(Path.of(settings("proofSources").str).toRealPath())
    val fixtureDir = output / "fixtures"
    os.makeDir.all(fixtureDir)
    val version = os.proc("mise", "exec", "wasm-tools@1.262.0", "--", "wasm-tools", "--version").call().out.text()
    require(version.startsWith("wasm-tools 1.262.0"), "fixture compiler version differs")
    os.write(output / "wasm-tools-version.txt", version)
    Seq("gc-import", "duplicate-arity").foreach { name =>
      os.proc(
        "mise",
        "exec",
        "wasm-tools@1.262.0",
        "--",
        "wasm-tools",
        "parse",
        sources / s"$name.wat",
        "-o",
        fixtureDir / s"$name.wasm"
      ).call()
      os.proc("mise", "exec", "wasm-tools@1.262.0", "--", "wasm-tools", "validate", fixtureDir / s"$name.wasm").call()
    }
    val distribution = output / "distribution"
    Distribution.main(Array(input.toString, distribution.toString))
    val packaged = ujson.read(os.read(distribution / "configurations.json"))
    val upstream = os.Path(Path.of(settings("compilerBundle").str).toRealPath())
    val payload  = output / "compiler-payload"
    os.makeDir.all(payload / "bundle")
    os.copy(upstream / "moonc.js", payload / "bundle" / "moonc.js")
    os.copy(upstream / "moonc.assets", payload / "bundle" / "moonc.assets")
    os.copy(upstream / os.up / "core", payload / "core")
    Seq("LICENSE.TXT", "CORE-LICENSE").foreach(name => os.copy(upstream / os.up / name, payload / name))
    archive(payload, output / "compiler-payload.tar.gz")
    val payloadInventory = Distribution.files(payload).map(file =>
      ujson.Obj(
        "path"   -> file.relativeTo(payload).toString,
        "bytes"  -> os.size(file),
        "sha256" -> Distribution.hash(file)
      )
    )
    val payloadBytes = payloadInventory.map(file => number(file("bytes"))).sum
    val results      = scala.collection.mutable.ArrayBuffer.empty[ujson.Obj]
    val checks       = scala.collection.mutable.ArrayBuffer.empty[ujson.Obj]
    val records      = output / "runs"
    os.makeDir.all(records)
    packaged("configurations").arr.foreach { config =>
      val before = Distribution.hash(os.Path(config("command")(0).str))
      def run(
          name: String,
          source: String,
          expected: Int = 42,
          mode: String = "compiler",
          aliases: Boolean = true,
          sampled: Boolean = false,
          expectPass: Boolean = true
      ): ujson.Obj =
        val id        = s"${config("id").str}-$name"
        val workspace = records / s"$id-workspace"
        val evidence  = records / s"$id-evidence"
        os.makeDir.all(workspace)
        os.write(workspace / "answer.mbt", source)
        // A real symlink escape must fail in the bridge, not merely a missing-file lookup.
        os.symlink(workspace / "escape", payload / "bundle" / "moonc.js")
        Seq("gc-import", "duplicate-arity").foreach(n => os.copy(fixtureDir / s"$n.wasm", workspace / s"$n.wasm"))
        val arguments = Seq(
          "run",
          "--bundle",
          (payload / "bundle").toString,
          "--workspace",
          workspace.toString,
          "--output",
          evidence.toString,
          "--mode",
          mode
        ) ++ (if aliases then Seq("--unique-imports") else Seq.empty) ++
          (if mode == "compiler" then
             Seq(
               "--arguments",
               (sources / "compile.json").toString,
               "--verify-wasm",
               "answer.wasm",
               "--expected-result",
               expected.toString
             )
           else Seq.empty) ++
          (if sampled then Seq("--hold-ms", "200") else Seq.empty)
        val measured = Profile.run(config, packaged, records, id, arguments, sampleMemory = sampled)
        measured("scope") = "isolated polyglot MoonBit compiler proof; one process, no worker"
        measured("sourceSha256") = Distribution.hash(workspace / "answer.mbt")
        val receipt = ujson.read(os.read(evidence / "receipt.json"))
        require((measured("exit").num == 0) == expectPass, s"unexpected exit for $id")
        require((receipt("status").str == "pass") == expectPass, s"unexpected receipt for $id")
        require(receipt("openFilesAfterClose").num == 0, s"open files after $id")
        if expectPass && mode == "compiler" then
          require(receipt("generated")("answer").num == expected, "output result differs")
          require(receipt("provenance")("nonImportSectionsUnchanged").bool, "adapter changed compiler code")
        if !expectPass && mode == "compiler" then
          require(!os.exists(workspace / "answer.wasm"), "invalid input left executable output")
          require(
            os.size(evidence / "stderr.txt") > 0 && os.size(evidence / "stderr.txt") <= 1048576,
            "invalid input diagnostics missing or unbounded"
          )
        measured("receipt") = (evidence / "receipt.json").relativeTo(output).toString
        measured("expectedPass") = expectPass
        measured("compiler") = receipt
        os.write.over(records / s"$id.json", measured.render(indent = 2))
        measured
      val source = os.read(sources / "answer.mbt")
      // Negative regression: the unmodified duplicate import name fails when arity decreases.
      checks += run("arity-original", source, mode = "duplicate-arity", aliases = false, expectPass = false)
      checks += run("arity-adapted", source, mode = "duplicate-arity")
      checks += run("gc", source, mode = "gc-probe", aliases = false)
      checks += run("bounds-tags-utf8", source, mode = "bounds", aliases = false)
      checks += run("invalid", "pub fn answer( -> Int {", expectPass = false)
      checks += run("second-source", source.replace("42", "43"), expected = 43)
      (1 to 5).foreach { trial =>
        results += run(s"timing-$trial", source)
        results += run(s"memory-$trial", source, sampled = true)
      }
      require(Distribution.hash(os.Path(config("command")(0).str)) == before, "delivery executable changed")
    }
    val summary = packaged("configurations").arr.map { config =>
      val runs    = results.filter(_("configuration").str == config("id").str).toVector
      val timing  = runs.filter(!_("sampleMemory").bool)
      val memory  = runs.filter(_("sampleMemory").bool)
      val first   = timing.map(run => number(run("startupToFirstResultNanos")) / 1e6)
      val compile = timing.map { run =>
        val phases = run("compiler")("phases").arr
        (number(phases.find(_("phase").str == "compiler-returned").get("elapsedNanos")) -
          number(phases.find(_("phase").str == "runtime-ready").get("elapsedNanos"))) / 1e6
      }
      val peaks = memory.map(run =>
        run("samples").arr.flatMap(sample =>
          if sample("aggregateBytes").isNull then None else Some(number(sample("aggregateBytes")))
        ).max
      )
      ujson.Obj(
        "id"                             -> config("id"),
        "installedBytes"                 -> config("installedBytes"),
        "packageBytes"                   -> config("packageBytes"),
        "executableBytes"                -> config("executableOrLauncherBytes"),
        "startToResultMedianMs"          -> median(first),
        "startToResultRangeMs"           -> ujson.Arr(first.min, first.max),
        "compilerLoadAndCompileMedianMs" -> median(compile),
        "peakRssMedianBytes"             -> median(peaks),
        "peakRssRangeBytes"              -> ujson.Arr(peaks.min, peaks.max),
        "workerBytes"                    -> 0,
        "nativeImageBuildTime"           -> config.obj.getOrElse("nativeImageBuildTime", ujson.Null),
        "nativeImageBuildPeakRss"        -> config.obj.getOrElse("nativeImageBuildPeakRss", ujson.Null)
      )
    }
    val receipt = ujson.Obj(
      "status"       -> "pass",
      "graalRelease" -> "25.5.5.0.3",
      "protocol"     -> protocol,
      "memory"       ->
        "50ms ps RSS sampling, parent is compiler process, workers zero; shared-page RSS, not private/committed memory; no forced GC; no 20-cycle leak claim",
      "scope"                -> "tiny source without standard library; full task 8.3 remains open",
      "payloadBytes"         -> payloadBytes,
      "payloadPackageBytes"  -> os.size(output / "compiler-payload.tar.gz"),
      "payloadPackageSha256" -> Distribution.hash(output / "compiler-payload.tar.gz"),
      "payloadFiles"         -> ujson.Arr.from(payloadInventory),
      "summary"              -> ujson.Arr.from(summary),
      "checks"               -> ujson.Arr.from(checks),
      "runs"                 -> ujson.Arr.from(results)
    )
    os.write(output / "receipt.json", receipt.render(indent = 2))
    report(output, receipt)

  private def report(output: os.Path, data: ujson.Value): Unit =
    val summaries = data("summary").arr
    val native    = summaries.find(_("id").str == "polyglot-native").get
    val delivery  = ujson.read(os.read(output / "distribution" / "configurations.json"))
    val build     =
      s"Native Image build: ${native("nativeImageBuildTime").str} Peak build RSS ${native("nativeImageBuildPeakRss").str}; optimization O2, compatibility CPU, Serial GC. External JVM runtime ${f"${number(delivery("externalJavaRuntimeBytes")) /
            1048576}%.2f"} MiB, provisioned separately."
    def chart(title: String, key: String, divisor: Double, unit: String): String =
      val maximum = summaries.map(row => number(row(key)) / divisor).max
      s"<h3>$title</h3>" + summaries.map { row =>
        val value = number(row(key)) / divisor
        f"<div class='row'><span>${row("id").str}</span><div class='track'><div class='bar' style='width:${value /
            maximum * 100}%.2f%%'></div></div><strong>$value%.2f $unit</strong></div>"
      }.mkString
    val timelines =
      data("runs").arr.filter(_("sampleMemory").bool).groupBy(_("configuration").str).toVector.sortBy(_._1).map {
        (id, runs) =>
          val run      = runs.head
          val samples  = run("samples").arr.filterNot(_("aggregateBytes").isNull)
          val duration = number(run("elapsedNanos"))
          val peak     = samples.map(s => number(s("aggregateBytes"))).max
          val points   = samples.map(s =>
            f"${number(s("elapsedNanos")) / duration * 680}%.1f,${130 - number(s("aggregateBytes")) / peak * 110}%.1f"
          ).mkString(" ")
          val markers = run("phases").arr.map { p =>
            val x = (number(p("nanoTime")) - number(run("startNanos"))) / duration * 680
            f"<line x1='$x%.1f' x2='$x%.1f' y1='10' y2='135' stroke='currentColor' opacity='.25'/><text x='$x%.1f' y='150' transform='rotate(35 $x%.1f 150)' font-size='10'>${p("phase").str}</text>"
          }.mkString
          s"<h3>$id: first memory trial</h3><svg viewBox='0 0 710 220' role='img' aria-label='RSS timeline'><polyline points='$points' fill='none' stroke='var(--chart-2,#5599dd)' stroke-width='2'/>$markers</svg><p>Peak ${f"${peak /
                1048576}%.2f"} MiB. Parent equals aggregate; worker RSS is zero.</p>"
      }.mkString
    val html =
      s"""<!doctype html><html lang="en"><meta charset="utf-8"><title>Polyglot MoonBit compiler proof</title>
<style>:root{font-family:system-ui;color-scheme:light dark}body{max-width:1050px;margin:24px auto;padding:12px}a{color:inherit}.row{display:flex;align-items:center;gap:10px;margin:12px 0}.row span{min-width:125px}.track{flex:1;min-width:0}.bar{height:22px;background:var(--chart-1,#6489bc);border-radius:3px}.row strong{min-width:95px;white-space:nowrap}svg{width:100%;max-height:250px}section{margin:25px 0}code{overflow-wrap:anywhere}</style>
<h1>MoonBit compiler runs on JVM and Native Image</h1>
<p>GraalJS + GraalWasm 25.5.5.0.3 loads the pinned Wasm compiler, compiles a tiny MoonBit function and executes its emitted 173-byte Wasm with answer 42. A second source returns 43 through the unchanged host executable. No native moonc or Node process is used.</p>
<p>The unmodified loader fails when a repeated import name has decreasing arity. A tested in-memory import-name adapter fixes the regression. Receipts verify every non-import section remains unchanged and retain original/adapted SHA-256 hashes. This is a compatibility workaround, not unmodified upstream support.</p>
${chart("Installed host application", "installedBytes", 1048576, "MiB")}
${chart("Compressed host package", "packageBytes", 1048576, "MiB")}
<p>Shared compiler payload adds ${f"${number(data("payloadBytes")) /
            1048576}%.2f"} MiB installed and ${f"${number(data("payloadPackageBytes")) / 1048576}%.2f"} MiB compressed, including expanded core and licenses. The JVM needs its separately inventoried external JDK; Native Image does not. Host packages exclude this shared payload. No separate debug symbols or explicit post-link stripping.</p>
<p>$build</p>
${chart("Fresh process to verified result, median of five", "startToResultMedianMs", 1, "ms")}
<p>Includes startup, JS runtime, compiler loading/compilation, import verification and output execution. No RSS sampler or phase holds in timing trials. Compiler-only warm throughput is unavailable.</p>
${chart("Sampled runtime peak RSS, median of five", "peakRssMedianBytes", 1048576, "MiB")}
<p>${data("memory").str}. Heap-used phase observations are in the raw receipts. Idle/cleanup holds do not force GC or establish leak freedom. The original six-host footprint and speed matrices remain separate.</p>
$timelines
<p>Both targets pass GC-array/JS callback, exception-tag/UTF-8, five denied-authority checks including a real symlink escape, invalid-source diagnostics and file cleanup. Negative arity regression is retained. Larger multi-file/core, cancellation and long-lived compiler qualification remain open.</p>
<p><a href="receipt.json">All samples, commands, checks, package inventory and summary</a> · <a href="distribution/configurations.json">Delivery identities and external Java bytes</a> · <a href="wasm-tools-version.txt">Fixture compiler</a></p></html>"""
    os.write.over(output / "index.html", html)
    val compact =
      s"""<!doctype html><html lang="en"><meta charset="utf-8"><title>Polyglot compiler comparison</title>
<style>:root{font-family:var(--font-sans,system-ui);color:var(--foreground);font-size:14px}body{margin:0}h3{font-size:15px;margin:18px 0 8px}p{color:var(--muted-foreground);line-height:1.45}.row{display:flex;align-items:center;gap:10px;margin:7px 0}.row span{min-width:105px}.track{flex:1;min-width:0}.bar{height:16px;background:var(--chart-1);border-radius:3px}.row strong{min-width:85px;font-size:12px;white-space:nowrap}</style>
${chart("Installed host application", "installedBytes", 1048576, "MiB")}
${chart("Fresh process to compiled and verified result", "startToResultMedianMs", 1000, "s")}
${chart("Sampled runtime peak RSS", "peakRssMedianBytes", 1048576, "MiB")}
<p>Median of five fresh trials per target. Memory uses separate sampled trials with 200 ms phase holds. One process, no workers. JVM and Native Image both compile MoonBit and execute the result, 42.</p>
<p>Host sizes exclude a shared ${f"${number(data("payloadBytes")) / 1048576}%.2f"} MiB compiler payload. JVM also needs ${f"${number(delivery("externalJavaRuntimeBytes")) /
            1048576}%.2f"} MiB external JDK. Import aliasing is required. Multi-file/core and compiler cancellation qualification remain open.</p></html>"""
    os.write.over(output / "summary.html", compact)
