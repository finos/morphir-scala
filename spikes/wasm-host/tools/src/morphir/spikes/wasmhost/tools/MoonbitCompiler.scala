package morphir.spikes.wasmhost.tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Path
import java.time.Duration

/** Acquires upstream compiler data without installing or executing its JavaScript launcher. */
object MoonbitCompiler:
  private val release    = "v0.10.19+97801ee1b"
  private val revision   = "d4ada10d212b5376f7f8bf49cd2fbaa275a395df"
  private val archiveSha = "ab55f4d92206916308922c768033d8a1b0eef5a2a3ea4c3ba8569c52cd40e2a9"
  private val url        =
    "https://github.com/moonbitlang/moonbit-compiler/releases/download/v0.10.19%2B97801ee1b/moonbit-wasm.tar.gz"

  private final class Reader(bytes: Array[Byte]):
    private var position = 0
    def remaining: Int   = bytes.length - position
    def byte(): Int      =
      require(remaining > 0, "truncated Wasm section")
      val value = bytes(position) & 255
      position += 1
      value
    def leb(): Long =
      @annotation.tailrec
      def loop(shift: Int, value: Long): Long =
        require(shift < 35, "excessive u32 LEB")
        val next   = byte()
        val result = value | ((next & 127).toLong << shift)
        if (next & 128) == 0 then result else loop(shift + 7, result)
      loop(0, 0)
    def take(length: Int): Array[Byte] =
      require(length >= 0 && length <= remaining, "invalid Wasm section/string length")
      val result = java.util.Arrays.copyOfRange(bytes, position, position + length)
      position += length
      result
    def string(): String  = String(take(Math.toIntExact(leb())), java.nio.charset.StandardCharsets.UTF_8)
    def valueType(): Unit = if Set(0x63, 0x64)(byte()) then { val _ = leb() }
    def limits(): Unit    =
      val flags = leb()
      val _     = leb()
      if (flags & 1) != 0 then { val _ = leb() }

  // Section inventory deliberately skips type bodies so GC rec groups remain inspectable.
  private def surface(bytes: Array[Byte]): ujson.Obj =
    val reader = Reader(bytes)
    require(reader.take(8).sameElements(Array[Byte](0, 97, 115, 109, 1, 0, 0, 0)), "not core Wasm")
    val imports   = Vector.newBuilder[ujson.Value]
    val exports   = Vector.newBuilder[ujson.Value]
    var firstType = -1
    @annotation.tailrec
    def sections(): Unit =
      if reader.remaining > 0 then
        val id      = reader.byte()
        val section = Reader(reader.take(Math.toIntExact(reader.leb())))
        if id == 1 then
          val count = section.leb()
          if count > 0 then firstType = section.byte()
        if id == 2 then
          val count = Math.toIntExact(section.leb())
          (0 until count).foreach { _ =>
            val module = section.string()
            val name   = section.string()
            val kind   = section.byte()
            imports += ujson.Obj("module" -> module, "name" -> name, "kind" -> kind)
            kind match
              case 0 => val _ = section.leb()
              case 1 => section.valueType(); section.limits()
              case 2 => section.limits()
              case 3 => section.valueType(); val _ = section.byte()
              case 4 => val _ = section.byte(); val _ = section.leb()
              case _ => throw IllegalArgumentException(s"unknown import kind $kind")
          }
        if id == 7 then
          val count = Math.toIntExact(section.leb())
          (0 until count).foreach { _ =>
            val name  = section.string()
            val kind  = section.byte()
            val index = section.leb()
            exports += ujson.Obj("name" -> name, "kind" -> kind, "index" -> index)
          }
        sections()
    sections()
    ujson.Obj(
      "firstTypeOpcode" -> firstType,
      "imports"         -> ujson.Arr.from(imports.result()),
      "exports"         -> ujson.Arr.from(exports.result())
    )

  def main(args: Array[String]): Unit =
    val settings = ujson.read(os.read(os.Path(Path.of(args(0)).toRealPath())))
    val output   = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    def fetch(uri: String, file: os.Path): Unit =
      val response = client.send(
        HttpRequest.newBuilder(URI.create(uri)).timeout(Duration.ofMinutes(3)).GET().build(),
        HttpResponse.BodyHandlers.ofByteArray()
      )
      require(response.statusCode() == 200, s"download failed: $uri ${response.statusCode()}")
      os.write.over(file, response.body())
    val archive = output / "moonbit-wasm.tar.gz"
    fetch(url, archive)
    require(Distribution.hash(archive) == archiveSha, "MoonBit compiler archive digest mismatch")
    val bundle = output / "bundle"
    os.makeDir.all(bundle)
    val paths = os.proc("tar", "-tzf", archive).call().out.lines()
    require(paths.forall(name => !name.startsWith("/") && !name.split('/').contains("..")), "unsafe archive path")
    os.proc("tar", "-xzf", archive, "-C", bundle).call()
    fetch(
      s"https://raw.githubusercontent.com/moonbitlang/moonbit-compiler/$revision/LICENSE.TXT",
      output / "LICENSE.TXT"
    )
    val core = output / "core"
    os.makeDir.all(core)
    val corePaths = os.proc("tar", "-tzf", bundle / "core.tar.gz").call().out.lines()
    require(
      corePaths.forall(name => !name.startsWith("/") && !name.split('/').contains("..")),
      "unsafe core archive path"
    )
    os.proc("tar", "-xzf", bundle / "core.tar.gz", "-C", core).call()
    os.copy.over(core / "core" / "LICENSE", output / "CORE-LICENSE")
    val compiler = os.walk(bundle / "moonc.assets").find(_.ext == "wasm").get
    val modules  = os.walk(bundle).filter(_.ext == "wasm").map { file =>
      val inspected = surface(os.read.bytes(file))
      inspected("path") = file.relativeTo(bundle).toString
      inspected("sha256") = Distribution.hash(file)
      inspected("bytes") = os.size(file)
      inspected
    }
    os.write.over(output / "surfaces.json", ujson.Arr.from(modules).render(indent = 2))
    val loading = settings("configurations").arr.map { config =>
      val run = Profile.run(
        config,
        settings,
        output,
        s"${config("id").str}-moonc-load",
        Seq("probe", "--guest", compiler.toString),
        false
      )
      run("status") = if run("exit").num == 0 then "unexpected-probe-pass" else "adapter-refused"
      run("scope") = "Wasm MoonBit compiler admission, not Morphir guest qualification"
      run("engineCapability") = "not established by Morphir adapter admission"
      run("compilation") = "blocked; compiler-specific runtime/entrypoint adapter absent"
      run
    }
    val inventory = os.walk(bundle).filter(os.isFile(_)).map(file =>
      ujson.Obj(
        "path"   -> file.relativeTo(bundle).toString,
        "bytes"  -> os.size(file),
        "sha256" -> Distribution.hash(file)
      )
    )
    val coreFiles    = os.walk(core).filter(os.isFile(_))
    val payloadBytes = os.size(compiler) + os.size(bundle / "moonc.js") + coreFiles.map(os.size(_)).sum +
      os.size(output / "LICENSE.TXT") + os.size(output / "CORE-LICENSE")
    os.write(
      output / "receipt.json",
      ujson.Obj(
        "release"                       -> release,
        "candidateCompilerPayloadBytes" -> payloadBytes,
        "payloadScope"                  ->
          "moonc Wasm + JS loader + expanded core + licenses; excludes an unimplemented compiler adapter, moonfmt and mooninfo",
        "releaseRepositoryRevision" -> revision,
        "compilerBuildIdentity"     -> "97801ee1b from release name; not verified as the release repository commit",
        "url"                       -> url,
        "archiveSha256"             -> archiveSha,
        "archiveBytes"              -> os.size(archive),
        "bundleLogicalBytesIncludingCompressedCore" -> inventory.map(value => Benchmarks.number(value("bytes"))).sum,
        "extractedCoreLogicalBytes"                 -> coreFiles.map(os.size(_)).sum,
        "coreArchiveSha256"                         -> Distribution.hash(bundle / "core.tar.gz"),
        "coreSourceRevision"  -> "not supplied as a separate marker in this bundle; content bound by archive digest",
        "moonRevision"        -> os.read(bundle / "moon_version").trim,
        "licenseSha256"       -> Distribution.hash(output / "LICENSE.TXT"),
        "coreLicenseSha256"   -> Distribution.hash(output / "CORE-LICENSE"),
        "loader"              -> "moonc.js, retained as data; not executed by Node or another JS runtime",
        "runtimeRequirements" -> ujson.Arr(
          "wasm_of_ocaml host ABI",
          "GC/reference types",
          "JavaScript bindings and string builtins",
          "exception tags",
          "bounded virtual filesystem required"
        ),
        "status"                -> "blocked",
        "files"                 -> ujson.Arr.from(inventory),
        "loading"               -> ujson.Arr.from(loading),
        "compileLatency"        -> ujson.Null,
        "compilerRuntimeMemory" -> ujson.Null,
        "nextExperiment"        ->
          "Implement and qualify a bounded wasm_of_ocaml compiler adapter and test engine features independently; do not relax Morphir guest admission"
      ).render(indent = 2)
    )
    val rows = loading.map(run =>
      s"<tr><td>${run("configuration").str}</td><td>${run("status").str}</td><td>blocked / unavailable</td><td><a href='${run("stderr").str}'>Loading log</a></td></tr>"
    ).mkString
    os.write(
      output / "index.html",
      s"""<!doctype html><html lang="en"><meta charset="utf-8"><title>MoonBit compiler hosting</title><style>body{font:16px/1.5 system-ui;max-width:1000px;margin:24px auto;padding:16px}table{border-collapse:collapse;width:100%}td,th{text-align:left;padding:8px;border-bottom:1px solid #aaa}</style>
<h1>MoonBit Wasm compiler hosting</h1><p>The verified $release bundle cannot run through the current Morphir guest adapters. These loading attempts do not establish whether each underlying engine can support the compiler with a new runtime adapter. Compilation, compiler-execution latency and compiler runtime memory remain unavailable.</p>
<table><tr><th>Configuration</th><th>Current host loading</th><th>Compilation / timing</th><th>Evidence</th></tr>$rows</table>
<p>The bundle includes wasm_of_ocaml GC/reference-type code and a JavaScript loader with bindings, exception tags and string builtins. A separate bounded compiler adapter must implement the actual imported runtime and workspace behavior before compilation can be tested. Node and installed native moonc were not substituted.</p>
<p>Archive: ${os.size(archive)} bytes. Candidate compiler payload: $payloadBytes bytes for moonc Wasm, loader, expanded core and licenses. This is an input inventory, not a working packaged compiler host; adapter size is unavailable. Compressed and expanded core are recorded separately in the inventory.</p>
<p>The release repository revision is $revision; the compiler build suffix is a separate identity. The bundled core has no separate source-revision marker and is content-bound by its archive digest. The receipt retains the moon revision marker and license digests.</p>
<p><a href='receipt.json'>Acquisition, inventories and loading receipts</a> · <a href='surfaces.json'>Complete module imports/exports</a> · <a href='LICENSE.TXT'>Compiler license</a> · <a href='CORE-LICENSE'>Core license</a> · <a href='https://www.moonbitlang.com/blog/moonbit-wasm-compiler'>Official runtime-hosting explanation</a></p></html>"""
    )
