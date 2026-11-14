package morphir.spikes.wasmhost.tools

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Path
import java.security.MessageDigest

/** Runs the independent upstream guest checker; its scope is narrower than independent host qualification. */
object Mck:
  def main(args: Array[String]): Unit =
    val settings = ujson.read(os.read(os.Path(Path.of(args(0)).toAbsolutePath)))
    val output   = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val source = "9690dfc3761e1cbe1cd3d6fa327a5e082b9892ac"
    val client = HttpClient.newBuilder().followRedirects(HttpClient.Redirect.NORMAL).build()
    require(
      System.getProperty("os.name") == "Mac OS X" && System.getProperty("os.arch") == "aarch64",
      "the spike checker artifact is pinned for macOS arm64"
    )
    val checkerUrl =
      "https://github.com/finos/morphir/releases/download/v0.4.0-beta.12/morphir-0.4.0-beta.12-aarch64-apple-darwin.tgz"
    val checkerArchive = output / "checker.tar.gz"
    val downloaded     =
      client.send(HttpRequest.newBuilder(URI.create(checkerUrl)).GET().build(), HttpResponse.BodyHandlers.ofByteArray())
    require(downloaded.statusCode() == 200, "checker acquisition failed")
    os.write(checkerArchive, downloaded.body())
    require(
      Distribution.hash(checkerArchive) == "61c25b33782122570f4189cfb42ae701982d89698950135d8d5d4029d187fd38",
      "checker archive digest mismatch"
    )
    os.proc("tar", "-xzf", checkerArchive, "-C", output, "morphir").call()
    val checker = output / "morphir"
    os.perms.set(checker, "rwxr-xr-x")
    val kits = Seq(
      "sdk"    -> ("guest-generic-json.json", "c2a795167773b187b8322785f75f4667aa0b8dcf"),
      "python" -> ("guest-python-json.json", "20392bb378abf50b70c0d48279db48d6dab89dff")
    ).map { case (kind, (name, blob)) =>
      val url      = s"https://raw.githubusercontent.com/finos/morphir/$source/spec/mep/mck/$name"
      val response =
        client.send(HttpRequest.newBuilder(URI.create(url)).GET().build(), HttpResponse.BodyHandlers.ofByteArray())
      require(response.statusCode() == 200, s"MCK corpus acquisition failed: $name")
      val bytes    = response.body()
      val identity = MessageDigest.getInstance(
        "SHA-1"
      ).digest(s"blob ${bytes.length}\u0000".getBytes(java.nio.charset.StandardCharsets.UTF_8) ++ bytes)
        .map(b => f"${b & 255}%02x").mkString
      require(identity == blob, "MCK corpus git object mismatch")
      val file = output / name
      os.write(file, bytes)
      kind -> file
    }.toMap
    val receipts =
      for
        config <- settings("configurations").arr
        kind   <- Seq("sdk", "python")
      yield
        val id         = s"${config("id").str}-$kind"
        val guest      = os.Path(settings("guests").str) / s"$kind.wasm"
        val kit        = kits(kind)
        val report     = output / s"$id.report.json"
        val host       = config("command")(0).str
        val hostArgs   = Seq("mep", "--framed", "--guest", guest.toString).flatMap(arg => Seq("--host-arg", arg))
        val identities =
          config("files").arr.map(file => (os.Path(host) / os.up / os.RelPath(file("path").str)).toString).toVector ++
            Seq(guest.toString, (os.Path(settings("javaHome").str) / "bin" / "java").toString)
        val environment = Map("JAVA_HOME" -> settings("javaHome").str) ++
          (if config("backend").str == "extism-ffm" then
             Map(
               "MORPHIR_EXTISM_LIBRARY" -> config("nativeLibrary").str,
               "MORPHIR_EXTISM_SHA256"  -> Distribution.hash(os.Path(config("nativeLibrary").str))
             )
           else Map.empty)
        val command = Seq(
          checker.toString,
          "mck",
          "mep",
          "guest-run",
          "--kit",
          kit.toString,
          "--expect-kit-sha256",
          Distribution.hash(kit),
          "--host",
          host
        ) ++ hostArgs ++
          identities.flatMap(file => Seq("--identity-file", file)) ++ Seq("--report", report.toString)
        val run = os.proc(command).call(
          env = environment,
          timeout = 180000,
          check = false,
          stdout = output / s"$id.run.stdout",
          stderr = output / s"$id.run.stderr"
        )
        val checked = os.proc(checker, "mck", "mep", "report-check", "--kit", kit, report)
          .call(
            timeout = 30000,
            check = false,
            stdout = output / s"$id.check.stdout",
            stderr = output / s"$id.check.stderr"
          )
        ujson.Obj(
          "configuration"        -> config("id"),
          "guest"                -> kind,
          "checkerSha256"        -> Distribution.hash(checker),
          "checkerRelease"       -> "0.4.0-beta.12",
          "checkerArchiveSha256" -> Distribution.hash(checkerArchive),
          "corpusSourceRevision" -> source,
          "corpusSha256"         -> Distribution.hash(kit),
          "command"              -> ujson.Arr.from(command),
          "runExit"              -> run.exitCode,
          "reportCheckExit"      -> checked.exitCode,
          "status"               -> (if run.exitCode == 0 && checked.exitCode == 0 then "pass" else "fail"),
          "scope"                -> "upstream bounded guest profile, not independent full host conformance"
        )
    os.write(output / "mck.json", ujson.Arr.from(receipts).render(indent = 2))
