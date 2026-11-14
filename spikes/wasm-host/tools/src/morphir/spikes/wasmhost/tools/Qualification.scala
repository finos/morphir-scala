package morphir.spikes.wasmhost.tools

import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path
import java.util.concurrent.{Executors, TimeUnit}
import scala.jdk.CollectionConverters.*

/** Independent semantic assertions over the experimental executable's public MEP transport. */
object Qualification:
  private def qualify(
      config: ujson.Value,
      settings: ujson.Value,
      guest: os.Path,
      kind: String,
      output: os.Path
  ): ujson.Obj =
    val id      = s"${config("id").str}-$kind"
    val stderr  = output / s"$id.stderr.txt"
    val builder =
      new ProcessBuilder((config("command").arr.map(_.str).toVector ++ Seq("mep", "--guest", guest.toString)).asJava)
        .redirectError(stderr.toIO)
    val _ = builder.environment().put("JAVA_HOME", settings("javaHome").str)
    if config("backend").str == "extism-ffm" then
      val library = os.Path(config("nativeLibrary").str)
      val _       = builder.environment().put("MORPHIR_EXTISM_LIBRARY", library.toString)
      val _       = builder.environment().put("MORPHIR_EXTISM_SHA256", Distribution.hash(library))
    val process                          = builder.start()
    val reader                           = BufferedReader(InputStreamReader(process.getInputStream, UTF_8))
    val writer                           = OutputStreamWriter(process.getOutputStream, UTF_8)
    val executor                         = Executors.newSingleThreadExecutor()
    val exchanges                        = scala.collection.mutable.ArrayBuffer.empty[ujson.Value]
    var sequence                         = 0
    def send(request: ujson.Value): Unit =
      writer.write(request.render() + "\n")
      writer.flush()
    def request(method: String, params: ujson.Value): ujson.Value =
      sequence += 1
      val request = ujson.Obj("jsonrpc" -> "2.0", "id" -> sequence, "method" -> method, "params" -> params)
      send(request)
      val line = executor.submit(new java.util.concurrent.Callable[String]:
        def call() = reader.readLine()).get(30, TimeUnit.SECONDS)
      require(line != null, "guest transport closed before response")
      val reply = ujson.read(line)
      exchanges += ujson.Obj("request" -> request, "response" -> reply)
      require(reply("id") == request("id"), "request identity mismatch")
      require(!reply.obj.contains("error"), s"guest error: ${reply.render()}")
      reply("result")
    def notify(method: String): Unit = send(ujson.Obj("jsonrpc" -> "2.0", "method" -> method, "params" -> ujson.Obj()))
    val result                       = try
      val describe    = request("morphir.extension.describe", ujson.Obj("protocolVersions" -> ujson.Arr("0.1")))
      val initialized = request(
        "morphir.initialize",
        ujson.Obj(
          "protocolVersions" -> ujson.Arr("0.1"),
          "host"             -> ujson.Obj("name" -> "morphir-scala-spike", "version" -> "0.1.0")
        )
      )
      require(initialized("protocolVersion").str == "0.1", "unexpected negotiated protocol")
      notify("morphir.initialized")
      val language = if kind == "sdk" then "morphir-fixture-flags" else "python"
      val source   = if kind == "sdk" then "ready = true\nblocked = false\n"
      else "from dataclasses import dataclass\n\n@dataclass(frozen=True)\nclass Item:\n    value: int\n"
      val extension  = if kind == "sdk" then "flags" else "py"
      val parameters = ujson.Obj(
        "languageId" -> language,
        "sources"    -> ujson.Obj(
          "root"      -> "file:///workspace",
          "documents" -> ujson.Arr(ujson.Obj(
            "uri"        -> s"main.$extension",
            "languageId" -> language,
            "version"    -> 1,
            "text"       -> source
          ))
        ),
        "package"      -> ujson.Obj("name" -> "acme", "exposedModules" -> ujson.Arr("main")),
        "dependencies" -> ujson.Arr(),
        "options"      -> ujson.Obj("irVersion" -> "3", "typesOnly" -> false)
      )
      val compiled = request("morphir.frontend.compile", parameters)
      require(compiled("success").bool && compiled("ir")("formatVersion").num == 3, "compile did not produce IR 3")
      if kind == "sdk" then
        val diagnostic =
          compiled("diagnostics").arr.find(d => d.obj.get("code").contains(ujson.Str("fixture.host"))).getOrElse(
            throw IllegalStateException("guest did not report host services")
          )
        val host = ujson.read(diagnostic("message").str)
        Seq("cacheRoundTrip", "morphirLog", "wasiClock", "wasiRandomSeed", "workspaceRoot").foreach(key =>
          require(host(key).bool, s"host service failed: $key")
        )
        require(host("wasiArguments").num == 0 && host("wasiEnvironment").num == 0, "WASI authority escaped")
      (1 to 3).foreach { _ =>
        require(
          request("morphir.frontend.compile", parameters)("ir") == compiled("ir"),
          "repeated compile changed semantic output"
        )
      }
      val generated = request(
        "morphir.backend.generate",
        ujson.Obj("ir" -> compiled("ir"), "target" -> language, "options" -> ujson.Obj())
      )
      require(generated("success").bool && generated("artifacts").arr.nonEmpty, "generate failed")
      val text = generated("artifacts").arr.map(_("content").str).mkString("\n")
      require(
        text.contains(if kind == "sdk" then "ready = true" else "value: int"),
        "generated semantic output mismatch"
      )
      val _ = request("morphir.shutdown", ujson.Obj())
      notify("morphir.exit")
      try writer.close()
      catch case _: java.io.IOException => ()
      require(process.waitFor(5, TimeUnit.SECONDS) && process.exitValue() == 0, "guest did not exit cleanly")
      ujson.Obj(
        "status"          -> "pass",
        "protocol"        -> "0.1",
        "description"     -> describe,
        "compileGenerate" -> true,
        "repeatedCalls"   -> 3,
        "modelTier"       -> "not run"
      )
    catch
      case error: Exception => ujson.Obj(
          "status"    -> "fail",
          "error"     -> error.toString,
          "modelTier" -> "not run",
          "stderr"    -> stderr.last
        )
    finally
      if process.isAlive then
        val _ = process.destroyForcibly()
        require(process.waitFor(5, TimeUnit.SECONDS), "qualification worker survived forced termination")
      reader.close()
      try writer.close()
      catch case _: java.io.IOException => ()
      process.getErrorStream.close()
      val _ = executor.shutdownNow()
      require(executor.awaitTermination(2, TimeUnit.SECONDS), "qualification read thread survived teardown")
    result("configuration") = config("id")
    result("guest") = kind
    result("guestSha256") = Distribution.hash(guest)
    result("exchanges") = ujson.Arr.from(exchanges)
    os.write(output / s"$id.json", result.render(indent = 2))
    result

  def main(args: Array[String]): Unit =
    val settings = ujson.read(os.read(os.Path(Path.of(args(0)).toAbsolutePath)))
    val output   = os.Path(Path.of(args(1)).toAbsolutePath)
    os.makeDir.all(output)
    val guests   = os.Path(settings("guests").str)
    val receipts =
      for
        config <- settings("configurations").arr
        kind   <- Seq("sdk", "python")
      yield qualify(config, settings, guests / s"$kind.wasm", kind, output)
    os.write(output / "qualification.json", ujson.Arr.from(receipts).render(indent = 2))
