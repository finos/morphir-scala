package morphir.spikes.wasmhost

import java.nio.charset.StandardCharsets.UTF_8

/** Repeated representative legacy work. Independent qualification asserts the richer semantic results. */
private[wasmhost] final class MepWorkload(session: GuestSession, kind: String):
  require(kind == "sdk" || kind == "python", "guest kind must be sdk or python")
  private var sequence  = 0
  private val language  = if kind == "sdk" then "morphir-fixture-flags" else "python"
  private val extension = if kind == "sdk" then "flags" else "py"
  private val source    = if kind == "sdk" then "ready = true\nblocked = false\n"
  else "from dataclasses import dataclass\n\n@dataclass(frozen=True)\nclass Item:\n    value: int\n"
  private def request(method: String, params: ujson.Value): ujson.Value =
    sequence += 1
    val payload = ujson.Obj("jsonrpc" -> "2.0", "id" -> sequence, "method" -> method, "params" -> params)
    val reply   = session.handle(payload.render().getBytes(UTF_8))
    require(reply.status > 0, "MEP request returned no response")
    val response = ujson.read(MepWorker.utf8(reply.bytes))
    require(
      response("id").num == sequence && !response.obj.contains("error"),
      s"MEP request failed: ${response.render()}"
    )
    response("result")
  private val initialized = request(
    "morphir.initialize",
    ujson.Obj(
      "protocolVersions" -> ujson.Arr("0.1"),
      "host"             -> ujson.Obj("name" -> "morphir-scala-spike", "version" -> "0.1.0")
    )
  )
  require(initialized("protocolVersion").str == "0.1", "unexpected MEP negotiation")
  private val parameters = ujson.Obj(
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

  def compile(): Unit =
    val result = request("morphir.frontend.compile", parameters)
    require(result("success").bool && result("ir")("formatVersion").num == 3, "MEP compile failed")
  def finish(): Unit =
    val _ = request("morphir.shutdown", ujson.Obj())
