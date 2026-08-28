package millbuild

import java.nio.charset.StandardCharsets.UTF_8

object MepNativeImageSmoke {
  def verify(executable: os.Path, expectedVersion: String): Unit = {
    require(os.isFile(executable), s"MEP native executable does not exist: $executable")
    val runtimeVersion = s"$expectedVersion-runtime-must-not-win"

    val result = os.proc(executable).call(
      stdin = requests.iterator.flatMap(frame).toArray,
      stdout = os.Pipe,
      stderr = os.Pipe,
      env = Map(
        "PATH"                    -> "/usr/bin:/bin",
        "MORPHIR_ELM_MEP_VERSION" -> runtimeVersion
      ),
      propagateEnv = false,
      check = false
    )

    require(result.exitCode == 0, s"MEP native smoke exited ${result.exitCode}: ${result.err.text()}")

    val responses = decodeFrames(result.out.bytes).map(bytes => ujson.read(bytes))
    require(responses.size == 6, s"expected 6 framed MEP responses, received ${responses.size}")
    val byId = responses.map(response => response("id").str -> response).toMap

    verifyMetadata(byId("init")("result")("extension"), expectedVersion, runtimeVersion)
    verifyMetadata(byId("info")("result"), expectedVersion, runtimeVersion)
    require(byId("caps")("result")("frontend")("compile").bool, "frontend compile capability is false")

    val valid = byId("valid")("result")
    require(valid("success").bool, "valid Elm source did not compile")
    require(valid("irVersion").str == "3", s"unexpected IR version: ${valid("irVersion")}")
    require(valid("modules").arr.map(_.str) == Seq("Example"), s"unexpected compiled modules: ${valid("modules")}")
    require(valid("ir").isInstanceOf[ujson.Obj], "valid Elm compile did not return an IR object")

    val invalid = byId("invalid")("result")
    require(!invalid("success").bool, "invalid Elm source unexpectedly compiled")
    require(invalid("modules").arr.isEmpty, s"invalid Elm compile returned modules: ${invalid("modules")}")
    require(
      invalid("diagnostics").arr.exists(_("code").str == "elm.parser"),
      s"invalid Elm compile did not return elm.parser: ${invalid("diagnostics")}"
    )
    require(byId("shutdown")("result") == ujson.Obj(), s"unexpected shutdown result: ${byId("shutdown")}")
  }

  private def verifyMetadata(metadata: ujson.Value, expectedVersion: String, runtimeVersion: String): Unit = {
    require(metadata("id").str == "morphir-scala-elm", s"unexpected provider ID: ${metadata("id")}")
    require(
      metadata("name").str == "Morphir Scala Elm frontend",
      s"unexpected provider name: ${metadata("name")}"
    )
    require(
      metadata("version").str == expectedVersion,
      s"expected compiled provider version $expectedVersion, received ${metadata("version")}"
    )
    require(metadata("version").str != runtimeVersion, "runtime environment changed compiled provider metadata")
  }

  private def requests: Seq[ujson.Value] = Seq(
    request(
      "init",
      "morphir.initialize",
      ujson.Obj(
        "protocolVersions" -> ujson.Arr("0.1"),
        "host"             -> ujson.Obj("name" -> "native-smoke", "version" -> "1.0.0")
      )
    ),
    request("info", "morphir.extension.info", ujson.Obj()),
    request("caps", "morphir.extension.capabilities", ujson.Obj()),
    request(
      "valid",
      "morphir.frontend.compile",
      compileParams(
        uri = "file:///workspace/Example.elm",
        module = "Example",
        source =
          """module Example exposing (add)
            |
            |add : Int -> Int -> Int
            |add left right =
            |    left + right
            |""".stripMargin
      )
    ),
    request(
      "invalid",
      "morphir.frontend.compile",
      compileParams(
        uri = "file:///workspace/Broken.elm",
        module = "Broken",
        source =
          """module Broken exposing (value)
            |
            |value : Int
            |value =
            |""".stripMargin
      )
    ),
    request("shutdown", "morphir.shutdown", ujson.Obj())
  )

  private def request(id: String, method: String, params: ujson.Obj): ujson.Obj = ujson.Obj(
    "jsonrpc" -> "2.0",
    "id"      -> id,
    "method"  -> method,
    "params"  -> params
  )

  private def compileParams(uri: String, module: String, source: String): ujson.Obj = ujson.Obj(
    "languageId" -> "elm",
    "documents"  -> ujson.Arr(ujson.Obj(
      "uri"        -> uri,
      "languageId" -> "elm",
      "version"    -> 1,
      "text"       -> source
    )),
    "package"      -> ujson.Obj("name" -> s"local/${module.toLowerCase}", "exposedModules" -> ujson.Arr(module)),
    "dependencies" -> ujson.Arr(),
    "options"      -> ujson.Obj("typesOnly" -> false, "irVersion" -> "3")
  )

  private def frame(json: ujson.Value): Array[Byte] = {
    val body   = ujson.write(json).getBytes(UTF_8)
    val header = s"Content-Length: ${body.length}\r\n\r\n".getBytes(UTF_8)
    header ++ body
  }

  private def decodeFrames(output: Array[Byte]): Vector[Array[Byte]] = {
    val delimiter = "\r\n\r\n".getBytes(UTF_8)

    def delimiterAt(index: Int): Boolean =
      index + delimiter.length <= output.length &&
        delimiter.indices.forall(offset => output(index + offset) == delimiter(offset))

    @annotation.tailrec
    def loop(offset: Int, frames: Vector[Array[Byte]]): Vector[Array[Byte]] =
      if offset == output.length then frames
      else {
        val headerEnd = (offset until output.length).find(delimiterAt).getOrElse {
          throw new IllegalArgumentException(s"MEP stdout contains an incomplete or unframed response at byte $offset")
        }
        val header = String(output.slice(offset, headerEnd), UTF_8)
        val length = header.stripPrefix("Content-Length: ").toIntOption.filter(_ >= 0).getOrElse {
          throw new IllegalArgumentException(s"MEP stdout contains an invalid frame header: $header")
        }
        require(header == s"Content-Length: $length", s"MEP stdout contains non-frame data: $header")
        val bodyStart = headerEnd + delimiter.length
        val bodyEnd   = bodyStart + length
        require(bodyEnd <= output.length, s"MEP stdout frame body is truncated: expected $length bytes")
        loop(bodyEnd, frames :+ output.slice(bodyStart, bodyEnd))
      }

    loop(0, Vector.empty)
  }
}
