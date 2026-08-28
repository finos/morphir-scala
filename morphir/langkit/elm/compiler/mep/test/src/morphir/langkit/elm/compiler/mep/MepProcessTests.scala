package morphir.langkit.elm.compiler.mep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import kyo.test.*
import zio.json.*
import zio.json.ast.Json

class MepProcessTests extends Test[Any]:

  "MepProcess" - {
    "writes only framed JSON-RPC to stdout and exits after shutdown" in {
      val initialize =
        """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      val shutdown = """{"jsonrpc":"2.0","id":2,"method":"morphir.shutdown","params":{}}"""
      val stdin    = ByteArrayInputStream(MepFrameCodec.encodeJson(initialize) ++ MepFrameCodec.encodeJson(shutdown))
      val stdout   = ByteArrayOutputStream()
      val stderr   = ByteArrayOutputStream()

      val exitCode = MepProcess.run(stdin, stdout, PrintStream(stderr), ProviderMetadata.default)
      val decoder  = MepFrameCodec.decoder()
      val decoded  = decoder.feed(stdout.toByteArray)

      assert(exitCode == 0)
      assert(decoded.frames.size == 2)
      assert(decoder.finish == Right(()))
      assert(decoded.frames.forall(frame => String(frame, UTF_8).startsWith("{\"jsonrpc\":\"2.0\"")))
      assert(stderr.size == 0)
    }

    "returns a parse error for malformed UTF-8 request bodies" in {
      val prefix =
        """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"bad""".getBytes(
          UTF_8
        )
      val suffix = """","version":"1.0.0"}}}""".getBytes(UTF_8)
      val body   = prefix ++ Array(0xc3.toByte) ++ suffix
      val stdin  = ByteArrayInputStream(MepFrameCodec.encode(body))
      val stdout = ByteArrayOutputStream()
      val stderr = ByteArrayOutputStream()

      val exitCode  = MepProcess.run(stdin, stdout, PrintStream(stderr), ProviderMetadata.default)
      val response  = MepFrameCodec.decoder().feed(stdout.toByteArray).frames.head
      val json      = String(response, UTF_8).fromJson[Json].toOption.get
      val errorCode = json match
        case Json.Obj(fields) => fields.toMap.get("error").collect {
            case Json.Obj(error) => error.toMap.get("code")
          }.flatten
        case _ => None

      assert(exitCode == 0)
      assert(errorCode == Some(Json.Num(-32700)))
      assert(stderr.size == 0)
    }

    "writes completed responses before reporting a trailing framing error" in {
      val initialize =
        """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      val malformed = "Content-Length: nope\r\n\r\n".getBytes(UTF_8)
      val stdin     = ByteArrayInputStream(MepFrameCodec.encodeJson(initialize) ++ malformed)
      val stdout    = ByteArrayOutputStream()
      val stderr    = ByteArrayOutputStream()

      val exitCode = MepProcess.run(stdin, stdout, PrintStream(stderr), ProviderMetadata.default)
      val decoded  = MepFrameCodec.decoder().feed(stdout.toByteArray)

      assert(exitCode == 1)
      assert(decoded.frames.size == 1)
      assert(String(decoded.frames.head, UTF_8).contains("\"protocolVersion\":\"0.1\""))
      assert(String(stderr.toByteArray, UTF_8).contains("invalid Content-Length"))
    }
  }
