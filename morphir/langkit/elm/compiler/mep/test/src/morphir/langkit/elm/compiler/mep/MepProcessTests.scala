package morphir.langkit.elm.compiler.mep

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets.UTF_8
import kyo.test.*

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
      val decoded  = MepFrameCodec.decoder().feed(stdout.toByteArray).toOption.get

      assert(exitCode == 0)
      assert(decoded.frames.size == 2)
      assert(decoded.decoder.finish == Right(()))
      assert(decoded.frames.forall(frame => String(frame, UTF_8).startsWith("{\"jsonrpc\":\"2.0\"")))
      assert(stderr.size == 0)
    }
  }
