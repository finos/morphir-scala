package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*

class MepSessionTests extends Test[Any]:
  private type Value = Structure.Value

  private val emptyParams      = record()
  private val initializeParams = Structure.encode(
    InitializeRequest(Chunk("0.1"), HostMetadata("test-host", "1.0.0"))
  )
  private val elmSource =
    """module Example exposing (add)
      |
      |add : Int -> Int -> Int
      |add left right = left + right
      |""".stripMargin
  private val compileParams = Structure.encode(
    CompileRequest(
      "elm",
      Chunk(SourceDocument("file:///workspace/Example.elm", "elm", DocumentVersion(1), elmSource)),
      CompilePackage("local/example", Chunk("Example")),
      Chunk.empty,
      CompileOptions(typesOnly = false, irVersion = "3")
    )
  )

  private def record(fields: (String, Value)*): Value = Structure.Value.Record(Chunk.from(fields))

  private def request(id: JsonRpcId, method: String, params: Value = emptyParams): String =
    val envelope: JsonRpcEnvelope = JsonRpcRequest(id, method, Present(params), Absent)
    Json.encode(envelope)

  private def notification(method: String, params: Value = emptyParams): String =
    val envelope: JsonRpcEnvelope = JsonRpcNotification(method, Present(params), Absent)
    Json.encode(envelope)

  private def nullIdRequest(method: String, params: Value = emptyParams): String =
    Json.encode(
      record(
        "jsonrpc" -> Structure.Value.Str("2.0"),
        "id"      -> Structure.Value.Null,
        "method"  -> Structure.Value.Str(method),
        "params"  -> params
      )
    )

  private def value(transition: SessionTransition): Value =
    Json.decode[Value](transition.response.get) match
      case Result.Success(value) => value
      case other                 => throw AssertionError(s"response did not decode: $other")

  private def at(value: Value, path: String*): Option[Value] =
    path.foldLeft(Option(value)) {
      case (Some(Structure.Value.Record(fields)), field) => fields.iterator.toMap.get(field)
      case _                                             => None
    }

  private def initializedSession(provider: ProviderMetadata = ProviderMetadata.default): MepSession =
    MepSession.loaded(provider).handle(request(JsonRpcId(1), "morphir.initialize", initializeParams)).session

  "MepSession" - {
    "uses Kyo JSON-RPC envelopes and rejects malformed wire shapes" in {
      val session = MepSession.loaded(ProviderMetadata.default)
      val bodies  = Vector(
        "[]",
        "1",
        """{"id":"missing-version","method":"morphir.initialize","params":{}}""",
        """{"jsonrpc":"1.0","id":"wrong-version","method":"morphir.initialize","params":{}}""",
        """{"jsonrpc":"2.0","id":20,"params":{}}""",
        """{"jsonrpc":"2.0","id":21,"method":1,"params":{}}""",
        """{"jsonrpc":"2.0","id":1.5,"method":"morphir.initialize","params":{}}""",
        """{"jsonrpc":"2.0","id":{},"method":"morphir.initialize","params":{}}"""
      )

      val responses = bodies.map(body => value(session.handle(body)))

      assert(responses.forall(at(_, "error", "code").contains(Structure.Value.Integer(-32600))))
      assert(at(responses(2), "id").contains(Structure.Value.Str("missing-version")))
      assert(at(responses(3), "id").contains(Structure.Value.Str("wrong-version")))
      assert(at(responses(6), "id").contains(Structure.Value.Null))
      assert(at(responses(7), "id").contains(Structure.Value.Null))
    }

    "returns a Kyo JSON-RPC parse error for malformed JSON" in {
      val response = value(MepSession.loaded(ProviderMetadata.default).handle("{"))

      assert(at(response, "id").contains(Structure.Value.Null))
      assert(at(response, "error", "code").contains(Structure.Value.Integer(-32700)))
    }

    "preserves an explicit null request id and returns a response" in {
      val transition = MepSession.loaded(ProviderMetadata.default).handle(
        nullIdRequest("morphir.initialize", initializeParams)
      )
      val response = value(transition)

      assert(transition.session.state == SessionState.Ready)
      assert(at(response, "id").contains(Structure.Value.Null))
      assert(at(response, "result", "protocolVersion").contains(Structure.Value.Str("0.1")))
    }

    "negotiates MEP 0.1 and reports provider metadata and capabilities" in {
      val provider    = Main.providerMetadata("morphir-scala-elm", "Morphir Scala Elm frontend", "9.8.7")
      val initialized = MepSession.loaded(provider).handle(
        request(JsonRpcId("init"), "morphir.initialize", initializeParams)
      )
      val initialize = value(initialized)
      val info       = value(initialized.session.handle(
        request(JsonRpcId("info"), "morphir.extension.info")
      ))
      val capabilities = value(initialized.session.handle(
        request(JsonRpcId(42), "morphir.extension.capabilities")
      ))

      assert(initialized.session.state == SessionState.Ready)
      assert(at(initialize, "result", "protocolVersion").contains(Structure.Value.Str("0.1")))
      assert(at(initialize, "result", "extension", "version").contains(Structure.Value.Str("9.8.7")))
      assert(at(info, "result", "id").contains(Structure.Value.Str("morphir-scala-elm")))
      assert(at(info, "result", "version").contains(Structure.Value.Str("9.8.7")))
      assert(at(capabilities, "result", "frontend", "compile").contains(Structure.Value.Bool(true)))
      assert(at(capabilities, "result", "streaming").contains(Structure.Value.Bool(false)))
    }

    "responds to ping before initialization and validates object parameters" in {
      val session = MepSession.loaded(ProviderMetadata.default)
      val pong    = value(session.handle(request(JsonRpcId("ping"), "morphir.ping")))
      val invalid = value(session.handle(
        request(JsonRpcId(2), "morphir.ping", Structure.Value.Null)
      ))

      assert(at(pong, "result", "ok").contains(Structure.Value.Bool(true)))
      assert(at(invalid, "error", "code").contains(Structure.Value.Integer(-32602)))
    }

    "supports initialize notifications and rejects incompatible or malformed initialization" in {
      val loaded       = MepSession.loaded(ProviderMetadata.default)
      val notified     = loaded.handle(notification("morphir.initialize", initializeParams))
      val incompatible = loaded.handle(
        request(
          JsonRpcId(7),
          "morphir.initialize",
          Structure.encode(InitializeRequest(Chunk("9.0"), HostMetadata("test-host", "1.0.0")))
        )
      )
      val malformed = loaded.handle(
        request(JsonRpcId(8), "morphir.initialize", record("protocolVersions" -> Structure.Value.Sequence(Chunk.empty)))
      )

      assert(notified.session.state == SessionState.Ready)
      assert(notified.response.isEmpty)
      assert(at(value(incompatible), "error", "code").contains(Structure.Value.Integer(-32011)))
      assert(at(value(malformed), "error", "code").contains(Structure.Value.Integer(-32602)))
    }

    "enforces initialization and method lifecycle" in {
      val loaded  = MepSession.loaded(ProviderMetadata.default)
      val before  = value(loaded.handle(request(JsonRpcId(1), "morphir.frontend.compile", compileParams)))
      val ready   = initializedSession()
      val repeat  = value(ready.handle(request(JsonRpcId(2), "morphir.initialize", initializeParams)))
      val unknown = value(ready.handle(request(JsonRpcId(3), "morphir.unknown")))

      assert(at(before, "error", "code").contains(Structure.Value.Integer(-32600)))
      assert(at(repeat, "error", "message").contains(Structure.Value.Str("The MEP session is already initialized")))
      assert(at(unknown, "error", "code").contains(Structure.Value.Integer(-32601)))
      assert(ready.handle(notification("morphir.initialized")).response.isEmpty)
      assert(ready.handle(notification("morphir.unknown")).response.isEmpty)
    }

    "compiles Elm through the Kyo code model and embeds a v3 wire projection" in {
      val response = value(initializedSession().handle(
        request(JsonRpcId("compile"), "morphir.frontend.compile", compileParams)
      ))

      assert(at(response, "result", "success").contains(Structure.Value.Bool(true)))
      assert(at(response, "result", "irVersion").contains(Structure.Value.Str("3")))
      assert(at(response, "result", "ir").exists(_.isInstanceOf[Structure.Value.Record]))
      assert(
        at(response, "result", "modules").contains(
          Structure.Value.Sequence(Chunk(Structure.Value.Str("Example")))
        )
      )
      assert(at(response, "result", "ir", "formatVersion").contains(Structure.Value.Integer(3)))
    }

    "returns compiler diagnostics as normal compile results with caller locations" in {
      val badSource = "module Example exposing (add)\n\nadd = \\value -> value\n"
      val badParams = Structure.encode(
        CompileRequest(
          "elm",
          Chunk(SourceDocument("file:///workspace/Example.elm", "elm", DocumentVersion(1), badSource)),
          CompilePackage("local/example", Chunk("Example")),
          Chunk.empty,
          CompileOptions(typesOnly = false, irVersion = "3")
        )
      )
      val response = value(initializedSession().handle(
        request(JsonRpcId(9), "morphir.frontend.compile", badParams)
      ))

      assert(at(response, "result", "success").contains(Structure.Value.Bool(false)))
      assert(at(response, "result", "diagnostics").exists {
        case Structure.Value.Sequence(values) => values.nonEmpty
        case _                                => false
      })
    }

    "maps invalid compile params and compiler panic to JSON-RPC errors" in {
      val invalid = value(initializedSession().handle(
        request(JsonRpcId(10), "morphir.frontend.compile", record("languageId" -> Structure.Value.Str("elm")))
      ))
      val panicSession = MepSession.loaded(
        ProviderMetadata.default,
        _ => Result.panic(IllegalStateException("synthetic compiler panic"))
      ).handle(request(JsonRpcId(1), "morphir.initialize", initializeParams)).session
      val panic = value(panicSession.handle(
        request(JsonRpcId(11), "morphir.frontend.compile", compileParams)
      ))

      assert(at(invalid, "error", "code").contains(Structure.Value.Integer(-32602)))
      assert(at(panic, "error", "code").contains(Structure.Value.Integer(-32603)))
      assert(at(panic, "error", "message").contains(Structure.Value.Str("Internal error")))
    }

    "executes notifications without responses, including compile panics" in {
      val ready      = initializedSession()
      val panicReady = MepSession.loaded(
        ProviderMetadata.default,
        _ => Result.panic(IllegalStateException("synthetic compiler panic"))
      ).handle(request(JsonRpcId(1), "morphir.initialize", initializeParams)).session

      assert(ready.handle(notification("morphir.frontend.compile", compileParams)).response.isEmpty)
      assert(panicReady.handle(notification("morphir.frontend.compile", compileParams)).response.isEmpty)
      assert(ready.handle(notification("morphir.extension.info")).response.isEmpty)
    }

    "shuts down, waits for exit, and accepts only the terminating notification" in {
      val ready      = initializedSession()
      val shutdown   = ready.handle(request(JsonRpcId(12), "morphir.shutdown"))
      val rejected   = shutdown.session.handle(request(JsonRpcId(13), "morphir.frontend.compile", compileParams))
      val terminated = shutdown.session.handle(notification("morphir.exit"))

      assert(shutdown.session.state == SessionState.AwaitExit)
      assert(at(value(shutdown), "result").contains(record()))
      assert(at(value(rejected), "error", "code").contains(Structure.Value.Integer(-32600)))
      assert(terminated.session.state == SessionState.Stopped)
      assert(terminated.response.isEmpty)
    }

    "accepts exit before initialization only as a notification" in {
      val loaded      = MepSession.loaded(ProviderMetadata.default)
      val terminated  = loaded.handle(notification("morphir.exit"))
      val exitRequest = value(initializedSession().handle(request(JsonRpcId(14), "morphir.exit")))

      assert(terminated.session.state == SessionState.Stopped)
      assert(terminated.response.isEmpty)
      assert(at(exitRequest, "error", "message").contains(Structure.Value.Str("morphir.exit is a notification")))
    }
  }
end MepSessionTests
