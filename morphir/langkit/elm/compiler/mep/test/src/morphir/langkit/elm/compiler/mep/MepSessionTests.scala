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
    "preserves the exact initialize and capabilities wire output" in {
      val caps =
        """{"frontend":{"languages":[{"id":"elm","fileExtensions":[".elm"]}],"irVersions":["3"],"compile":true,"incremental":false,"fragments":false,"multiDocument":false},"workspace":{"protocolVersions":["0.1.0-draft.1"],"discover":true},"streaming":false,"incremental":false,"cancellation":false,"progress":false}"""
      val identity =
        """{"id":"morphir-scala-elm","name":"Morphir Scala Elm frontend","version":"9.8.7","types":["frontend","workspace"]}"""
      val initialized = MepSession.loaded(ProviderMetadata.default.copy(version = "9.8.7"))
        .handle(request(JsonRpcId(1), "morphir.initialize", initializeParams))
      assert(initialized.response.get ==
        s"""{"jsonrpc":"2.0","id":1,"result":{"protocolVersion":"0.1","extension":$identity,"capabilities":$caps}}""")
      assert(initialized.session.handle(request(JsonRpcId(2), "morphir.extension.capabilities")).response.get ==
        s"""{"jsonrpc":"2.0","id":2,"result":$caps}""")
    }

    "describes without changing state and agrees with all four session rules" in {
      val loaded      = MepSession.loaded(ProviderMetadata.default.copy(version = "9.8.7"))
      val params      = record("protocolVersions" -> Structure.Value.Sequence(Chunk(Structure.Value.Str("0.1"))))
      val described   = loaded.handle(request(JsonRpcId(1), "morphir.extension.describe", params))
      val initialized = loaded.handle(request(JsonRpcId(2), "morphir.initialize", initializeParams))
      val claims      = at(value(described), "result").get
      val session     = at(value(initialized), "result").get
      val later       = initialized.session.handle(request(JsonRpcId(3), "morphir.extension.describe", params))

      def containsMembers(claimed: Value, reported: Value): Boolean = (claimed, reported) match
        case (Structure.Value.Record(claims), Structure.Value.Record(members)) =>
          members.forall { case (key, member) => claims.toMap.get(key).exists(containsMembers(_, member)) }
        case _ => claimed == reported

      assert(described.session eq loaded)
      assert(later.session eq initialized.session)
      assert(at(value(later), "result").contains(claims))
      assert(at(claims, "claimsVersion").contains(Structure.Value.Str("0.1.0-draft.2")))
      Chunk("id", "name", "version").foreach { field =>
        assert(at(session, "extension", field) == at(claims, "extension", field))
      }
      val Structure.Value.Sequence(protocols) = at(claims, "protocolVersions").get: @unchecked
      assert(protocols.contains(at(session, "protocolVersion").get))
      val Structure.Value.Sequence(types)         = at(claims, "extension", "types").get: @unchecked
      val Structure.Value.Sequence(reportedTypes) = at(session, "extension", "types").get: @unchecked
      assert(reportedTypes.forall(types.contains))
      assert(containsMembers(at(claims, "capabilities").get, at(session, "capabilities").get))
      assert(loaded.handle(notification("morphir.extension.describe", params)).session eq loaded)
      assert(loaded.handle(notification("morphir.extension.describe", params)).response.isEmpty)
    }

    "validates describe parameters and uses the initialize protocol mismatch error" in {
      val loaded      = MepSession.loaded(ProviderMetadata.default)
      val unsupported = record("protocolVersions" -> Structure.Value.Sequence(Chunk(Structure.Value.Str("9.0"))))
      val refused     = loaded.handle(request(JsonRpcId(1), "morphir.extension.describe", unsupported))
      val init        = loaded.handle(request(
        JsonRpcId(1),
        "morphir.initialize",
        Structure.encode(InitializeRequest(Chunk("9.0"), HostMetadata("test", "1")))
      ))
      assert(at(value(refused), "error") == at(value(init), "error"))
      assert(refused.session eq loaded)
      val empty = loaded.handle(request(
        JsonRpcId(1),
        "morphir.extension.describe",
        record("protocolVersions" -> Structure.Value.Sequence(Chunk.empty))
      ))
      assert(at(value(empty), "error", "code").contains(Structure.Value.Integer(-32011)))
      Chunk(
        record(),
        Structure.Value.Null,
        record("protocolVersions" -> Structure.Value.Str("0.1")),
        record("protocolVersions" -> Structure.Value.Sequence(Chunk(Structure.Value.Integer(1))))
      ).foreach { params =>
        val bad = loaded.handle(request(JsonRpcId(1), "morphir.extension.describe", params))
        assert(at(value(bad), "error", "code").contains(Structure.Value.Integer(-32602)))
        assert(bad.session eq loaded)
      }
      val missing = loaded.handle("""{"jsonrpc":"2.0","id":1,"method":"morphir.extension.describe"}""")
      assert(at(value(missing), "error", "code").contains(Structure.Value.Integer(-32602)))
      val shutdown = initializedSession().handle(request(JsonRpcId(2), "morphir.shutdown")).session
      val after    = shutdown.handle(request(JsonRpcId(3), "morphir.extension.describe", unsupported))
      assert(at(value(after), "error", "code").contains(Structure.Value.Integer(-32014)))
      assert(after.session eq shutdown)
    }

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

    "declares workspace discovery and a single-document frontend" in {
      val initialized = initializedSession()
      val initialize  = value(MepSession.loaded(ProviderMetadata.default).handle(
        request(JsonRpcId("init"), "morphir.initialize", initializeParams)
      ))
      val info         = value(initialized.handle(request(JsonRpcId("info"), "morphir.extension.info")))
      val capabilities = value(initialized.handle(request(JsonRpcId("caps"), "morphir.extension.capabilities")))
      val workspace    = record(
        "protocolVersions" -> Structure.Value.Sequence(Chunk(Structure.Value.Str("0.1.0-draft.1"))),
        "discover"         -> Structure.Value.Bool(true)
      )
      val types = Structure.Value.Sequence(Chunk(Structure.Value.Str("frontend"), Structure.Value.Str("workspace")))

      assert(at(initialize, "result", "extension", "types").contains(types))
      assert(at(info, "result", "types").contains(types))
      assert(at(initialize, "result", "capabilities", "workspace").contains(workspace))
      assert(at(capabilities, "result", "workspace").contains(workspace))
      assert(at(capabilities, "result", "frontend", "multiDocument").contains(Structure.Value.Bool(false)))
    }

    "answers workspace discovery for an ad-hoc selection" in {
      val params = Json.decode[Value](
        """{"protocolVersion":"0.1.0-draft.1","developmentRoot":{"entries":{".":{"kind":"directory"},""" +
          """"Example.elm":{"kind":"file","text":"module Example exposing (add)\n"}}},"cliOverlay":{},""" +
          """"purpose":{"kind":"ad-hoc-sources","project":{"kind":"synthesized"},""" +
          """"sources":{"root":".","paths":["Example.elm"]},"languageId":"elm"}}"""
      ).getOrThrow
      val ready    = initializedSession()
      val response = value(ready.handle(request(JsonRpcId("discover"), "morphir.workspace.discover", params)))
      val refused  = value(ready.handle(request(JsonRpcId(2), "morphir.workspace.discover", record())))
      val early    = value(MepSession.loaded(ProviderMetadata.default).handle(
        request(JsonRpcId(3), "morphir.workspace.discover", params)
      ))
      val project = at(response, "result", "snapshot", "projects").collect {
        case Structure.Value.Sequence(Chunk(project)) => project
      }

      assert(at(response, "result", "status").contains(Structure.Value.Str("success")))
      assert(project.flatMap(at(_, "name")).contains(Structure.Value.Str("local/example")))
      assert(at(refused, "error", "code").contains(Structure.Value.Integer(-32602)))
      assert(at(early, "error", "code").contains(Structure.Value.Integer(-32014)))
      assert(ready.handle(notification("morphir.workspace.discover", params)).response.isEmpty)
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

      assert(at(before, "error", "code").contains(Structure.Value.Integer(-32014)))
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

    "compiles modern sources with or without a root exactly like legacy documents" in {
      val ready  = initializedSession()
      val legacy = value(ready.handle(request(JsonRpcId(20), "morphir.frontend.compile", compileParams)))
      val Structure.Value.Record(fields) = compileParams: @unchecked
      val documents                      = at(compileParams, "documents").get
      val roots = Chunk(Chunk.empty[(String, Value)], Chunk("root" -> Structure.Value.Str("file:///another-root/")))

      assert(at(legacy, "result", "success").contains(Structure.Value.Bool(true)))
      roots.foreach { rootFields =>
        val sources  = Structure.Value.Record(rootFields.append("documents" -> documents))
        val modern   = Structure.Value.Record(fields.filter(_._1 != "documents").append("sources" -> sources))
        val response = value(ready.handle(request(JsonRpcId(21), "morphir.frontend.compile", modern)))
        assert(at(response, "result") == at(legacy, "result"))
      }
    }

    "rejects ambiguous and missing compile envelopes with invalid params" in {
      val Structure.Value.Record(fields) = compileParams: @unchecked
      val both                           =
        Structure.Value.Record(fields.append("sources" -> record("documents" -> at(compileParams, "documents").get)))
      val neither = Structure.Value.Record(fields.filter(_._1 != "documents"))
      val cases   = Chunk(
        both    -> "Ambiguous morphir.frontend.compile parameters: provide either sources or documents, not both",
        neither -> "Invalid morphir.frontend.compile parameters: missing sources or documents"
      )
      cases.foreach { case (params, message) =>
        val response = value(initializedSession().handle(request(JsonRpcId(22), "morphir.frontend.compile", params)))
        assert(at(response, "error", "code").contains(Structure.Value.Integer(-32602)))
        assert(at(response, "error", "message").contains(Structure.Value.Str(message)))
      }
    }

    "rejects non-string compile source roots with invalid params" in {
      val Structure.Value.Record(fields) = compileParams: @unchecked
      val invalidRoots = Chunk(Structure.Value.Integer(1), Structure.Value.Null, Structure.Value.Bool(false), record())
      invalidRoots.foreach { root =>
        val sources  = record("root" -> root, "documents" -> at(compileParams, "documents").get)
        val params   = Structure.Value.Record(fields.filter(_._1 != "documents").append("sources" -> sources))
        val response = value(initializedSession().handle(request(JsonRpcId(23), "morphir.frontend.compile", params)))
        assert(at(response, "error", "code").contains(Structure.Value.Integer(-32602)))
      }
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
      assert(at(value(rejected), "error", "code").contains(Structure.Value.Integer(-32014)))
      assert(terminated.session.state == SessionState.Stopped)
      assert(terminated.response.isEmpty)
    }

    "rejects exit before shutdown and request-shaped exit calls" in {
      val loaded      = initializedSession()
      val terminated  = loaded.handle(notification("morphir.exit"))
      val exitRequest = value(initializedSession().handle(request(JsonRpcId(14), "morphir.exit")))

      assert(terminated.session.state == SessionState.Failed)
      assert(terminated.response.isEmpty)
      assert(at(exitRequest, "error", "message").contains(Structure.Value.Str("morphir.exit is a notification")))
    }
  }
end MepSessionTests
