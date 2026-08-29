package morphir.langkit.elm.compiler.mep

import kyo.*
import kyo.test.*
import org.finos.morphir.ir.MorphirIRFile
import org.finos.morphir.ir.distribution.Distribution
import org.finos.morphir.ir.json.MorphirJsonSupport.*
import org.finos.morphir.naming.{ModuleName, PackageName}
import zio.json.*
import zio.json.ast.Json

class MepSessionTests extends Test[Any]:

  private def at(value: Json, path: String*): Option[Json] =
    path.foldLeft(Option(value)) {
      case (Some(Json.Obj(fields)), field) => fields.toMap.get(field)
      case _                               => None
    }

  private def initializedSession: MepSession =
    MepSession.loaded(ProviderMetadata.default).handle(
      """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
    ).session

  private def metadataResponses(provider: ProviderMetadata): (Json, Json) =
    val initialize = MepSession.loaded(provider).handle(
      """{"jsonrpc":"2.0","id":"init","method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
    )
    val initialized = initialize.session
    val info        = initialized.handle(
      """{"jsonrpc":"2.0","id":"info","method":"morphir.extension.info","params":{}}"""
    )

    (
      initialize.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get,
      info.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get
    )

  "MepSession" - {
    "rejects non-object JSON-RPC envelopes as invalid requests" in {
      val session   = MepSession.loaded(ProviderMetadata.default)
      val responses = Vector("[]", "1").map(session.handle(_).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get)

      assert(responses.forall(response => at(response, "id") == Some(Json.Null)))
      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32600))))
    }

    "requires the JSON-RPC 2.0 envelope version" in {
      val session  = MepSession.loaded(ProviderMetadata.default)
      val requests = Vector(
        """{"id":"missing-version","method":"morphir.initialize","params":{}}""",
        """{"jsonrpc":"1.0","id":"wrong-version","method":"morphir.initialize","params":{}}"""
      )
      val responses =
        requests.map(session.handle(_).response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get)

      assert(at(responses.head, "id") == Some(Json.Str("missing-version")))
      assert(at(responses.last, "id") == Some(Json.Str("wrong-version")))
      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32600))))
    }

    "requires a string method in the JSON-RPC envelope" in {
      val session  = MepSession.loaded(ProviderMetadata.default)
      val requests = Vector(
        """{"jsonrpc":"2.0","id":20,"params":{}}""",
        """{"jsonrpc":"2.0","id":21,"method":1,"params":{}}"""
      )
      val responses =
        requests.map(session.handle(_).response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get)

      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32600))))
      assert(responses.forall(response =>
        at(response, "error", "message") == Some(Json.Str("Invalid JSON-RPC request"))
      ))
    }

    "rejects an empty JSON-RPC method" in {
      val response = MepSession.loaded(ProviderMetadata.default)
        .handle("""{"jsonrpc":"2.0","id":"empty-method","method":"","params":{}}""")
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "id") == Some(Json.Str("empty-method")))
      assert(at(response, "error", "code") == Some(Json.Num(-32600)))
      assert(at(response, "error", "message") == Some(Json.Str("Invalid JSON-RPC request")))
    }

    "rejects invalid JSON-RPC ID types with a null response ID" in {
      val session    = MepSession.loaded(ProviderMetadata.default)
      val invalidIds = Vector("{}", "[]", "true")
      val responses  = invalidIds.map { id =>
        session.handle(
          s"""{"jsonrpc":"2.0","id":$id,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
        ).response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get
      }

      assert(responses.forall(response => at(response, "id") == Some(Json.Null)))
      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32600))))
    }

    "rejects a fractional numeric JSON-RPC ID with a null response ID" in {
      val response = MepSession.loaded(ProviderMetadata.default)
        .handle(
          """{"jsonrpc":"2.0","id":1.5,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
        )
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "id") == Some(Json.Null))
      assert(at(response, "error", "code") == Some(Json.Num(-32600)))
    }

    "negotiates MEP 0.1 with the configured provider identity" in {
      val request =
        """{"jsonrpc":"2.0","id":"init-1","method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""

      val transition = MepSession.loaded(ProviderMetadata.default).handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Ready)
      assert(at(response, "id") == Some(Json.Str("init-1")))
      assert(at(response, "result", "protocolVersion") == Some(Json.Str("0.1")))
      assert(at(response, "result", "extension", "id") == Some(Json.Str("morphir-scala-elm")))
      assert(at(response, "result", "extension", "name") == Some(Json.Str("Morphir Scala Elm frontend")))
      assert(at(response, "result", "extension", "version") == Some(Json.Str("0.1.0")))
      assert(at(response, "result", "capabilities", "frontend", "compile") == Some(Json.Bool(true)))
    }

    "reports the compiled build metadata exactly during initialize and extension info" in {
      val (initialize, info) = metadataResponses(Main.providerMetadata)

      assert(at(initialize, "result", "extension", "id") == Some(Json.Str("morphir-scala-elm")))
      assert(at(initialize, "result", "extension", "name") == Some(Json.Str("Morphir Scala Elm frontend")))
      assert(at(initialize, "result", "extension", "version") == Some(Json.Str(MepBuildInfo.providerVersion)))
      assert(at(info, "result", "id") == Some(Json.Str("morphir-scala-elm")))
      assert(at(info, "result", "name") == Some(Json.Str("Morphir Scala Elm frontend")))
      assert(at(info, "result", "version") == Some(Json.Str(MepBuildInfo.providerVersion)))
    }

    "reports overridden build metadata exactly during initialize and extension info" in {
      val provider = Main.providerMetadata(
        id = "morphir-scala-elm",
        name = "Morphir Scala Elm frontend",
        version = "9.8.7"
      )
      val (initialize, info) = metadataResponses(provider)

      assert(at(initialize, "result", "extension", "id") == Some(Json.Str("morphir-scala-elm")))
      assert(at(initialize, "result", "extension", "name") == Some(Json.Str("Morphir Scala Elm frontend")))
      assert(at(initialize, "result", "extension", "version") == Some(Json.Str("9.8.7")))
      assert(at(info, "result", "id") == Some(Json.Str("morphir-scala-elm")))
      assert(at(info, "result", "name") == Some(Json.Str("Morphir Scala Elm frontend")))
      assert(at(info, "result", "version") == Some(Json.Str("9.8.7")))
    }

    "responds to ping before initialization" in {
      val response = MepSession.loaded(ProviderMetadata.default)
        .handle("""{"jsonrpc":"2.0","id":"ping-loaded","method":"morphir.ping","params":{}}""")
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "id") == Some(Json.Str("ping-loaded")))
      assert(at(response, "result", "ok") == Some(Json.Bool(true)))
    }

    "rejects non-object ping parameters" in {
      val response = MepSession.loaded(ProviderMetadata.default)
        .handle("""{"jsonrpc":"2.0","id":41,"method":"morphir.ping","params":null}""")
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
      assert(at(response, "error", "message") == Some(Json.Str("morphir.ping parameters must be an object")))
    }

    "executes an initialize notification without responding" in {
      val transition = MepSession.loaded(ProviderMetadata.default).handle(
        """{"jsonrpc":"2.0","method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      )

      assert(transition.session.state == SessionState.Ready)
      assert(transition.response.isEmpty)
    }

    "rejects incompatible protocol versions while preserving an integer ID" in {
      val request =
        """{"jsonrpc":"2.0","id":7,"method":"morphir.initialize","params":{"protocolVersions":["9.0"],"host":{"name":"test-host","version":"1.0.0"}}}"""

      val transition = MepSession.loaded(ProviderMetadata.default).handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Loaded)
      assert(at(response, "id") == Some(Json.Num(7)))
      assert(at(response, "error", "code") == Some(Json.Num(-32011)))
    }

    "requires host identity during initialization" in {
      val transition = MepSession.loaded(ProviderMetadata.default).handle(
        """{"jsonrpc":"2.0","id":8,"method":"morphir.initialize","params":{"protocolVersions":["0.1"]}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Loaded)
      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects non-string protocol version entries" in {
      val transition = MepSession.loaded(ProviderMetadata.default).handle(
        """{"jsonrpc":"2.0","id":19,"method":"morphir.initialize","params":{"protocolVersions":["0.1",1],"host":{"name":"test-host","version":"1.0.0"}}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Loaded)
      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects compile before initialization" in {
      val request = """{"jsonrpc":"2.0","id":1,"method":"morphir.frontend.compile","params":{}}"""

      val transition = MepSession.loaded(ProviderMetadata.default).handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(at(response, "error", "code") == Some(Json.Num(-32600)))
      assert(at(response, "error", "message") == Some(Json.Str("The MEP session is not initialized")))
    }

    "accepts the initialized notification without responding" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","method":"morphir.initialized","params":{}}"""
      )
      val response: Maybe[String] = transition.response

      assert(transition.session.state == SessionState.Ready)
      assert(response == Absent)
    }

    "returns exact extension metadata after initialization" in {
      val response = initializedSession
        .handle("""{"jsonrpc":"2.0","id":"info","method":"morphir.extension.info","params":{}}""")
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "id") == Some(Json.Str("info")))
      assert(at(response, "result") == Some(Json.Obj(
        "id"      -> Json.Str("morphir-scala-elm"),
        "name"    -> Json.Str("Morphir Scala Elm frontend"),
        "version" -> Json.Str("0.1.0"),
        "types"   -> Json.Arr(Json.Str("frontend"))
      )))
    }

    "returns exact extension capabilities after initialization" in {
      val response = initializedSession
        .handle("""{"jsonrpc":"2.0","id":42,"method":"morphir.extension.capabilities","params":{}}""")
        .response
        .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
        .get

      assert(at(response, "id") == Some(Json.Num(42)))
      assert(at(response, "result", "frontend", "languages") == Some(Json.Arr(
        Json.Obj("id" -> Json.Str("elm"), "fileExtensions" -> Json.Arr(Json.Str(".elm")))
      )))
      assert(at(response, "result", "frontend", "irVersions") == Some(Json.Arr(Json.Str("3"))))
      assert(at(response, "result", "frontend", "compile") == Some(Json.Bool(true)))
      assert(at(response, "result", "frontend", "incremental") == Some(Json.Bool(false)))
      assert(at(response, "result", "frontend", "fragments") == Some(Json.Bool(false)))
      assert(at(response, "result", "streaming") == Some(Json.Bool(false)))
      assert(at(response, "result", "incremental") == Some(Json.Bool(false)))
      assert(at(response, "result", "cancellation") == Some(Json.Bool(false)))
      assert(at(response, "result", "progress") == Some(Json.Bool(false)))
    }

    "rejects non-object extension metadata method parameters" in {
      val methods   = Vector("morphir.extension.info", "morphir.extension.capabilities")
      val responses = methods.zipWithIndex.map { case (method, index) =>
        initializedSession
          .handle(s"""{"jsonrpc":"2.0","id":${43 + index},"method":"$method","params":null}""")
          .response
          .flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption))
          .get
      }

      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32602))))
      assert(responses.zip(methods).forall { case (response, method) =>
        at(response, "error", "message") == Some(Json.Str(s"$method parameters must be an object"))
      })
    }

    "suppresses responses for core method notifications" in {
      val methods     = Vector("morphir.ping", "morphir.extension.info", "morphir.extension.capabilities")
      val transitions = methods.map(method =>
        initializedSession.handle(s"""{"jsonrpc":"2.0","method":"$method","params":{}}""")
      )

      assert(transitions.forall(_.session.state == SessionState.Ready))
      assert(transitions.forall(_.response.isEmpty))
    }

    "rejects repeated initialization as a lifecycle error" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","id":24,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Ready)
      assert(at(response, "error", "code") == Some(Json.Num(-32600)))
      assert(at(response, "error", "message") == Some(Json.Str("The MEP session is already initialized")))
    }

    "rejects an unknown method after initialization" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","id":"unknown-1","method":"morphir.unknown","params":{}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(at(response, "error", "code") == Some(Json.Num(-32601)))
      assert(at(response, "error", "message") == Some(Json.Str("Method not found: morphir.unknown")))
    }

    "suppresses responses for unknown method notifications" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","method":"morphir.unknown","params":{}}"""
      )

      assert(transition.session.state == SessionState.Ready)
      assert(transition.response.isEmpty)
    }

    "rejects invalid compile parameters" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","id":2,"method":"morphir.frontend.compile","params":{"languageId":"elm"}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
      assert(at(response, "error", "message") == Some(Json.Str("Invalid morphir.frontend.compile parameters")))
    }

    "encodes invalid compiler output as a JSON-RPC internal error" in {
      val response = initializedSession
        .compileErrorResponse(Json.Num(33), MepCompileError.InvalidCompilerOutput("bad IR"))
        .fromJson[Json]
        .toOption
        .get

      assert(at(response, "id") == Some(Json.Num(33)))
      assert(at(response, "error", "code") == Some(Json.Num(-32603)))
      assert(at(response, "error", "message") == Some(Json.Str("Internal error")))
    }

    "maps a compiler panic request to a JSON-RPC internal error" in {
      val panic   = IllegalStateException("synthetic compiler panic")
      val session = MepSession.loaded(
        ProviderMetadata.default,
        (_: Json) => Result.panic(panic)
      ).handle(
        """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      ).session
      val request =
        """{"jsonrpc":"2.0","id":41,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val transition = session.handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Ready)
      assert(at(response, "id") == Some(Json.Num(41)))
      assert(at(response, "error", "code") == Some(Json.Num(-32603)))
      assert(at(response, "error", "message") == Some(Json.Str("Internal error")))
    }

    "compiles one Elm document to embedded Morphir IR 3" in {
      val request =
        """{"jsonrpc":"2.0","id":"compile-1","method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right =\n    left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val transition = initializedSession.handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(at(response, "result", "success") == Some(Json.Bool(true)))
      assert(at(response, "result", "irVersion") == Some(Json.Str("3")))
      assert(at(response, "result", "ir").exists(_.isInstanceOf[Json.Obj]))
      assert(at(response, "result", "modules") == Some(Json.Arr(Json.Str("Example"))))
      assert(at(response, "result", "diagnostics") == Some(Json.Arr()))

      val (irPackage, irModules) = at(response, "result", "ir")
        .flatMap(JsonDecoder[MorphirIRFile].fromJsonAST(_).toOption)
        .collect {
          case MorphirIRFile(_, library: Distribution.Library) =>
            library.packageName -> library.packageDef.modules.keySet
        }
        .get
      val responseModules = at(response, "result", "modules").collect {
        case Json.Arr(values) => values.collect { case Json.Str(module) => ModuleName.fromString(module) }.toSet
      }.get
      assert(irPackage == PackageName.fromString("local/example"))
      assert(responseModules == irModules)
    }

    "ignores a fake module header inside a leading block comment" in {
      val request =
        """{"jsonrpc":"2.0","id":39,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"{-\nmodule Fake exposing (fake)\n-}\nmodule Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "result", "success") == Some(Json.Bool(true)))
      assert(at(response, "result", "modules") == Some(Json.Arr(Json.Str("Example"))))
    }

    "compiles an underscore-bearing module with a multiline header" in {
      val request =
        """{"jsonrpc":"2.0","id":40,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/My_Module.elm","languageId":"elm","version":1,"text":"module My_Module exposing\n    ( add\n    )\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["My_Module"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "result", "success") == Some(Json.Bool(true)))
      assert(at(response, "result", "modules") == Some(Json.Arr(Json.Str("My_Module"))))

      val ir        = at(response, "result", "ir").get.toJson.fromJson[MorphirIRFile].toOption.get
      val irModules = ir.distribution match
        case library: Distribution.Library => library.packageDef.modules.keys.map(_.toString).toSet
        case _                             => Set.empty[String]
      assert(irModules == Set("MyModule"))
    }

    "executes a valid compile notification without responding" in {
      val notification =
        """{"jsonrpc":"2.0","method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val transition = initializedSession.handle(notification)

      assert(transition.session.state == SessionState.Ready)
      assert(transition.response.isEmpty)
    }

    "executes a compiler panic notification without responding or crashing" in {
      val panic       = IllegalStateException("synthetic compiler panic")
      var invocations = 0
      val session     = MepSession.loaded(
        ProviderMetadata.default,
        (_: Json) =>
          invocations += 1
          Result.panic(panic)
      ).handle(
        """{"jsonrpc":"2.0","id":1,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}"""
      ).session
      val notification =
        """{"jsonrpc":"2.0","method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val transition = session.handle(notification)

      assert(invocations == 1)
      assert(transition.session.state == SessionState.Ready)
      assert(transition.response.isEmpty)
    }

    "returns parser failures as normal compile results with caller locations" in {
      val request =
        """{"jsonrpc":"2.0","id":3,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Broken.elm","languageId":"elm","version":2,"text":"module Broken exposing (value)\n\nvalue : Int\nvalue =\n"}],"package":{"name":"local/broken","exposedModules":["Broken"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val transition = initializedSession.handle(request)
      val response   = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get
      val diagnostic = at(response, "result", "diagnostics").collect {
        case Json.Arr(values) if values.nonEmpty => values.head
      }.get

      assert(at(response, "result", "success") == Some(Json.Bool(false)))
      assert(at(response, "result", "ir").isEmpty)
      assert(at(response, "result", "modules") == Some(Json.Arr()))
      assert(at(diagnostic, "severity") == Some(Json.Str("error")))
      assert(at(diagnostic, "code") == Some(Json.Str("elm.parser")))
      assert(at(diagnostic, "location", "uri") == Some(Json.Str("file:///workspace/Broken.elm")))
      assert(at(diagnostic, "location", "range", "start", "line").exists(_.isInstanceOf[Json.Num]))
    }

    "maps malformed module headers to elm.parser" in {
      val request =
        """{"jsonrpc":"2.0","id":4,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Malformed.elm","languageId":"elm","version":1,"text":"module Malformed\n\nvalue = 1\n"}],"package":{"name":"local/malformed","exposedModules":["Malformed"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get
      val diagnostic = at(response, "result", "diagnostics").collect {
        case Json.Arr(values) if values.nonEmpty => values.head
      }.get

      assert(at(diagnostic, "code") == Some(Json.Str("elm.parser")))
      assert(at(diagnostic, "location", "uri") == Some(Json.Str("file:///workspace/Malformed.elm")))
    }

    "retains stable codes for non-parser compiler diagnostics" in {
      val request =
        """{"jsonrpc":"2.0","id":18,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (value)\n\nvalue : Int\nvalue = missing\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get
      val diagnostic = at(response, "result", "diagnostics").collect {
        case Json.Arr(values) if values.nonEmpty => values.head
      }.get

      assert(at(diagnostic, "code") == Some(Json.Str("ELM-IR006")))
      assert(at(diagnostic, "message") == Some(Json.Str("Unsupported Elm type: function signature")))
      assert(at(diagnostic, "location", "uri") == Some(Json.Str("file:///workspace/Example.elm")))
    }

    "responds to shutdown and waits for exit" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","id":9,"method":"morphir.shutdown","params":{}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.AwaitExit)
      assert(at(response, "result") == Some(Json.Obj()))
    }

    "rejects reinitialize, compile, and unknown requests while awaiting exit" in {
      val awaitingExit = initializedSession.handle(
        """{"jsonrpc":"2.0","id":9,"method":"morphir.shutdown","params":{}}"""
      ).session
      val requests = Vector(
        """{"jsonrpc":"2.0","id":30,"method":"morphir.initialize","params":{"protocolVersions":["0.1"],"host":{"name":"test-host","version":"1.0.0"}}}""",
        """{"jsonrpc":"2.0","id":31,"method":"morphir.frontend.compile","params":{}}""",
        """{"jsonrpc":"2.0","id":32,"method":"morphir.unknown","params":{}}"""
      )
      val transitions = requests.map(awaitingExit.handle)
      val responses = transitions.map(_.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get)

      assert(transitions.forall(_.session.state == SessionState.AwaitExit))
      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32600))))
    }

    "accepts only an exit notification after shutdown without responding" in {
      val awaitingExit = initializedSession.handle(
        """{"jsonrpc":"2.0","id":9,"method":"morphir.shutdown","params":{}}"""
      ).session

      val transition = awaitingExit.handle("""{"jsonrpc":"2.0","method":"morphir.exit"}""")

      assert(transition.session.state == SessionState.Stopped)
      assert(transition.response.isEmpty)
    }

    "rejects explicit null or non-object shutdown parameters" in {
      val requests = Vector(
        """{"jsonrpc":"2.0","id":22,"method":"morphir.shutdown","params":null}""",
        """{"jsonrpc":"2.0","id":23,"method":"morphir.shutdown","params":[]}"""
      )
      val transitions = requests.map(initializedSession.handle)
      val responses = transitions.map(_.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get)

      assert(transitions.forall(_.session.state == SessionState.Ready))
      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32602))))
    }

    "executes a shutdown notification without responding" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","method":"morphir.shutdown"}"""
      )

      assert(transition.session.state == SessionState.AwaitExit)
      assert(transition.response.isEmpty)
    }

    "suppresses invalid-parameter shutdown notification errors" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","method":"morphir.shutdown","params":null}"""
      )

      assert(transition.session.state == SessionState.Ready)
      assert(transition.response.isEmpty)
    }

    "accepts exit as a terminating notification before initialization" in {
      val transition = MepSession.loaded(ProviderMetadata.default).handle(
        """{"jsonrpc":"2.0","method":"morphir.exit"}"""
      )

      assert(transition.session.state == SessionState.Stopped)
      assert(transition.response.isEmpty)
    }

    "rejects exit when sent as a request after initialization" in {
      val transition = initializedSession.handle(
        """{"jsonrpc":"2.0","id":25,"method":"morphir.exit","params":{}}"""
      )
      val response = transition.response.flatMap(value => Maybe.fromOption(value.fromJson[Json].toOption)).get

      assert(transition.session.state == SessionState.Ready)
      assert(at(response, "error", "code") == Some(Json.Num(-32600)))
      assert(at(response, "error", "message") == Some(Json.Str("morphir.exit is a notification")))
    }

    "rejects a non-Elm compile language" in {
      val request =
        """{"jsonrpc":"2.0","id":10,"method":"morphir.frontend.compile","params":{"languageId":"gleam","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects more than one by-value document" in {
      val document =
        """{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}"""
      val request =
        s"""{"jsonrpc":"2.0","id":11,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[$document,$document],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a document whose language is not Elm" in {
      val request =
        """{"jsonrpc":"2.0","id":12,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"gleam","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects an empty document URI" in {
      val request =
        """{"jsonrpc":"2.0","id":17,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "accepts the maximum unsigned 64-bit document version" in {
      val request =
        """{"jsonrpc":"2.0","id":35,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":18446744073709551615,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "result", "success") == Some(Json.Bool(true)))
    }

    "rejects a fractional document version" in {
      val request =
        """{"jsonrpc":"2.0","id":36,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1.5,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a document version above the unsigned 64-bit range" in {
      val request =
        """{"jsonrpc":"2.0","id":37,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":18446744073709551616,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a negative document version" in {
      val request =
        """{"jsonrpc":"2.0","id":38,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":-1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects an IR version other than 3" in {
      val request =
        """{"jsonrpc":"2.0","id":13,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"4"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects types-only compilation in the first slice" in {
      val request =
        """{"jsonrpc":"2.0","id":34,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":true,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a non-canonical package identity" in {
      val request =
        """{"jsonrpc":"2.0","id":14,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"Local/Example","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a package identity with an unseparated numeric word" in {
      val request =
        """{"jsonrpc":"2.0","id":26,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/foo2","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a package identity with doubled hyphens" in {
      val request =
        """{"jsonrpc":"2.0","id":27,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/foo--bar","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects a package identity with a trailing hyphen" in {
      val request =
        """{"jsonrpc":"2.0","id":28,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example-","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects package words outside the canonical lowercase-or-numeric grammar" in {
      val packageNames = Vector("local/fooBar", "local/foo_bar")
      val responses    = packageNames.zipWithIndex.map { case (packageName, index) =>
        val request =
          raw"""{"jsonrpc":"2.0","id":${29 +
              index},"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"$packageName","exposedModules":["Example"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""
        initializedSession.handle(request).response.flatMap(value =>
          Maybe.fromOption(value.fromJson[Json].toOption)
        ).get
      }

      assert(responses.forall(response => at(response, "error", "code") == Some(Json.Num(-32602))))
    }

    "rejects an exposed module list that cannot match the one-document IR" in {
      val request =
        """{"jsonrpc":"2.0","id":15,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Example","Other"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }

    "rejects an exposed module identity that differs from the source header" in {
      val request =
        """{"jsonrpc":"2.0","id":16,"method":"morphir.frontend.compile","params":{"languageId":"elm","documents":[{"uri":"file:///workspace/Example.elm","languageId":"elm","version":1,"text":"module Example exposing (add)\n\nadd : Int -> Int -> Int\nadd left right = left + right\n"}],"package":{"name":"local/example","exposedModules":["Other"]},"dependencies":[],"options":{"typesOnly":false,"irVersion":"3"}}}"""

      val response = initializedSession.handle(request).response.flatMap(value =>
        Maybe.fromOption(value.fromJson[Json].toOption)
      ).get

      assert(at(response, "error", "code") == Some(Json.Num(-32602)))
    }
  }
