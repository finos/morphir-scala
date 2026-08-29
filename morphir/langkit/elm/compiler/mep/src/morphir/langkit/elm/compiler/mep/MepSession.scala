package morphir.langkit.elm.compiler.mep

import kyo.*
import zio.json.*
import zio.json.ast.Json

final case class SessionTransition(session: MepSession, response: Maybe[String]) derives CanEqual

private[mep] type CompileFrontend = Json => Result[MepCompileError, Json]

final case class MepSession private (
    state: SessionState,
    provider: ProviderMetadata,
    private val compileFrontend: CompileFrontend
):
  def handle(body: String): SessionTransition =
    body.fromJson[Json] match
      case Right(Json.Obj(fields))
          if fields.toMap.get("id").exists {
            case _: Json.Str | Json.Null => false
            case Json.Num(value)         => value.stripTrailingZeros.scale > 0
            case _                       => true
          } =>
        SessionTransition(this, Present(error(Json.Null, -32600, "Invalid JSON-RPC request").toJson))
      case Right(Json.Obj(fields)) if fields.toMap.get("jsonrpc") != Some(Json.Str("2.0")) =>
        val id = fields.toMap.getOrElse("id", Json.Null)
        SessionTransition(this, Present(error(id, -32600, "Invalid JSON-RPC request").toJson))
      case Right(Json.Obj(fields))
          if !fields.toMap.get("method").exists {
            case Json.Str(value) => value.nonEmpty
            case _               => false
          } =>
        val id = fields.toMap.getOrElse("id", Json.Null)
        SessionTransition(this, Present(error(id, -32600, "Invalid JSON-RPC request").toJson))
      case Right(Json.Obj(fields))
          if fields.toMap.get("method") == Some(Json.Str("morphir.exit")) && !fields.toMap.contains("id") =>
        SessionTransition(copy(state = SessionState.Stopped), Absent)
      case Right(Json.Obj(fields)) if state == SessionState.Stopped =>
        val response = fields.toMap.get("id").map(id => error(id, -32600, "The MEP session is stopped").toJson)
        SessionTransition(this, Maybe.fromOption(response))
      case Right(Json.Obj(fields)) if state == SessionState.AwaitExit =>
        val response = fields.toMap.get("id").map(id => error(id, -32600, "The MEP session is awaiting exit").toJson)
        SessionTransition(this, Maybe.fromOption(response))
      case Right(Json.Obj(fields))
          if fields.toMap.get("method") == Some(Json.Str("morphir.ping")) &&
            fields.toMap.get("params").forall(_.isInstanceOf[Json.Obj]) =>
        val response = fields.toMap.get("id").map(id => success(id, Json.Obj("ok" -> Json.Bool(true))).toJson)
        SessionTransition(this, Maybe.fromOption(response))
      case Right(Json.Obj(fields)) if fields.toMap.get("method") == Some(Json.Str("morphir.ping")) =>
        val response = fields.toMap.get("id").map { id =>
          error(id, -32602, "morphir.ping parameters must be an object").toJson
        }
        SessionTransition(this, Maybe.fromOption(response))
      case Right(request @ Json.Obj(_)) if state == SessionState.Ready => ready(request)
      case Right(request @ Json.Obj(_))                                => initialize(request)
      case Right(_) => SessionTransition(this, Present(error(Json.Null, -32600, "Invalid JSON-RPC request").toJson))
      case Left(_)  => parseError

  private[mep] def parseError: SessionTransition =
    SessionTransition(this, Present(error(Json.Null, -32700, "Parse error").toJson))

  private def ready(request: Json.Obj): SessionTransition =
    val fields = request.fields.toMap
    if fields.get("method") == Some(Json.Str("morphir.initialized")) && !fields.contains("id") then
      SessionTransition(this, Absent)
    else if fields.get("method") == Some(Json.Str("morphir.shutdown")) && !fields.contains("id") &&
      fields.get("params").forall(_.isInstanceOf[Json.Obj])
    then SessionTransition(copy(state = SessionState.AwaitExit), Absent)
    else if fields.get("method") == Some(Json.Str("morphir.shutdown")) && !fields.contains("id") then
      SessionTransition(this, Absent)
    else if fields.get("method") == Some(Json.Str("morphir.extension.info")) &&
      fields.get("params").forall(_.isInstanceOf[Json.Obj])
    then SessionTransition(this, Maybe.fromOption(fields.get("id").map(id => success(id, extensionInfo).toJson)))
    else if fields.get("method") == Some(Json.Str("morphir.extension.info")) then
      SessionTransition(
        this,
        Maybe.fromOption(
          fields.get("id").map(id => error(id, -32602, "morphir.extension.info parameters must be an object").toJson)
        )
      )
    else if fields.get("method") == Some(Json.Str("morphir.extension.capabilities")) &&
      fields.get("params").forall(_.isInstanceOf[Json.Obj])
    then SessionTransition(this, Maybe.fromOption(fields.get("id").map(id => success(id, capabilities).toJson)))
    else if fields.get("method") == Some(Json.Str("morphir.extension.capabilities")) then
      SessionTransition(
        this,
        Maybe.fromOption(
          fields.get("id").map(id =>
            error(id, -32602, "morphir.extension.capabilities parameters must be an object").toJson
          )
        )
      )
    else
      (fields.get("method"), fields.get("id")) match
        case (Some(Json.Str("morphir.initialize")), Some(id)) =>
          SessionTransition(this, Present(error(id, -32600, "The MEP session is already initialized").toJson))
        case (Some(Json.Str("morphir.shutdown")), Some(id)) if fields.get("params").forall(_.isInstanceOf[Json.Obj]) =>
          SessionTransition(copy(state = SessionState.AwaitExit), Present(success(id, Json.Obj()).toJson))
        case (Some(Json.Str("morphir.shutdown")), Some(id)) =>
          SessionTransition(this, Present(error(id, -32602, "morphir.shutdown parameters must be an object").toJson))
        case (Some(Json.Str("morphir.frontend.compile")), Some(id)) =>
          val compileResult = fields.get("params") match
            case Some(params) => compileFrontend(params)
            case None         => Result.fail(MepCompileError.InvalidParams("compile params are required"))
          compileResult match
            case Result.Success(result)         => SessionTransition(this, Present(success(id, result).toJson))
            case Result.Failure(compileFailure) =>
              SessionTransition(this, Present(compileErrorResponse(id, compileFailure)))
            case Result.Panic(_) =>
              SessionTransition(
                this,
                Present(compileErrorResponse(id, MepCompileError.InvalidCompilerOutput("compiler panic")))
              )
        case (Some(Json.Str("morphir.frontend.compile")), None) =>
          fields.get("params").foreach(compileFrontend)
          SessionTransition(this, Absent)
        case (Some(Json.Str("morphir.exit")), Some(id)) =>
          SessionTransition(this, Present(error(id, -32600, "morphir.exit is a notification").toJson))
        case (Some(Json.Str(method)), None)
            if method != "morphir.frontend.compile" && method != "morphir.shutdown" =>
          SessionTransition(this, Absent)
        case (Some(Json.Str(method)), Some(id)) =>
          SessionTransition(this, Present(error(id, -32601, s"Method not found: $method").toJson))
        case _ => initialize(request)

  private def initialize(request: Json.Obj): SessionTransition =
    val fields     = request.fields.toMap
    val id         = fields.getOrElse("id", Json.Null)
    val transition =
      if fields.get("method") != Some(Json.Str("morphir.initialize")) then
        SessionTransition(this, Present(error(id, -32600, "The MEP session is not initialized").toJson))
      else initializeRequest(fields, id)
    if fields.contains("id") then transition else transition.copy(response = Absent)

  private def initializeRequest(fields: Map[String, Json], id: Json): SessionTransition =
    val versions =
      for
        case Json.Str("2.0") <- fields.get("jsonrpc")
        case Json.Obj(params) <- fields.get("params")
        case Json.Arr(values) <- params.toMap.get("protocolVersions")
        if values.forall(_.isInstanceOf[Json.Str])
        case Json.Obj(host) <- params.toMap.get("host")
        case Json.Str(_) <- host.toMap.get("name")
        case Json.Str(_) <- host.toMap.get("version")
      yield values.collect { case Json.Str(value) => value }

    versions match
      case Some(offered) if offered.contains(provider.protocolVersion) =>
        val next = copy(state = SessionState.Ready)
        SessionTransition(next, Present(success(id, initializationResult).toJson))
      case Some(offered) =>
        val data = Json.Obj(
          "hostVersions"      -> Json.Arr(offered.map(Json.Str.apply)*),
          "extensionVersions" -> Json.Arr(Json.Str(provider.protocolVersion))
        )
        SessionTransition(
          this,
          Present(error(id, -32011, "No compatible Morphir Extension Protocol version", Some(data)).toJson)
        )
      case _ => SessionTransition(this, Present(error(id, -32602, "Invalid morphir.initialize parameters").toJson))

  private def initializationResult: Json = Json.Obj(
    "protocolVersion" -> Json.Str(provider.protocolVersion),
    "extension"       -> extensionInfo,
    "capabilities"    -> capabilities
  )

  private def capabilities: Json = Json.Obj(
    "frontend" -> Json.Obj(
      "languages" -> Json.Arr(provider.languages.map(language =>
        Json.Obj(
          "id"             -> Json.Str(language.id),
          "fileExtensions" -> Json.Arr(language.fileExtensions.map(Json.Str.apply)*)
        )
      )*),
      "irVersions"  -> Json.Arr(provider.irVersions.map(Json.Str.apply)*),
      "compile"     -> Json.Bool(provider.compile),
      "incremental" -> Json.Bool(false),
      "fragments"   -> Json.Bool(false)
    ),
    "streaming"    -> Json.Bool(false),
    "incremental"  -> Json.Bool(false),
    "cancellation" -> Json.Bool(false),
    "progress"     -> Json.Bool(false)
  )

  private def extensionInfo: Json = Json.Obj(
    "id"      -> Json.Str(provider.id),
    "name"    -> Json.Str(provider.name),
    "version" -> Json.Str(provider.version),
    "types"   -> Json.Arr(provider.types.map(Json.Str.apply)*)
  )

  private def success(id: Json, result: Json): Json =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result)

  private[mep] def compileErrorResponse(id: Json, compileFailure: MepCompileError): String =
    val message = compileFailure match
      case _: MepCompileError.InvalidParams => "Invalid morphir.frontend.compile parameters"
      case _                                => "Internal error"
    error(id, MepCompileError.jsonRpcCode(compileFailure), message).toJson

  private def error(id: Json, code: Int, message: String, data: Option[Json] = None): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "error"   -> Json.Obj(
        (Vector("code" -> Json.Num(code), "message" -> Json.Str(message)) ++ data.map("data" -> _))*
      )
    )

object MepSession:
  def loaded(provider: ProviderMetadata): MepSession = loaded(provider, MepElmFrontend.compile)

  private[mep] def loaded(provider: ProviderMetadata, compileFrontend: CompileFrontend): MepSession =
    MepSession(SessionState.Loaded, provider, compileFrontend)
