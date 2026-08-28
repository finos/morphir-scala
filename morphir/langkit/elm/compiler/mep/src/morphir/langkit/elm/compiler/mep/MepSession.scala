package morphir.langkit.elm.compiler.mep

import zio.json.*
import zio.json.ast.Json

final case class SessionTransition(session: MepSession, response: Option[String]) derives CanEqual

final case class MepSession private (state: SessionState, provider: ProviderMetadata):
  def handle(body: String): SessionTransition =
    body.fromJson[Json].toOption match
      case Some(Json.Obj(fields))
          if fields.toMap.get("method") == Some(Json.Str("morphir.exit")) && !fields.toMap.contains("id") =>
        SessionTransition(copy(state = SessionState.Stopped), None)
      case Some(request @ Json.Obj(_)) if state == SessionState.Ready => ready(request)
      case Some(request @ Json.Obj(_))                                => initialize(request)
      case _ => SessionTransition(this, Some(error(Json.Null, -32700, "Parse error").toJson))

  private def ready(request: Json.Obj): SessionTransition =
    val fields = request.fields.toMap
    if fields.get("method") == Some(Json.Str("morphir.initialized")) && !fields.contains("id") then
      SessionTransition(this, None)
    else if fields.get("method") == Some(Json.Str("morphir.shutdown")) && !fields.contains("id") &&
      fields.get("params").forall(_.isInstanceOf[Json.Obj])
    then SessionTransition(copy(state = SessionState.Stopped), None)
    else
      (fields.get("method"), fields.get("id")) match
        case (Some(Json.Str("morphir.shutdown")), Some(id)) if fields.get("params").forall(_.isInstanceOf[Json.Obj]) =>
          SessionTransition(copy(state = SessionState.Stopped), Some(success(id, Json.Obj()).toJson))
        case (Some(Json.Str("morphir.frontend.compile")), Some(id)) =>
          fields.get("params").flatMap(params => MepElmFrontend.compile(params).toOption) match
            case Some(result) => SessionTransition(this, Some(success(id, result).toJson))
            case None         =>
              SessionTransition(this, Some(error(id, -32602, "Invalid morphir.frontend.compile parameters").toJson))
        case (Some(Json.Str(method)), Some(id)) =>
          SessionTransition(this, Some(error(id, -32601, s"Method not found: $method").toJson))
        case _ => initialize(request)

  private def initialize(request: Json.Obj): SessionTransition =
    val fields = request.fields.toMap
    val id     = fields.getOrElse("id", Json.Null)
    if fields.get("method") != Some(Json.Str("morphir.initialize")) then
      SessionTransition(this, Some(error(id, -32600, "The MEP session is not initialized").toJson))
    else initializeRequest(fields, id)

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
        SessionTransition(next, Some(success(id, initializationResult).toJson))
      case Some(offered) =>
        val data = Json.Obj(
          "hostVersions"      -> Json.Arr(offered.map(Json.Str.apply)*),
          "extensionVersions" -> Json.Arr(Json.Str(provider.protocolVersion))
        )
        SessionTransition(
          this,
          Some(error(id, -32011, "No compatible Morphir Extension Protocol version", Some(data)).toJson)
        )
      case _ => SessionTransition(this, Some(error(id, -32602, "Invalid morphir.initialize parameters").toJson))

  private def initializationResult: Json = Json.Obj(
    "protocolVersion" -> Json.Str(provider.protocolVersion),
    "extension"       -> Json.Obj(
      "id"      -> Json.Str(provider.id),
      "name"    -> Json.Str(provider.name),
      "version" -> Json.Str(provider.version),
      "types"   -> Json.Arr(provider.types.map(Json.Str.apply)*)
    ),
    "capabilities" -> Json.Obj(
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
  )

  private def success(id: Json, result: Json): Json =
    Json.Obj("jsonrpc" -> Json.Str("2.0"), "id" -> id, "result" -> result)

  private def error(id: Json, code: Int, message: String, data: Option[Json] = None): Json =
    Json.Obj(
      "jsonrpc" -> Json.Str("2.0"),
      "id"      -> id,
      "error"   -> Json.Obj(
        (Vector("code" -> Json.Num(code), "message" -> Json.Str(message)) ++ data.map("data" -> _))*
      )
    )

object MepSession:
  def loaded(provider: ProviderMetadata): MepSession = MepSession(SessionState.Loaded, provider)
