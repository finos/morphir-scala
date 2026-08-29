package morphir.langkit.elm.compiler.mep

import kyo.*

final case class SessionTransition(session: MepSession, response: Maybe[String]) derives CanEqual

private[mep] type CompileFrontend = Structure.Value => Result[MepCompileError, Structure.Value]

private final case class MepWireError(
    override val code: Int,
    label: String,
    override val data: Maybe[Structure.Value] = Absent
)(using Frame)
    extends JsonRpcApplicationError(code, label, data)

private final case class IncomingCall(
    id: Maybe[JsonRpcId],
    method: String,
    params: Option[Structure.Value]
)

final case class MepSession private (
    state: SessionState,
    provider: ProviderMetadata,
    private val compileFrontend: CompileFrontend
):
  def handle(body: String): SessionTransition =
    Json.decode[Structure.Value](body) match
      case Result.Success(raw) => decodeCall(raw) match
          case Right(call) => dispatch(call)
          case Left(id)    => SessionTransition(this, Present(error(id, -32600, "Invalid JSON-RPC request")))
      case Result.Failure(_) => parseError
      case Result.Panic(_)   => parseError

  private[mep] def parseError: SessionTransition =
    SessionTransition(this, Present(error(Absent, -32700, "Parse error")))

  private def decodeCall(raw: Structure.Value): Either[Maybe[JsonRpcId], IncomingCall] = raw match
    case Structure.Value.Record(fields) =>
      val values    = fields.iterator.toMap
      val id        = decodeId(values.get("id"))
      val invalidId = values.get("id").exists {
        case Structure.Value.Null | Structure.Value.Integer(_) | Structure.Value.Str(_) => false
        case _                                                                          => true
      }
      val validEnvelope =
        values.get("jsonrpc").contains(Structure.Value.Str("2.0")) &&
          values.get("method").exists {
            case Structure.Value.Str(value) => value.nonEmpty
            case _                          => false
          } && !invalidId
      if !validEnvelope then Left(if invalidId then Absent else id)
      else
        Structure.decode[JsonRpcEnvelope](raw) match
          case Result.Success(_: JsonRpcRequest | _: JsonRpcNotification) =>
            Right(
              IncomingCall(
                id,
                values("method").asInstanceOf[Structure.Value.Str].value,
                values.get("params")
              )
            )
          case _ => Left(id)
    case _ => Left(Absent)

  private def decodeId(value: Option[Structure.Value]): Maybe[JsonRpcId] = value match
    case Some(Structure.Value.Integer(number)) => Present(JsonRpcId(number))
    case Some(Structure.Value.Str(value))      => Present(JsonRpcId(value))
    case _                                     => Absent

  private def dispatch(call: IncomingCall): SessionTransition =
    if call.method == "morphir.exit" && call.id.isEmpty then
      SessionTransition(copy(state = SessionState.Stopped), Absent)
    else if state == SessionState.Stopped then
      respond(call.id)(errorFor(_, -32600, "The MEP session is stopped"))
    else if state == SessionState.AwaitExit then
      respond(call.id)(errorFor(_, -32600, "The MEP session is awaiting exit"))
    else if call.method == "morphir.ping" then
      if objectParams(call.params) then respondSuccess(call.id, Structure.encode(PingResult(ok = true)))
      else respond(call.id)(errorFor(_, -32602, "morphir.ping parameters must be an object"))
    else if state == SessionState.Ready then ready(call)
    else initialize(call)

  private def ready(call: IncomingCall): SessionTransition = call.method match
    case "morphir.initialized" if call.id.isEmpty => SessionTransition(this, Absent)
    case "morphir.shutdown" if call.id.isEmpty    =>
      if objectParams(call.params) then SessionTransition(copy(state = SessionState.AwaitExit), Absent)
      else SessionTransition(this, Absent)
    case "morphir.extension.info" =>
      if objectParams(call.params) then respondSuccess(call.id, extensionInfo)
      else respond(call.id)(errorFor(_, -32602, "morphir.extension.info parameters must be an object"))
    case "morphir.extension.capabilities" =>
      if objectParams(call.params) then respondSuccess(call.id, capabilities)
      else respond(call.id)(errorFor(_, -32602, "morphir.extension.capabilities parameters must be an object"))
    case "morphir.initialize" if call.id.isDefined =>
      respond(call.id)(errorFor(_, -32600, "The MEP session is already initialized"))
    case "morphir.shutdown" if call.id.isDefined =>
      if objectParams(call.params) then
        respondSuccess(call.id, Structure.Value.Record(Chunk.empty), copy(state = SessionState.AwaitExit))
      else respond(call.id)(errorFor(_, -32602, "morphir.shutdown parameters must be an object"))
    case "morphir.frontend.compile"          => compile(call)
    case "morphir.exit" if call.id.isDefined =>
      respond(call.id)(errorFor(_, -32600, "morphir.exit is a notification"))
    case _ if call.id.isEmpty => SessionTransition(this, Absent)
    case method               => respond(call.id)(errorFor(_, -32601, s"Method not found: $method"))

  private def compile(call: IncomingCall): SessionTransition =
    val result = call.params match
      case Some(params) => compileFrontend(params)
      case None         => Result.fail(MepCompileError.InvalidParams("compile params are required"))
    if call.id.isEmpty then SessionTransition(this, Absent)
    else
      result match
        case Result.Success(value)   => respondSuccess(call.id, value)
        case Result.Failure(failure) => respond(call.id)(id => compileErrorResponse(id, failure))
        case Result.Panic(_)         =>
          respond(call.id)(id => compileErrorResponse(id, MepCompileError.InvalidCompilerOutput("compiler panic")))

  private def initialize(call: IncomingCall): SessionTransition =
    if call.method != "morphir.initialize" then
      respond(call.id)(errorFor(_, -32600, "The MEP session is not initialized"))
    else
      val transition = call.params match
        case Some(params) => Structure.decode[InitializeRequest](params) match
            case Result.Success(request) if request.protocolVersions.contains(provider.protocolVersion) =>
              respondSuccess(
                call.id,
                Structure.encode(initializationResult),
                copy(state = SessionState.Ready)
              )
            case Result.Success(request) =>
              val data = Structure.Value.Record(
                Chunk(
                  "hostVersions" -> Structure.Value.Sequence(request.protocolVersions.map(Structure.Value.Str.apply)),
                  "extensionVersions" -> Structure.Value.Sequence(Chunk(Structure.Value.Str(provider.protocolVersion)))
                )
              )
              respond(call.id)(errorFor(_, -32011, "No compatible Morphir Extension Protocol version", Present(data)))
            case _ => respond(call.id)(errorFor(_, -32602, "Invalid morphir.initialize parameters"))
        case None => respond(call.id)(errorFor(_, -32602, "Invalid morphir.initialize parameters"))
      if call.id.isDefined then transition else transition.copy(response = Absent)

  private def objectParams(params: Option[Structure.Value]): Boolean = params match
    case None | Some(Structure.Value.Record(_)) => true
    case _                                      => false

  private def initializationResult: InitializationResult =
    InitializationResult(provider.protocolVersion, extensionInfoValue, capabilitiesValue)

  private def extensionInfoValue: ExtensionInfo =
    ExtensionInfo(provider.id, provider.name, provider.version, provider.types)

  private def capabilitiesValue: ExtensionCapabilities =
    ExtensionCapabilities(
      FrontendCapabilities(
        provider.languages,
        provider.irVersions,
        provider.compile,
        incremental = false,
        fragments = false
      ),
      streaming = false,
      incremental = false,
      cancellation = false,
      progress = false
    )

  private def extensionInfo: Structure.Value = Structure.encode(extensionInfoValue)

  private def capabilities: Structure.Value = Structure.encode(capabilitiesValue)

  private def respondSuccess(
      id: Maybe[JsonRpcId],
      value: Structure.Value,
      next: MepSession = this
  ): SessionTransition =
    respond(id, next)(requestId => encode(JsonRpcResponse.success(requestId, value)))

  private def respond(
      id: Maybe[JsonRpcId],
      next: MepSession = this
  )(response: JsonRpcId => String): SessionTransition =
    SessionTransition(next, id.map(response))

  private[mep] def compileErrorResponse(id: JsonRpcId, compileFailure: MepCompileError): String =
    val message = compileFailure match
      case _: MepCompileError.InvalidParams => "Invalid morphir.frontend.compile parameters"
      case _                                => "Internal error"
    error(Present(id), MepCompileError.jsonRpcCode(compileFailure), message)

  private def errorFor(
      id: JsonRpcId,
      code: Int,
      message: String,
      data: Maybe[Structure.Value] = Absent
  ): String = error(Present(id), code, message, data)

  private def error(
      id: Maybe[JsonRpcId],
      code: Int,
      message: String,
      data: Maybe[Structure.Value] = Absent
  ): String = id match
    case Present(requestId) => encode(JsonRpcResponse.failure(requestId, MepWireError(code, message, data)))
    case Absent             =>
      val errorFields = Chunk("code" -> Structure.Value.Integer(code), "message" -> Structure.Value.Str(message)) ++
        data.map(value => Chunk("data" -> value)).getOrElse(Chunk.empty)
      Json.encode(
        Structure.Value.Record(
          Chunk(
            "jsonrpc" -> Structure.Value.Str("2.0"),
            "id"      -> Structure.Value.Null,
            "error"   -> Structure.Value.Record(errorFields)
          )
        )
      )

  private def encode(response: JsonRpcResponse): String =
    Json.encode[JsonRpcEnvelope](response)

object MepSession:
  def loaded(provider: ProviderMetadata): MepSession = loaded(provider, MepElmFrontend.compile)

  private[mep] def loaded(provider: ProviderMetadata, compileFrontend: CompileFrontend): MepSession =
    MepSession(SessionState.Loaded, provider, compileFrontend)
