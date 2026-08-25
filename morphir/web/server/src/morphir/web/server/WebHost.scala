package morphir.web.server

import java.nio.charset.StandardCharsets
import kyo.*
import kyo.kernel.Effect
import morphir.appkit.SecretVault
import morphir.connector.github.GitHubTokenVerifier
import morphir.ui.github.GitHubConnectionCoordinator
import morphir.ui.services.{GitHubConnectionError, GitHubConnectionRpc}

object WebHost:

  private val Loopback = "127.0.0.1"

  val contentSecurityPolicy =
    "default-src 'self'; script-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; object-src 'none'; base-uri 'none'; frame-ancestors 'none'"

  final case class Config(
      port: Int = 0,
      openBrowser: Boolean = true,
      sessionTtl: Duration = 8.hours
  )

  sealed abstract class WebHostError(message: String) extends Exception(message):
    override def toString: String = s"WebHostError($message)"

  object WebHostError:
    final case class InvalidPort private[WebHost] () extends WebHostError("Port must be between 0 and 65535")
    final case class InvalidSessionTtl private[WebHost] ()
        extends WebHostError("Session TTL must be finite and positive")
    final case class BindFailed private[WebHost] ()    extends WebHostError("The loopback server could not bind")
    final case class StartupFailed private[WebHost] () extends WebHostError("The local web host could not start")

  trait BrowserLauncher:
    def open(url: String)(using Frame): Boolean < Async

  final class BoundHost private[WebHost] (
      val origin: String,
      val port: Int,
      waitForClose: () => Unit < Async
  ):
    def await(using Frame): Unit < Async = waitForClose()
    override def toString: String        = s"BoundHost($origin)"

  def start(
      config: Config,
      browserLauncher: BrowserLauncher,
      verifier: GitHubTokenVerifier,
      vault: Maybe[SecretVault]
  )(using Frame): BoundHost < (Async & Scope & Abort[WebHostError]) =
    startWithLauncher(config, url => browserLauncher.open(url), verifier, vault)

  def startWithLauncher(
      config: Config,
      browserLauncher: String => Boolean < Async,
      verifier: GitHubTokenVerifier,
      vault: Maybe[SecretVault]
  )(using Frame): BoundHost < (Async & Scope & Abort[WebHostError]) =
    validate(config).andThen {
      for
        sessions      <- LaunchSessions.init(config.sessionTtl)
        initialLaunch <-
          if config.openBrowser then
            sessions.createLaunch.map(launch => Present(launch): Maybe[LaunchSessions.LaunchCredential])
          else Sync.defer(Absent: Maybe[LaunchSessions.LaunchCredential])
        bootstrap   <- RootBootstrap.init(sessions, initialLaunch.nonEmpty)
        bridge      <- Scope.acquireRelease(HttpJsonRpcBridge.init)(bridge => releaseBridge(bridge.close))
        coordinator <- initializeCoordinator(verifier, vault)
        _           <- JsonRpcHandler.init(bridge.transport, GitHubConnectionRpc.routes(coordinator)*)
        boundPort   <- AtomicInt.init(-1)
        handlers = routes(boundPort, sessions, bridge, bootstrap)
        server <- bind(config.port, handlers)
        port = server.port
        _ <- boundPort.set(port)
        origin = s"http://$Loopback:$port"
        _ <- initialLaunch.fold(Kyo.unit) { launch =>
          openBrowser(browserLauncher, s"$origin/#launch=${launch.value}").map { opened =>
            if opened then Kyo.unit
            else Abort.run[Closed](bootstrap.launchFailed).unit
          }
        }
      yield BoundHost(origin, port, () => server.await)
    }

  private def openBrowser(browserLauncher: String => Boolean < Async, url: String)(using Frame): Boolean < Async =
    Effect.catching(browserLauncher(url))((_: Throwable) => false)

  private[server] def releaseBridge(close: => Unit < Async)(using Frame): Unit < (Async & Abort[Throwable]) =
    Async.mask(close)

  private def validate(config: Config)(using Frame): Unit < Abort[WebHostError] =
    if config.port < 0 || config.port > 65535 then Abort.fail(WebHostError.InvalidPort())
    else if config.sessionTtl == Duration.Infinity || config.sessionTtl <= Duration.Zero then
      Abort.fail(WebHostError.InvalidSessionTtl())
    else Kyo.unit

  private def initializeCoordinator(
      verifier: GitHubTokenVerifier,
      vault: Maybe[SecretVault]
  )(using Frame): GitHubConnectionCoordinator < (Async & Abort[WebHostError]) =
    Abort.run[GitHubConnectionError](GitHubConnectionCoordinator.init(verifier, vault)).map {
      case Result.Success(coordinator) => coordinator
      case Result.Failure(_)           => Abort.fail(WebHostError.StartupFailed())
      case Result.Panic(_)             => Abort.fail(WebHostError.StartupFailed())
    }

  private def bind(
      port: Int,
      handlers: Seq[HttpHandler[?, ?, ?]]
  )(using Frame): HttpServer < (Async & Scope & Abort[WebHostError]) =
    val config = HttpServerConfig.default
      .port(port)
      .host(Loopback)
      .maxContentLength(RequestPolicy.maxRequestBytes)
      .withoutAutoFilters
    Abort.run[HttpBindException](HttpServer.init(config)(handlers*)).map {
      case Result.Success(server) => server
      case Result.Failure(_)      => Abort.fail(WebHostError.BindFailed())
      case Result.Panic(_)        => Abort.fail(WebHostError.BindFailed())
    }

  private def routes(
      boundPort: AtomicInt,
      sessions: LaunchSessions,
      bridge: HttpJsonRpcBridge,
      bootstrap: RootBootstrap
  )(using Frame): Seq[HttpHandler[?, ?, ?]] =
    Seq(
      rootRoute(boundPort, bootstrap),
      exchangeRoute(boundPort, sessions, bootstrap),
      rpcRoute(boundPort, sessions, bridge)
    ) ++ registeredApiFallbacks(boundPort, sessions) ++ unmatchedApiFallbacks(boundPort) ++ Seq(staticRoute(boundPort))

  private def rootRoute(
      boundPort: AtomicInt,
      bootstrap: RootBootstrap
  )(using Frame): HttpHandler[?, ?, ?] =
    val route = HttpRoute.getRaw("/").response(_.bodyBinary)
    route.handler { request =>
      boundPort.get.map { port =>
        if !validHost(request.headers, port) then binary(HttpStatus.Unauthorized)
        else
          Abort.run[Closed](bootstrap.decide(request.headers)).map {
            case Result.Success(RootBootstrap.Decision.ServeHtml)        => staticResponse("/")
            case Result.Success(RootBootstrap.Decision.Redirect(launch)) =>
              binary(HttpStatus.Found, headers = securityHeaders.set("Location", s"/#launch=${launch.value}"))
            case _ => binary(HttpStatus.ServiceUnavailable)
          }
      }
    }

  private def staticRoute(boundPort: AtomicInt)(using Frame): HttpHandler[?, ?, ?] =
    val route = HttpRoute.getRaw(Capture.Rest("path")).response(_.bodyBinary)
    route.handler { request =>
      boundPort.get.map { port =>
        if !validHost(request.headers, port) then binary(HttpStatus.Unauthorized)
        else staticResponse(request.path)
      }
    }

  private def exchangeRoute(
      boundPort: AtomicInt,
      sessions: LaunchSessions,
      bootstrap: RootBootstrap
  )(using Frame): HttpHandler[?, ?, ?] =
    val route = HttpRoute.postRaw("/api/session/exchange").request(_.bodyBinary).response(_.bodyBinary)
    route.handler { request =>
      val body = new String(request.fields.body.toArrayUnsafe, StandardCharsets.UTF_8)
      boundPort.get.map { port =>
        RequestPolicy.authorizeExchange(RequestPolicy.Request(request.headers, body), port, sessions).map {
          case Result.Success(cookie) =>
            Abort.run[Closed](bootstrap.sessionEstablished).map { _ =>
              binary(HttpStatus.NoContent, headers = securityHeaders.set("Set-Cookie", cookie.headerValue))
            }
          case Result.Failure(rejection) => policyResponse(rejection)
          case Result.Panic(_)           => binary(HttpStatus.BadRequest)
        }
      }
    }

  private def rpcRoute(
      boundPort: AtomicInt,
      sessions: LaunchSessions,
      bridge: HttpJsonRpcBridge
  )(using Frame): HttpHandler[?, ?, ?] =
    val route = HttpRoute.postRaw("/api/jsonrpc").request(_.bodyBinary).response(_.bodyBinary)
    route.handler { request =>
      val body = new String(request.fields.body.toArrayUnsafe, StandardCharsets.UTF_8)
      boundPort.get.map { port =>
        RequestPolicy.authorizeRpc(RequestPolicy.Request(request.headers, body), port, sessions).map {
          case Result.Success(_) =>
            Abort.run[Closed](bridge.handle(body)).map {
              case Result.Success(response) => bridgeResponse(response)
              case Result.Failure(_)        => binary(HttpStatus.ServiceUnavailable)
              case Result.Panic(_)          => binary(HttpStatus.BadRequest)
            }
          case Result.Failure(rejection) => policyResponse(rejection)
          case Result.Panic(_)           => binary(HttpStatus.BadRequest)
        }
      }
    }

  private def registeredApiFallbacks(
      boundPort: AtomicInt,
      sessions: LaunchSessions
  )(using Frame): Seq[HttpHandler[?, ?, ?]] =
    val unsupportedMethods = apiMethods.filterNot(_ == HttpMethod.POST)
    val rpcRoutes          = unsupportedMethods.map(method => rawRoute(method, HttpPath.Literal("/api/jsonrpc")))
    val exchangeRoutes = unsupportedMethods.map(method => rawRoute(method, HttpPath.Literal("/api/session/exchange")))
    rpcRoutes.map(rpcFallback(_, boundPort, sessions)) ++
      exchangeRoutes.map(exchangeFallback(_, boundPort))

  private def rpcFallback[In](
      route: HttpRoute[In, Any, Nothing],
      boundPort: AtomicInt,
      sessions: LaunchSessions
  )(using Frame): HttpHandler[?, ?, ?] =
    route.response(_.bodyBinary).handler { request =>
      boundPort.get.map { port =>
        RequestPolicy.authorizeRpc(RequestPolicy.Request(request.headers, ""), port, sessions).map {
          case Result.Success(_)         => binary(HttpStatus.MethodNotAllowed)
          case Result.Failure(rejection) => policyResponse(rejection)
          case Result.Panic(_)           => binary(HttpStatus.BadRequest)
        }
      }
    }

  private def exchangeFallback[In](
      route: HttpRoute[In, Any, Nothing],
      boundPort: AtomicInt
  )(using Frame): HttpHandler[?, ?, ?] =
    route.response(_.bodyBinary).handler { request =>
      boundPort.get.map { port =>
        RequestPolicy.authorizeApiRequest(RequestPolicy.Request(request.headers, ""), port) match
          case Result.Success(_)         => binary(HttpStatus.MethodNotAllowed)
          case Result.Failure(rejection) => policyResponse(rejection)
          case Result.Panic(_)           => binary(HttpStatus.BadRequest)
      }
    }

  private def unmatchedApiFallbacks(boundPort: AtomicInt)(using Frame): Seq[HttpHandler[?, ?, ?]] =
    unmatchedApiPath(HttpPath.Literal("/api"), boundPort) ++
      unmatchedApiPath("/api" / Capture.Rest("apiPath"), boundPort)

  private def unmatchedApiPath[In](
      path: HttpPath[In],
      boundPort: AtomicInt
  )(using Frame): Seq[HttpHandler[?, ?, ?]] =
    apiMethods.map(method => rawRoute(method, path)).map { route =>
      route.response(_.bodyBinary).handler { request =>
        boundPort.get.map { port =>
          if validHost(request.headers, port) then binary(HttpStatus.NotFound)
          else binary(HttpStatus.Unauthorized)
        }
      }
    }

  private val apiMethods = Seq(
    HttpMethod.GET,
    HttpMethod.POST,
    HttpMethod.PUT,
    HttpMethod.PATCH,
    HttpMethod.DELETE,
    HttpMethod.HEAD,
    HttpMethod.OPTIONS,
    HttpMethod.TRACE,
    HttpMethod.CONNECT
  )

  private def rawRoute[In](method: HttpMethod, path: HttpPath[In]): HttpRoute[In, Any, Nothing] =
    HttpRoute(method, HttpRoute.RequestDef(path))

  private def staticResponse(path: String)(using Frame): HttpResponse["body" ~ Span[Byte]] < Sync =
    StaticAssets.load(path).map {
      case Result.Success(asset) =>
        val headers =
          if asset.html then securityHeaders.set("Content-Security-Policy", contentSecurityPolicy)
          else securityHeaders
        binary(HttpStatus.OK, asset.bytes, headers.set("Content-Type", asset.contentType))
      case Result.Failure(StaticAssets.Rejection.BadPath)  => binary(HttpStatus.BadRequest)
      case Result.Failure(StaticAssets.Rejection.NotFound) => binary(HttpStatus.NotFound)
      case Result.Panic(_)                                 => binary(HttpStatus.NotFound)
    }

  private def bridgeResponse(response: HttpJsonRpcBridge.Response): HttpResponse["body" ~ Span[Byte]] =
    val status  = HttpStatus(response.status)
    val bytes   = response.body.fold(Span.empty[Byte])(body => Span.fromUnsafe(body.getBytes(StandardCharsets.UTF_8)))
    val headers =
      if response.body.nonEmpty then response.headers.set("Content-Type", "application/json; charset=utf-8")
      else response.headers
    binary(status, bytes, headers)

  private def policyResponse(rejection: RequestPolicy.Rejection): HttpResponse["body" ~ Span[Byte]] =
    rejection match
      case RequestPolicy.Rejection.BadRequest      => binary(HttpStatus.BadRequest)
      case RequestPolicy.Rejection.Unauthorized    => binary(HttpStatus.Unauthorized)
      case RequestPolicy.Rejection.PayloadTooLarge => binary(HttpStatus.PayloadTooLarge)

  private def binary(
      status: HttpStatus,
      body: Span[Byte] = Span.empty,
      headers: HttpHeaders = securityHeaders
  ): HttpResponse["body" ~ Span[Byte]] =
    HttpResponse(status).copy(headers = headers).addField("body", body)

  private def validHost(headers: HttpHeaders, port: Int): Boolean =
    headers.getAll("Host") == Seq(s"$Loopback:$port")

  private def securityHeaders: HttpHeaders = RequestPolicy.responseHeaders

  private final class RootBootstrap(
      sessions: LaunchSessions,
      state: AtomicRef[RootBootstrap.State],
      mutex: Meter
  ):
    def decide(headers: HttpHeaders)(using Frame): RootBootstrap.Decision < (Async & Abort[Closed]) =
      mutex.run {
        RequestPolicy.authenticateSession(headers, sessions).map {
          case true =>
            state.set(RootBootstrap.State.NeedsLaunch).andThen(RootBootstrap.Decision.ServeHtml)
          case false =>
            state.get.map {
              case RootBootstrap.State.InitialHtml =>
                state.set(RootBootstrap.State.NeedsLaunch).andThen(RootBootstrap.Decision.ServeHtml)
              case RootBootstrap.State.NeedsLaunch =>
                sessions.createLaunch.map { launch =>
                  state.set(RootBootstrap.State.RedirectIssued).andThen(RootBootstrap.Decision.Redirect(launch))
                }
              case RootBootstrap.State.RedirectIssued =>
                state.set(RootBootstrap.State.NeedsLaunch).andThen(RootBootstrap.Decision.ServeHtml)
            }
        }
      }

    def launchFailed(using Frame): Unit < (Async & Abort[Closed]) =
      mutex.run(state.set(RootBootstrap.State.NeedsLaunch))

    def sessionEstablished(using Frame): Unit < (Async & Abort[Closed]) =
      mutex.run(state.set(RootBootstrap.State.NeedsLaunch))

  private object RootBootstrap:
    enum State:
      case InitialHtml, NeedsLaunch, RedirectIssued

    enum Decision:
      case ServeHtml
      case Redirect(launch: LaunchSessions.LaunchCredential)

    def init(sessions: LaunchSessions, initialHtml: Boolean)(using Frame): RootBootstrap < (Sync & Scope) =
      for
        state <- AtomicRef.init(if initialHtml then State.InitialHtml else State.NeedsLaunch)
        mutex <- Meter.initMutex
      yield RootBootstrap(sessions, state, mutex)
end WebHost
