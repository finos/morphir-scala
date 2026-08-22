package morphir.web.renderer

import kyo.*
import kyo.UI.*
import morphir.ui.github.{GitHubConnectionStore, GitHubConnectionView}
import morphir.ui.services.*
import org.scalajs.dom

object Main:

  private val LaunchPattern = "#launch=([A-Za-z0-9_-]{43})".r

  private[renderer] trait BrowserAdapter:
    def fragment(using Frame): String < Sync
    def removeFragment(using Frame): Unit < Sync
    def exchange(launch: String)(using Frame): Unit < (Async & Abort[Closed])
    def initializeApplication(using Frame): Unit < (Async & Scope & Abort[Closed])

  private[renderer] def start(browser: BrowserAdapter)(using Frame): Unit < (Async & Scope & Abort[Closed]) =
    browser.fragment.map {
      case LaunchPattern(launch) =>
        browser.removeFragment.andThen(browser.exchange(launch)).andThen(browser.initializeApplication)
      case "" => browser.initializeApplication
      case _  =>
        browser.removeFragment.andThen(Abort.fail(new Closed("Morphir web session", summon[Frame])))
    }

  def main(args: Array[String]): Unit =
    val _       = args
    val program = Scope.run(start(LiveBrowser))
    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)

  private object LiveBrowser extends BrowserAdapter:
    def fragment(using Frame): String < Sync =
      Sync.defer(dom.window.location.hash)

    def removeFragment(using Frame): Unit < Sync = Sync.defer {
      val location = dom.window.location
      dom.window.history.replaceState(null, dom.document.title, location.pathname + location.search)
    }

    def exchange(launch: String)(using Frame): Unit < (Async & Abort[Closed]) =
      BrowserSession.exchange(launch)

    def initializeApplication(using Frame): Unit < (Async & Scope & Abort[Closed]) =
      for
        transport <- FetchJsonRpcTransport()
        client    <- JsonRpcHandler.init(transport)
        github    <- GitHubConnectionStore.init(remoteGitHub(client, transport))
        _         <- github.load
        ui = connectionSettings(github)
        _ <- UI.runMount(ui, "#app")
      yield ()

  private[renderer] def connectionSettings(github: GitHubConnectionStore): UI =
    GitHubConnectionView.view(
      github.state,
      (submission, remember) => github.connect(submission, remember),
      github.disconnect
    )

  private[renderer] def remoteGitHub(
      client: JsonRpcHandler,
      transport: FetchJsonRpcTransport.SessionTransport
  ): GitHubConnectionService = new GitHubConnectionService:
    def status(): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      call(
        client.call[StatusRequest, StatusResponse](
          GitHubConnectionRpc.methods.status,
          StatusRequest()
        ),
        transport
      ).map(_.status)

    def connect(
        submission: TokenSubmission,
        remember: Boolean
    ): GitHubConnectionStatus < (Async & Abort[GitHubConnectionError]) =
      call(
        client.call[ConnectRequest, ConnectResponse](
          GitHubConnectionRpc.methods.connect,
          ConnectRequest(submission, remember)
        ),
        transport
      ).map(_.status)

    def disconnect(): Unit < (Async & Abort[GitHubConnectionError]) =
      call(
        client.call[DisconnectRequest, DisconnectResponse](
          GitHubConnectionRpc.methods.disconnect,
          DisconnectRequest()
        ),
        transport
      ).unit

  private def call[A](
      operation: A < (Async & Abort[JsonRpcError | Closed]),
      transport: FetchJsonRpcTransport.SessionTransport
  ): A < (Async & Abort[GitHubConnectionError]) =
    Abort.run[JsonRpcError | Closed](operation).map {
      case Result.Success(value)                                                                           => value
      case Result.Failure(error: JsonRpcImplementationError) if error.code == GitHubConnectionRpc.wireCode =>
        error.data match
          case Present(data) =>
            Structure.decode[GitHubConnectionError](data) match
              case Result.Success(githubError) => Abort.fail(githubError)
              case _                           => Abort.fail(GitHubConnectionError.GitHubUnavailable)
          case Absent => Abort.fail(GitHubConnectionError.GitHubUnavailable)
      case Result.Failure(_: Closed) =>
        transport.terminalCause.map {
          case Present(FetchJsonRpcTransport.TerminalCause.Unauthorized) =>
            Abort.fail(GitHubConnectionError.ExpiredLocalSession)
          case _ => Abort.fail(GitHubConnectionError.GitHubUnavailable)
        }
      case Result.Failure(_) | Result.Panic(_) =>
        Abort.fail(GitHubConnectionError.GitHubUnavailable)
    }
end Main
