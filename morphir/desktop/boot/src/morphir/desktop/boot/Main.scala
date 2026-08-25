package morphir.desktop.boot

import morphir.desktop.main.{DemoServices, Startup}

import kyo.*
import morphir.appkit.*
import morphir.appkit.electron.*
import morphir.connector.github.*
import morphir.ui.github.GitHubConnectionCoordinator
import morphir.ui.services.GitHubConnectionError
import scala.scalajs.js.annotation.*

/**
 * Electron main-process entry. `app/main.cjs` requires the linked CommonJS bundle and calls [[start]] with the app
 * directory. Everything before the kyo program is imperative Electron lifecycle glue; the RPC handler itself runs as a
 * kyo fiber for the life of the process.
 */
object Main:

  @JSExportTopLevel("start")
  def start(appDir: String): Unit =
    ElectronApp.whenReady {
      boot(appDir, LiveAssembly, show = true)
    }

  /** Explicit test entry used only by `app/run-capture.cjs`; ordinary startup always uses [[LiveAssembly]]. */
  @JSExportTopLevel("startSmoke")
  def startSmoke(appDir: String): Unit =
    ElectronApp.whenReady {
      boot(appDir, SmokeAssembly, show = false)
    }

  private def boot(appDir: String, assembly: ConnectionAssembly, show: Boolean): Unit =
    val window = ElectronApp.createWindow(
      ElectronApp.WindowOptions(
        show = show,
        preloadPath = Present(s"$appDir/preload.cjs"),
        chrome = ElectronApp.Chrome.Custom(trafficLightX = 16, trafficLightY = 18),
        backgroundColor = Present("#0f0d14")
      )
    )

    val program =
      Scope.run {
        Startup.failClosed[GitHubConnectionError, Async & Scope](
          Startup
            .initialize(
              assembly.github,
              github =>
                ElectronPorts.mainPort(window).map { port =>
                  val transport = ElectronIpcTransport.fromPort(port)
                  val routes    = DemoServices.routes(ElectronApp.appVersion, github)
                  JsonRpcHandler.init(transport, routes*).unit
                },
              Sync.defer(window.loadFile("index.html"))
            )
            .map(_ => Async.never),
          Sync.defer(ElectronApp.quit())
        )
      }

    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)

  private trait ConnectionAssembly:
    def github: GitHubConnectionCoordinator < (Async & Abort[GitHubConnectionError])

  private object LiveAssembly extends ConnectionAssembly:
    def github: GitHubConnectionCoordinator < (Async & Abort[GitHubConnectionError]) =
      ElectronSecretVault.system.map { vault =>
        GitHubConnectionCoordinator.init(GitHubTokenVerifier.live, vault)
      }

  private object SmokeAssembly extends ConnectionAssembly:
    def github: GitHubConnectionCoordinator < (Async & Abort[GitHubConnectionError]) =
      GitHubConnectionCoordinator.init(SmokeVerifier(), Present(SmokeVault()))

  private final class SmokeVerifier extends GitHubTokenVerifier:
    private var attempts = 0

    def verify(token: Token): GitHubLogin < (Abort[GitHubException] & Async) =
      val _ = token
      Sync.defer {
        attempts += 1
        attempts
      }.map {
        case 1 => Abort.fail(GitHubException.Unauthorized("Stored smoke credential rejected"))
        case 2 => Async.sleep(1500.millis).andThen(gitHubLogin"smoke-user")
        case 3 => Async.sleep(1500.millis).andThen(gitHubLogin"smoke-user")
        case _ =>
          Async.sleep(1500.millis).andThen(
            Abort.fail(GitHubException.Unauthorized("Submitted smoke credential rejected"))
          )
      }

  private final class SmokeVault extends SecretVault:
    private var stored: Maybe[Secret] = Secret.fromStored("smoke-stored-credential")

    def get(service: String, account: String): Maybe[Secret] < (Abort[SecretException] & Async) =
      Async.sleep(5000.millis).andThen(stored)

    def put(service: String, account: String, secret: Secret): Unit < (Abort[SecretException] & Async) =
      Sync.defer {
        stored = Present(secret)
      }

    def remove(service: String, account: String): Unit < (Abort[SecretException] & Async) =
      Sync.defer {
        stored = Absent
      }
