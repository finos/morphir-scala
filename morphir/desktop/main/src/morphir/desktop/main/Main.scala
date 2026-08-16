package morphir.desktop.main

import kyo.*
import morphir.appkit.electron.*
import scala.scalajs.js
import scala.scalajs.js.annotation.*

/**
 * Electron main-process entry. `app/main.cjs` requires the linked CommonJS bundle and calls [[start]] with the app
 * directory. Everything before the kyo program is imperative Electron lifecycle glue; the RPC handler itself runs as a
 * kyo fiber for the life of the process.
 */
object Main:

  private def smokeMode: Boolean =
    js.Dynamic.global.process.env.MORPHIR_SMOKE.asInstanceOf[js.UndefOr[String]].contains("1")

  /** Fires the renderer's end-of-smoke signal into app.quit when smoke mode is on; inert otherwise. */
  private val smokeDone =
    JsonRpcRoute.notification[String]("morphir/shell/smokeDone") { (_, _) =>
      Sync.defer(if smokeMode then ElectronApp.quit())
    }

  @JSExportTopLevel("start")
  def start(appDir: String): Unit =
    ElectronApp.whenReady {
      boot(appDir)
    }

  private def boot(appDir: String): Unit =
    val window = ElectronApp.createWindow(
      ElectronApp.WindowOptions(
        show = !smokeMode,
        preloadPath = Present(s"$appDir/preload.cjs"),
        chrome = ElectronApp.Chrome.Custom(trafficLightX = 16, trafficLightY = 20),
        backgroundColor = Present("#0f0d14")
      )
    )
    window.loadFile("index.html")

    val program =
      Scope.run {
        ElectronPorts.mainPort(window).map { port =>
          val transport = ElectronIpcTransport.fromPort(port)
          val routes    = DemoServices.routes(ElectronApp.appVersion) :+ smokeDone
          JsonRpcHandler.init(transport, routes*).map(_ => Async.never)
        }
      }

    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)
