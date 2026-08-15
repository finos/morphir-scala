package morphir.desktop.renderer

import kyo.*
import kyo.UI.*
import morphir.appkit.electron.*
import morphir.ui.{IrExplorerView, KnowledgeBrowserView}
import morphir.ui.services.*
import scala.scalajs.js
import scala.scalajs.js.annotation.*

/**
 * Renderer entry. `index.html` loads the linked script and calls `morphirRendererStart()`. The renderer never touches
 * Electron: its whole world is the `morphirIpc` bridge the preload exposed, lifted into an
 * [[morphir.appkit.electron.IpcPort]] and from there into the service contract.
 */
object RendererMain:

  private def bridgeFromWindow: IpcBridge =
    val raw = js.Dynamic.global.window.morphirIpc
    new IpcBridge:
      def postMessage(message: String): Unit                   = { val _ = raw.postMessage(message) }
      def onMessage(handler: js.Function1[String, Unit]): Unit = { val _ = raw.onMessage(handler) }

  @JSExportTopLevel("morphirRendererStart")
  def start(): Unit =
    val ws = WorkspaceRef("/demo")

    val program =
      Scope.run {
        for
          port    <- ElectronPorts.rendererPort(bridgeFromWindow)
          client  <- JsonRpcHandler.init(ElectronIpcTransport.fromPort(port))
          version <-
            client.call[AppVersionRequest, AppVersionResponse](ShellRpc.methods.appVersion, AppVersionRequest())
          packages <-
            client.call[ListPackagesRequest, ListPackagesResponse](IrRpc.methods.listPackages, ListPackagesRequest(ws))
          intents <- client.call[IntentIndexRequest, IntentIndexResponse](
            KnowledgeRpc.methods.intentIndex,
            IntentIndexRequest(ws)
          )
          ui = div(
            h1(s"Morphir Desktop ${version.version}").id("app-title"),
            section(h2("IR"), IrExplorerView.packageList(packages.packages)),
            section(h2("Knowledge"), KnowledgeBrowserView.intentTable(intents.intents))
          ).id("app-root")
          _ <- client.notify[String]("morphir/shell/smokeDone", "done")
          _ <- UI.runMount(ui)
          _ <- Async.never
        yield ()
      }

    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)
