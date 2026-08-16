package morphir.desktop.renderer

import kyo.*
import kyo.UI.*
import morphir.appkit.electron.*
import morphir.ui.{AppShell, IrExplorerView, KnowledgeBrowserView, Theme}
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

  private def injectTheme(): Unit =
    val doc   = js.Dynamic.global.document
    val style = doc.createElement("style")
    style.textContent = Theme.css
    val _ = doc.head.appendChild(style)

  @JSExportTopLevel("morphirRendererStart")
  def start(): Unit =
    injectTheme()
    val ws = WorkspaceRef("/demo")

    val program =
      Scope.run {
        for
          state   <- AppShell.ShellState.init()
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
          definition <- client.call[DefinitionRequest, DefinitionResponse](
            IrRpc.methods.definition,
            DefinitionRequest(ws, DefinitionRef("Morphir.SDK", "List", "map"))
          )
          ui = AppShell.shell(
            sectionTitle = "Overview",
            version = version.version,
            nav = Chunk(
              AppShell.NavItem("Overview", active = true),
              AppShell.NavItem("IR Explorer"),
              AppShell.NavItem("Knowledge"),
              AppShell.NavItem("Intents")
            ),
            panels = Chunk(
              AppShell.panel("IR Packages", IrExplorerView.packageList(packages.packages)),
              AppShell.panel("Intent Lifecycle", KnowledgeBrowserView.intentTable(intents.intents))
            ),
            rightRegion = AppShell.Region("Inspector", IrExplorerView.definitionCard(definition.definition)),
            bottomRegion = AppShell.Region(
              "Log",
              UI.pre(
                s"morphir-rpc connected\nservices: ir, knowledge, shell\napp version ${version.version}\nworkspace ${ws.path}"
              )
            ),
            state = state,
            customChrome = true
          )
          _ <- client.notify[String]("morphir/shell/smokeDone", "done")
          _ <- UI.runMount(ui)
          _ <- Async.never
        yield ()
      }

    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)
