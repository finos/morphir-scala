package morphir.desktop.renderer

import kyo.*
import kyo.UI.*
import morphir.appkit.electron.*
import morphir.ui.{AppShell, IrExplorerView, KnowledgeBrowserView, SchemePicker, SettingsView, Theme, Toggle}
import morphir.ui.layout.SettingsKey
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
          recents <- client.call[RecentWorkspacesRequest, RecentWorkspacesResponse](
            ShellRpc.methods.recentWorkspaces,
            RecentWorkspacesRequest()
          )
          bundles <- client.call[ListBundlesRequest, ListBundlesResponse](
            KnowledgeRpc.methods.listBundles,
            ListBundlesRequest(ws)
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
            settingsSections = settingsSections(version.version, recents.workspaces, bundles.bundles, state),
            customChrome = true
          )
          _ <- client.notify[String]("morphir/shell/smokeDone", "done")
          // Attach before mounting: runMount does not return while the app is on screen, and the drag
          // adapter listens on the document, so it needs no mounted element.
          _ <- AppShell.attachShellDom(state)
          _ <- UI.runMount(ui)
          _ <- Async.never
        yield ()
      }

    import AllowUnsafe.embrace.danger
    val _ = Sync.Unsafe.evalOrThrow(Fiber.initUnscoped(program).unit)

  /** The settings surface's sections. Values come from the same services the workspace uses. */
  private def settingsSections(
      version: String,
      recents: Chunk[WorkspaceRef],
      bundles: Chunk[BundleInfo],
      state: AppShell.ShellState
  ): Chunk[AppShell.SettingsSection] =
    Chunk(
      AppShell.SettingsSection(
        SettingsKey("general"),
        "General",
        Chunk(
          SettingsView.group(
            "Workspace",
            Chunk(
              SettingsView.Row.value("Active workspace", "The workspace the explorer reads from.", "/demo"),
              SettingsView.Row.value(
                "Recent workspaces",
                "Workspaces the shell offers on open.",
                recents.headOption.map(_.path).getOrElse("none")
              ),
              SettingsView.Row.value("Reopen on launch", "Restore the last workspace at startup.", "On")
            )
          )
        )
      ),
      AppShell.SettingsSection(
        SettingsKey("appearance"),
        "Appearance",
        Chunk(
          SettingsView.contentGroup(
            "Colour scheme",
            "Choose how morphir looks. System follows your operating system.",
            SchemePicker.view(state.colorScheme, state.selectColorScheme)
          ),
          SettingsView.group(
            "Theme",
            Chunk(
              SettingsView.Row.value("Accent", "Highlights, active nav and the version chip.", "magenta → violet"),
              SettingsView.Row.value("Window chrome", "Frameless window with app-drawn titlebar.", "Custom")
            )
          ),
          SettingsView.group(
            "Motion",
            Chunk(
              SettingsView.Row.control(
                "Panel animations",
                "Slide the side and bottom panels in and out.",
                Toggle.view("animations-toggle", state.animations.map(_.isEnabled), state.toggleAnimations)
              )
            )
          )
        )
      ),
      AppShell.SettingsSection(
        SettingsKey("services"),
        "Services",
        Chunk(
          SettingsView.group(
            "Morphir services",
            Chunk(
              SettingsView.Row.value("IR", "Packages, modules and definitions.", IrRpc.methods.listPackages),
              SettingsView.Row.value(
                "Knowledge",
                "Bundles, concepts and the intent index.",
                KnowledgeRpc.methods.intentIndex
              ),
              SettingsView.Row.value(
                "Shell",
                "Host affordances: dialogs, recents, version.",
                ShellRpc.methods.appVersion
              )
            )
          ),
          SettingsView.group(
            "Transport",
            Chunk(
              SettingsView.Row.value("Protocol", "One JSON-RPC envelope per message.", "kyo-jsonrpc"),
              SettingsView.Row.value("Wire", "Renderer to main over the preload bridge.", "Electron IPC"),
              SettingsView.Row.value("Listening ports", "The renderer never opens a socket.", "none")
            )
          )
        )
      ),
      AppShell.SettingsSection(
        SettingsKey("knowledge"),
        "Knowledge",
        Chunk(
          SettingsView.group(
            "Bundles",
            bundles.map(bundle =>
              SettingsView.Row.value(bundle.title, s"Bundle ${bundle.slug}.", s"${bundle.conceptCount} concepts")
            )
          )
        )
      ),
      AppShell.SettingsSection(
        SettingsKey("about"),
        "About",
        Chunk(
          SettingsView.group(
            "morphir-desktop",
            Chunk(
              SettingsView.Row.value("Application version", "Reported by the shell service.", version),
              SettingsView.Row.value("Client library", "Views and the service contract.", "morphir-ui"),
              SettingsView.Row.value(
                "Host integration",
                "Facades, transport and secret store.",
                "morphir-appkit-electron"
              )
            )
          )
        )
      )
    )
