package morphir.ui

import kyo.*
import morphir.ui.github.GitHubConnectionView
import morphir.ui.layout.{RegionPanel, ResizeHandle, SettingsSidebar, Shell, Sidebar, Topbar}
import morphir.ui.{IrExplorerView, KnowledgeBrowserView, SchemePicker, SettingsView, Toggle}
import morphir.ui.theme.{Base, Tokens}

/**
 * The morphir client theme, aggregated for hosts: the typed stylesheets of the theme and layout packages rendered to
 * one string, plus two raw blocks. `legacyCss` holds the component rules still awaiting migration; `quarantineCss`
 * holds only properties the typed vocabulary cannot express at RC6 and is meant to stay tiny.
 */
object Theme:

  def sheet: Stylesheet =
    Tokens.sheet ++ Base.sheet ++ Shell.sheet ++ Sidebar.sheet ++ Topbar.sheet ++ RegionPanel.sheet ++
      ResizeHandle.sheet ++
      SettingsSidebar.sheet ++ SettingsView.sheet ++ Toggle.sheet ++ SchemePicker.sheet ++ IrExplorerView.sheet ++
      KnowledgeBrowserView.sheet ++ GitHubConnectionView.sheet

  /** Each rule here names the missing typed vocabulary that forces it to stay raw. */
  private val quarantineCss: String =
    """
      |/* -webkit-app-region: no typed property (frameless-window drag regions). */
      |.titlebar { -webkit-app-region: drag; }
      |.icon-btn, .nav-item, .chip { -webkit-app-region: no-drag; }
      |/* CSS grid: no typed vocabulary at RC6. */
      |.content {
      |  flex: 1; overflow: auto; padding: 22px;
      |  display: grid; grid-template-columns: repeat(auto-fit, minmax(380px, 1fr)); gap: 16px;
      |  align-content: start;
      |}
      |/* The settings surface stacks its groups in one column. */
      |.content.content-settings { grid-template-columns: minmax(0, 1fr); gap: 0; }
      |/* Disabling motion needs a universal selector and !important to beat the typed per-element
      | * transitions; neither is expressible in Style. */
      |.no-motion *, .no-motion *::before, .no-motion *::after {
      |  transition: none !important; animation: none !important;
      |}
      |/* col-resize / row-resize are not in the typed Cursor enum; the body classes keep the cursor
      | * steady while a drag outruns the 5px strip. */
      |.resize-vertical { cursor: col-resize; }
      |.resize-horizontal { cursor: row-resize; }
      |body.resizing-col, body.resizing-col * { cursor: col-resize; user-select: none; }
      |body.resizing-row, body.resizing-row * { cursor: row-resize; user-select: none; }
      |/* inset box-shadow: Style.shadow has no inset arm. */
      |.nav-item.active { box-shadow: inset 2px 0 0 var(--accent); }
      |/* background-clip: text (gradient text) is not in the BackgroundClip enum. */
      |.brand-mark {
      |  background: linear-gradient(120deg, var(--accent), var(--accent2));
      |  -webkit-background-clip: text; background-clip: text; color: transparent;
      |}
      |/* Reactive connection state replaces only the inert marker so password inputs stay mounted. */
      |.github-connection:has([data-github-state="disconnected-idle"]) #github-connected,
      |.github-connection:has([data-github-state="disconnected-idle"]) #github-rejected,
      |.github-connection:has([data-github-state="disconnected-idle"]) #github-connect-busy,
      |.github-connection:has([data-github-state="disconnected-busy"]) #github-connected,
      |.github-connection:has([data-github-state="disconnected-busy"]) #github-rejected,
      |.github-connection:has([data-github-state="disconnected-busy"]) #github-connect-idle,
      |.github-connection:has([data-github-state="connected-idle"]) #github-disconnected,
      |.github-connection:has([data-github-state="connected-idle"]) #github-rejected,
      |.github-connection:has([data-github-state="connected-idle"]) #github-disconnect-busy,
      |.github-connection:has([data-github-state="connected-busy"]) #github-disconnected,
      |.github-connection:has([data-github-state="connected-busy"]) #github-rejected,
      |.github-connection:has([data-github-state="connected-busy"]) #github-disconnect-idle,
      |.github-connection:has([data-github-state="rejected-idle"]) #github-disconnected,
      |.github-connection:has([data-github-state="rejected-idle"]) #github-connected,
      |.github-connection:has([data-github-state="rejected-idle"]) #github-replace-busy,
      |.github-connection:has([data-github-state="rejected-busy"]) #github-disconnected,
      |.github-connection:has([data-github-state="rejected-busy"]) #github-connected,
      |.github-connection:has([data-github-state="rejected-busy"]) #github-replace-idle {
      |  display: none;
      |}
      |""".stripMargin

  def css: String =
    sheet.render + "\n" + Base.rawCss + quarantineCss
