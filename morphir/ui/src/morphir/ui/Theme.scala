package morphir.ui

import kyo.*
import morphir.ui.layout.{RegionPanel, Shell, Sidebar, Topbar}
import morphir.ui.theme.{Base, Tokens}

/**
 * The morphir client theme, aggregated for hosts: the typed stylesheets of the theme and layout packages rendered to
 * one string, plus two raw blocks. `legacyCss` holds the component rules still awaiting migration; `quarantineCss`
 * holds only properties the typed vocabulary cannot express at RC6 and is meant to stay tiny.
 */
object Theme:

  def sheet: Stylesheet =
    Tokens.sheet ++ Base.sheet ++ Shell.sheet ++ Sidebar.sheet ++ Topbar.sheet ++ RegionPanel.sheet

  /** Each rule here names the missing typed vocabulary that forces it to stay raw. */
  private val quarantineCss: String =
    """
      |/* -webkit-app-region: no typed property (frameless-window drag regions). */
      |.sidebar-head, .topbar { -webkit-app-region: drag; }
      |.icon-btn, .nav-item, .chip { -webkit-app-region: no-drag; }
      |/* CSS grid: no typed vocabulary at RC6. */
      |.content {
      |  flex: 1; overflow: auto; padding: 22px;
      |  display: grid; grid-template-columns: repeat(auto-fit, minmax(380px, 1fr)); gap: 16px;
      |  align-content: start;
      |}
      |/* inset box-shadow: Style.shadow has no inset arm. */
      |.nav-item.active { box-shadow: inset 2px 0 0 var(--accent); }
      |/* background-clip: text (gradient text) is not in the BackgroundClip enum. */
      |.brand-mark {
      |  background: linear-gradient(120deg, var(--accent), var(--accent2));
      |  -webkit-background-clip: text; background-clip: text; color: transparent;
      |}
      |""".stripMargin

  private val legacyCss: String =
    """
      |#ir-packages, #ir-modules, #kb-bundles { list-style: none; }
      |#ir-packages li, #ir-modules li, #kb-bundles li {
      |  padding: 9px 4px; border-bottom: 1px solid #221d2e; font: 13px var(--mono);
      |}
      |#ir-packages li:last-child, #ir-modules li:last-child, #kb-bundles li:last-child { border-bottom: 0; }
      |#ir-packages li:hover, #ir-modules li:hover { background: #1f1a29; }
      |#kb-intents { width: 100%; border-collapse: collapse; font-size: 13px; }
      |#kb-intents th {
      |  text-align: left; font: 600 10px var(--mono); letter-spacing: 0.14em; text-transform: uppercase;
      |  color: var(--muted2); padding: 4px 10px 10px 4px; border-bottom: 1px solid var(--panel-edge);
      |}
      |#kb-intents td { padding: 9px 10px 9px 4px; border-bottom: 1px solid #221d2e; }
      |#kb-intents tr:last-child td { border-bottom: 0; }
      |#kb-intents td:first-child { font: 600 12px var(--mono); color: var(--accent2); }
      |#kb-intents td:nth-child(3) { font: 600 11px var(--mono); color: #f2b7dd; }
      |#kb-intents td:nth-child(4) { font: 12px var(--mono); color: var(--muted2); }
      |#ir-definition pre {
      |  background: #131019; border: 1px solid #241f30; border-radius: 8px;
      |  padding: 12px 14px; font: 12.5px var(--mono); overflow-x: auto;
      |}
      """.stripMargin

  def css: String =
    sheet.render + "\n" + Base.rawCss + quarantineCss + legacyCss
