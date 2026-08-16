package morphir.ui

import kyo.*
import kyo.test.*

class AppShellTests extends Test[Any]:

  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  def sampleShell(collapsed: SignalRef[Boolean], customChrome: Boolean = false): UI =
    AppShell.shell(
      sectionTitle = "Overview",
      version = "1.2.3",
      nav = Chunk(AppShell.NavItem("IR Explorer", active = true), AppShell.NavItem("Knowledge")),
      panels = Chunk(AppShell.panel("IR Packages", UI.p("body"))),
      collapsed = collapsed,
      customChrome = customChrome
    )

  "AppShell" - {

    "shell renders sidebar, nav, topbar chip, panels, toggle and settings" in
      Signal.initRef(false).map { collapsed =>
        renderOnce(sampleShell(collapsed)).map { html =>
          assert(
            html.contains("sidebar") && html.contains("nav-item") && html.contains("IR Explorer") &&
              html.contains("v1.2.3") && html.contains("panel") && html.contains("IR Packages") &&
              html.contains("settings-button") && html.contains("sidebar-toggle") && html.contains("<svg")
          )
        }
      }

    "collapsed shell hides the sidebar and moves the toggle into the topbar" in
      Signal.initRef(true).map { collapsed =>
        renderOnce(sampleShell(collapsed)).map { html =>
          assert(
            !html.contains("nav-item") && !html.contains("brand") && !html.contains("IR Explorer") &&
              html.contains("topbar-left") && html.contains("sidebar-toggle") && html.contains("IR Packages")
          )
        }
      }

    "toggling the ref re-renders the sidebar variant" in
      Signal.initRef(false).map { collapsed =>
        for
          first  <- renderOnce(sampleShell(collapsed))
          _      <- collapsed.set(true)
          second <- renderOnce(sampleShell(collapsed))
        yield assert(first.contains("IR Explorer") && !second.contains("IR Explorer") && second.contains("topbar-left"))
      }

    "custom chrome adds the titlebar drag spacer; default omits it" in
      Signal.initRef(false).map { collapsed =>
        for
          plain  <- renderOnce(sampleShell(collapsed))
          chrome <- renderOnce(sampleShell(collapsed, customChrome = true))
        yield assert(!plain.contains("titlebar-drag") && chrome.contains("titlebar-drag"))
      }
  }
end AppShellTests
