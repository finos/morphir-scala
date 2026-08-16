package morphir.ui

import kyo.*
import kyo.test.*

class AppShellTests extends Test[Any]:

  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  def sampleShell(collapsed: SignalRef[Boolean]): UI =
    AppShell.shell(
      sectionTitle = "Overview",
      version = "1.2.3",
      nav = Chunk(AppShell.NavItem("IR Explorer", active = true), AppShell.NavItem("Knowledge")),
      panels = Chunk(AppShell.panel("IR Packages", UI.p("body"))),
      collapsed = collapsed
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

    "collapsed shell renders the rail without nav labels" in
      Signal.initRef(true).map { collapsed =>
        renderOnce(sampleShell(collapsed)).map { html =>
          assert(
            html.contains("rail") && html.contains("rail-nav-item") &&
              !html.contains("IR Explorer") && html.contains("sidebar-toggle") && html.contains("settings-button")
          )
        }
      }

    "toggling the ref re-renders the sidebar variant" in
      Signal.initRef(false).map { collapsed =>
        for
          first  <- renderOnce(sampleShell(collapsed))
          _      <- collapsed.set(true)
          second <- renderOnce(sampleShell(collapsed))
        yield assert(first.contains("IR Explorer") && !second.contains("IR Explorer") && second.contains("rail"))
      }
  }
end AppShellTests
