package morphir.ui

import kyo.*
import kyo.test.*

class AppShellTests extends Test[Any]:

  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  "AppShell" - {

    "shell renders sidebar, nav, topbar chip and panels" in {
      val ui = AppShell.shell(
        sectionTitle = "Overview",
        version = "1.2.3",
        nav = Chunk(AppShell.NavItem("IR Explorer", active = true), AppShell.NavItem("Knowledge")),
        panels = Chunk(AppShell.panel("IR Packages", UI.p("body")))
      )
      renderOnce(ui).map { html =>
        assert(
          html.contains("sidebar") && html.contains("nav-item") && html.contains("IR Explorer") &&
            html.contains("v1.2.3") && html.contains("panel") && html.contains("IR Packages") &&
            html.contains("settings-button") && html.contains("<svg")
        )
      }
    }
  }
end AppShellTests
