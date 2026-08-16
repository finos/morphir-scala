package morphir.ui

import kyo.*
import kyo.test.*
import morphir.ui.layout.RegionVisibility.Collapsed

class AppShellTests extends Test[Any]:

  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  def sampleShell(state: AppShell.ShellState, customChrome: Boolean = false): UI =
    AppShell.shell(
      sectionTitle = "Overview",
      version = "1.2.3",
      nav = Chunk(AppShell.NavItem("IR Explorer", active = true), AppShell.NavItem("Knowledge")),
      panels = Chunk(AppShell.panel("IR Packages", UI.p("body"))),
      rightRegion = AppShell.Region("Inspector", UI.p("right-body")),
      bottomRegion = AppShell.Region("Log", UI.p("bottom-body")),
      state = state,
      customChrome = customChrome
    )

  "AppShell" - {

    "expanded shell renders all three regions with their toggles" in
      AppShell.ShellState.init().map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("nav-item") && html.contains("IR Explorer") && html.contains("v1.2.3") &&
              html.contains("sidebar-toggle") && html.contains("right-toggle") && html.contains("bottom-toggle") &&
              html.contains("rightbar") && html.contains("Inspector") &&
              html.contains("bottombar") && html.contains("Log") && html.contains("settings-button")
          )
        }
      }

    "left collapse hides the sidebar and moves the toggle into the topbar" in
      AppShell.ShellState.init(left = Collapsed).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            !html.contains("nav-item") && !html.contains("brand") &&
              html.contains("titlebar-left") && html.contains("sidebar-toggle") && html.contains("IR Packages")
          )
        }
      }

    "right and bottom collapse hide their regions but keep the toggles" in
      AppShell.ShellState.init(right = Collapsed, bottom = Collapsed).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            !html.contains("Inspector") && !html.contains("bottombar") &&
              html.contains("right-toggle") && html.contains("bottom-toggle")
          )
        }
      }

    "toggling the refs re-renders each region" in
      AppShell.ShellState.init().map { state =>
        for
          first  <- renderOnce(sampleShell(state))
          _      <- state.left.set(Collapsed)
          _      <- state.right.set(Collapsed)
          _      <- state.bottom.set(Collapsed)
          second <- renderOnce(sampleShell(state))
        yield assert(
          first.contains("IR Explorer") && first.contains("Inspector") && first.contains("bottombar") &&
            !second.contains("IR Explorer") && !second.contains("Inspector") && !second.contains("bottombar")
        )
      }

    "custom chrome inserts the lights inset; default omits it" in
      AppShell.ShellState.init().map { state =>
        for
          plain  <- renderOnce(sampleShell(state))
          chrome <- renderOnce(sampleShell(state, customChrome = true))
        yield assert(!plain.contains("lights-inset") && chrome.contains("lights-inset"))
      }
  }
end AppShellTests
