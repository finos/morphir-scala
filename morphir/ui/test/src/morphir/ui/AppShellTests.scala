package morphir.ui

import kyo.*
import kyo.test.*
import morphir.ui.layout.{PanelBounds, SettingsKey, ShellRoute}
import morphir.ui.layout.RegionVisibility.Collapsed

class AppShellTests extends Test[Any]:

  val generalKey    = SettingsKey("general")
  val appearanceKey = SettingsKey("appearance")

  val settingsSections = Chunk(
    AppShell.SettingsSection(generalKey, "General", Chunk(SettingsView.group("Workspace", Chunk.empty))),
    AppShell.SettingsSection(
      appearanceKey,
      "Appearance",
      Chunk(SettingsView.group("Theme", Chunk(SettingsView.Row("Accent", "Highlight colour", "magenta"))))
    )
  )

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
      settingsSections = settingsSections,
      customChrome = customChrome
    )

  "AppShell" - {

    "expanded shell renders all three regions with their toggles" in
      AppShell.ShellState.init().map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("app-body") && html.contains("brand-zone") && html.contains("nav-item") &&
              html.contains("IR Explorer") && html.contains("v1.2.3") &&
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

  "AppShell resizing" - {

    "each region renders its own drag strip" in
      AppShell.ShellState.init().map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("left-resize") && html.contains("right-resize") && html.contains("bottom-resize") &&
              html.contains("resize-vertical") && html.contains("resize-horizontal")
          )
        }
      }

    "a collapsed region hides its strip with the region" in
      AppShell.ShellState.init(right = Collapsed).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(html.contains("left-resize") && !html.contains("right-resize"))
        }
      }

    "resize commands clamp into the region bounds" in
      AppShell.ShellState.init().map { state =>
        for
          _         <- state.resizeLeft(40)
          tooNarrow <- state.leftWidth.get
          _         <- state.resizeLeft(9000)
          tooWide   <- state.leftWidth.get
          _         <- state.resizeLeft(300)
          inRange   <- state.leftWidth.get
        yield assert(
          tooNarrow.px == PanelBounds.left.min && tooWide.px == PanelBounds.left.max && inRange.px == 300
        )
      }

    "each region clamps against its own bounds" in
      AppShell.ShellState.init().map { state =>
        for
          _      <- state.resizeRight(10)
          right  <- state.rightWidth.get
          _      <- state.resizeBottom(9000)
          bottom <- state.bottomHeight.get
        yield assert(right.px == PanelBounds.right.min && bottom.px == PanelBounds.bottom.max)
      }

    "a resized region renders at its new size" in
      AppShell.ShellState.init().map { state =>
        for
          _    <- state.resizeLeft(360)
          html <- renderOnce(sampleShell(state))
        yield assert(html.contains("360px"))
      }
  }

  "AppShell settings surface" - {

    "openSettings routes to settings and lands on the given section" in
      AppShell.ShellState.init().map { state =>
        for
          _       <- state.openSettings(appearanceKey)
          route   <- state.route.get
          section <- state.settingsSection.get
        yield assert(route == ShellRoute.Settings && section == appearanceKey)
      }

    "closeSettings returns to the workspace" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        state.closeSettings.andThen(state.route.get).map(route => assert(route == ShellRoute.Workspace))
      }

    "the settings route renders the section list, back row and section content" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("settings-item") && html.contains("General") && html.contains("Appearance") &&
              html.contains("settings-back") && html.contains("settings-content") &&
              html.contains("Settings /") && html.contains("Workspace") &&
              !html.contains("IR Explorer") && !html.contains("Inspector") && !html.contains("bottombar")
          )
        }
      }

    "selecting a section swaps the content" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        for
          first  <- renderOnce(sampleShell(state))
          _      <- state.selectSettingsSection(appearanceKey)
          second <- renderOnce(sampleShell(state))
        yield assert(
          first.contains("Workspace") && !first.contains("Highlight colour") &&
            second.contains("Highlight colour") && second.contains("Settings / ")
        )
      }

    "leaving settings restores the workspace surface" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        for
          settings  <- renderOnce(sampleShell(state))
          _         <- state.closeSettings
          workspace <- renderOnce(sampleShell(state))
        yield assert(
          settings.contains("settings-back") &&
            !workspace.contains("settings-back") && workspace.contains("IR Explorer") &&
            workspace.contains("Inspector")
        )
      }
  }
end AppShellTests
