package morphir.ui

import kyo.*
import kyo.test.*
import morphir.ui.layout.{ColorScheme, PanelBounds, SettingsKey, ShellRoute}
import morphir.ui.layout.RegionVisibility.Collapsed

class AppShellTests extends Test[Any]:

  val generalKey    = SettingsKey("general")
  val appearanceKey = SettingsKey("appearance")

  val settingsSections = Chunk(
    AppShell.SettingsSection(generalKey, "General", Chunk(SettingsView.group("Workspace", Chunk.empty))),
    AppShell.SettingsSection(
      appearanceKey,
      "Appearance",
      Chunk(SettingsView.group("Theme", Chunk(SettingsView.Row.value("Accent", "Highlight colour", "magenta"))))
    )
  )

  /** The signal's value right now: streamCurrent emits it, then would go on emitting changes. */
  def current(signal: Signal[Int]): Int < Async =
    signal.streamCurrent.take(1).run.map(_.head)

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

    "left collapse moves the toggle into the topbar" in
      AppShell.ShellState.init(left = Collapsed).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            !html.contains("brand-zone") &&
              html.contains("titlebar-left") && html.contains("sidebar-toggle") && html.contains("IR Packages")
          )
        }
      }

    "collapsed regions stay mounted so the slide can play" in
      AppShell.ShellState.init(right = Collapsed, bottom = Collapsed).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("rightbar") && html.contains("bottombar") &&
              html.contains("right-toggle") && html.contains("bottom-toggle")
          )
        }
      }

    "collapsing drives each region's extent to zero" in
      AppShell.ShellState.init().map { state =>
        for
          openLeft    <- current(state.leftExtent)
          _           <- state.left.set(Collapsed)
          closedLeft  <- current(state.leftExtent)
          _           <- state.right.set(Collapsed)
          closedRight <- current(state.rightExtent)
          _           <- state.bottom.set(Collapsed)
          closedFoot  <- current(state.bottomExtent)
        yield assert(openLeft == 224 && closedLeft == 0 && closedRight == 0 && closedFoot == 0)
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

    "a resized region reports its new extent" in
      AppShell.ShellState.init().map { state =>
        for
          _      <- state.resizeLeft(360)
          extent <- current(state.leftExtent)
        yield assert(extent == 360)
      }
  }

  "AppShell motion" - {

    "panels carry the shared slide duration" in {
      val css = Theme.css
      assert(
        css.contains(s"${morphir.ui.theme.Tokens.slideMs}ms") &&
          css.contains(".sidebar") && css.contains(".rightbar") && css.contains(".bottombar")
      )
    }

    "animations are on by default" in
      AppShell.ShellState.init().map { state =>
        for
          setting <- state.animations.get
          html    <- renderOnce(sampleShell(state))
        yield assert(setting == morphir.ui.layout.AnimationSetting.Enabled && !html.contains("no-motion"))
      }

    "the appearance toggle starts on" in
      AppShell.ShellState.init().map { state =>
        renderOnce(Toggle.view("animations-toggle", state.animations.map(_.isEnabled), state.toggleAnimations))
          .map(html => assert(html.contains("toggle on")))
      }

    "turning animations back on restores the animated root" in
      AppShell.ShellState.init().map { state =>
        for
          _        <- state.toggleAnimations
          off      <- renderOnce(sampleShell(state))
          _        <- state.toggleAnimations
          setting  <- state.animations.get
          restored <- renderOnce(sampleShell(state))
        yield assert(off.contains("no-motion") && setting.isEnabled && !restored.contains("no-motion"))
      }

    "toggling animations flips the root motion class" in
      AppShell.ShellState.init().map { state =>
        for
          animated <- renderOnce(sampleShell(state))
          _        <- state.toggleAnimations
          setting  <- state.animations.get
          still    <- renderOnce(sampleShell(state))
        yield assert(
          !animated.contains("no-motion") && !setting.isEnabled && still.contains("no-motion")
        )
      }

    "a settings row can carry a control instead of a value" in
      AppShell.ShellState.init().map { state =>
        val row = SettingsView.Row.control(
          "Animations",
          "Slide panels in and out.",
          Toggle.view("animations-toggle", state.animations.map(_.isEnabled), state.toggleAnimations)
        )
        renderOnce(SettingsView.group("Motion", Chunk(row))).map { html =>
          assert(html.contains("settings-control") && html.contains("animations-toggle") &&
            html.contains("toggle-knob"))
        }
      }

    "the toggle reflects the setting it reads" in
      AppShell.ShellState.init().map { state =>
        val toggle = Toggle.view("animations-toggle", state.animations.map(_.isEnabled), state.toggleAnimations)
        for
          on  <- renderOnce(toggle)
          _   <- state.toggleAnimations
          off <- renderOnce(toggle)
        yield assert(on.contains("toggle on") && !off.contains("toggle on"))
      }
  }

  "AppShell colour scheme" - {

    "the shell ships dark and says so on the root" in
      AppShell.ShellState.init().map { state =>
        for
          scheme <- state.colorScheme.get
          html   <- renderOnce(sampleShell(state))
        yield assert(scheme == ColorScheme.Dark && html.contains(morphir.ui.theme.Tokens.Scheme.dark))
      }

    "selecting a scheme repaints the root" in
      AppShell.ShellState.init().map { state =>
        for
          _      <- state.selectColorScheme(ColorScheme.Light)
          light  <- renderOnce(sampleShell(state))
          _      <- state.selectColorScheme(ColorScheme.System)
          system <- renderOnce(sampleShell(state))
        yield assert(
          light.contains(morphir.ui.theme.Tokens.Scheme.light) &&
            !light.contains(morphir.ui.theme.Tokens.Scheme.dark) &&
            system.contains(morphir.ui.theme.Tokens.Scheme.system)
        )
      }

    "the picker marks the active card and offers all three" in
      AppShell.ShellState.init(colorScheme = ColorScheme.Light).map { state =>
        renderOnce(SchemePicker.view(state.colorScheme, state.selectColorScheme)).map { html =>
          assert(
            html.contains("scheme-system") && html.contains("scheme-light") && html.contains("scheme-dark") &&
              html.contains("System") && html.contains("Light") && html.contains("Dark") &&
              html.contains("scheme-card active")
          )
        }
      }

    "restore defaults puts the scheme back to dark" in
      AppShell.ShellState.init().map { state =>
        for
          _      <- state.selectColorScheme(ColorScheme.Light)
          _      <- state.restoreDefaults
          scheme <- state.colorScheme.get
        yield assert(scheme == morphir.ui.layout.ShellDefaults.colorScheme)
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

    "the settings titlebar swaps region toggles for restore defaults" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("restore-defaults") && html.contains("Restore defaults") &&
              !html.contains("right-toggle") && !html.contains("bottom-toggle") && !html.contains("app-version")
          )
        }
      }

    "the workspace titlebar keeps the region toggles and the version chip" in
      AppShell.ShellState.init().map { state =>
        renderOnce(sampleShell(state)).map { html =>
          assert(
            html.contains("right-toggle") && html.contains("bottom-toggle") && html.contains("app-version") &&
              !html.contains("restore-defaults")
          )
        }
      }

    "restore defaults puts every region and the animation setting back" in
      AppShell.ShellState.init(route = ShellRoute.Settings).map { state =>
        for
          _         <- state.right.set(Collapsed)
          _         <- state.bottom.set(Collapsed)
          _         <- state.resizeLeft(400)
          _         <- state.toggleAnimations
          _         <- state.restoreDefaults
          leftSize  <- current(state.leftExtent)
          rightSize <- current(state.rightExtent)
          footSize  <- current(state.bottomExtent)
          motion    <- state.animations.get
          route     <- state.route.get
        yield assert(
          leftSize == morphir.ui.layout.ShellDefaults.leftWidth &&
            rightSize == morphir.ui.layout.ShellDefaults.rightWidth &&
            footSize == morphir.ui.layout.ShellDefaults.bottomHeight &&
            motion.isEnabled && route == ShellRoute.Settings
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
