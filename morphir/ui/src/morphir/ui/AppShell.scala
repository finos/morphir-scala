package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.layout
import morphir.ui.layout.{RegionVisibility, SettingsKey, ShellRoute}

/**
 * Application chrome shared by every morphir client: a full-width titlebar over a body of collapsible left, right and
 * bottom regions. The gear in the sidebar footer switches the whole surface to settings — its own section list beside
 * the matching settings content — and Back returns to the workspace. Composition only; the pieces live in
 * [[morphir.ui.layout]], styling in [[Theme]].
 *
 * Region visibility, the active route and the selected settings section live in a [[layout.ShellState]] of host-owned
 * [[kyo.SignalRef]]s. Views read its signals and call its commands; they never write a ref themselves.
 */
object AppShell:

  final case class NavItem(label: String, active: Boolean = false)

  /** A titled side/bottom region. */
  final case class Region(title: String, body: UI)

  /** One entry in the settings surface: the key that identifies it, its sidebar label, and its content. */
  final case class SettingsSection(key: SettingsKey, label: String, panels: Chunk[UI])

  type ShellState = layout.ShellState
  val ShellState = layout.ShellState

  def panel(title: String, body: UI): UI =
    section.cssClass(layout.Shell.Css.panel)(h2(title), body)

  def shell(
      sectionTitle: String,
      version: String,
      nav: Chunk[NavItem],
      panels: Chunk[UI],
      rightRegion: Region,
      bottomRegion: Region,
      state: ShellState,
      settingsSections: Chunk[SettingsSection] = Chunk.empty,
      customChrome: Boolean = false
  ): UI =

    def sectionFor(key: SettingsKey): Maybe[SettingsSection] =
      Maybe.fromOption(settingsSections.toSeq.find(_.key == key))

    def settingsLabel(key: SettingsKey): String =
      sectionFor(key).map(_.label).getOrElse("Settings")

    def settingsPanels(key: SettingsKey): Chunk[UI] =
      sectionFor(key).map(_.panels).getOrElse(Chunk.empty)

    def openSettings: Unit < Async =
      state.openSettings(settingsSections.headOption.map(_.key).getOrElse(SettingsKey("general")))

    def titlebar(route: ShellRoute, key: SettingsKey, leftVisibility: RegionVisibility): UI =
      val title = if route.isSettings then settingsLabel(key) else sectionTitle
      layout.Topbar.view(route, title, version, state, customChrome, leftVisibility)

    def sidebar(route: ShellRoute, key: SettingsKey): UI =
      if route.isSettings then layout.SettingsSidebar.view(settingsSections, key, state)
      else layout.Sidebar.view(nav, openSettings)

    def content(route: ShellRoute, key: SettingsKey): UI =
      if route.isSettings then
        div.cssClass(layout.Shell.Css.content).cssClass(layout.Shell.Css.settings).id("settings-content")(
          fragment(settingsPanels(key).toSeq*)
        )
      else div.cssClass(layout.Shell.Css.content)(fragment(panels.toSeq*))

    state.animations.render { motion =>
      state.colorScheme.render { scheme =>
        val themed = div.cssClass(layout.Shell.Css.app).cssClass(scheme.cssClass)
        val root   = if motion.isEnabled then themed else themed.cssClass(layout.Shell.Css.noMotion)
        root.id("app-root")(
          state.route.render { route =>
            state.settingsSection.render { key =>
              state.left.render(visibility => titlebar(route, key, visibility))
            }
          },
          div.cssClass(layout.Shell.Css.body)(
            state.route.render { route =>
              state.settingsSection.render(key => sidebar(route, key))
            },
            state.left.render { visibility =>
              if visibility.isCollapsed then div.cssClass("left-handle-hidden").hidden(true)
              else layout.ResizeHandle.column(layout.ResizeHandle.leftId)
            },
            div.cssClass(layout.Shell.Css.main)(
              state.route.render { route =>
                state.settingsSection.render(key => content(route, key))
              },
              state.route.render { route =>
                state.bottom.render { visibility =>
                  if route.isSettings || visibility.isCollapsed then div.cssClass("bottom-handle-hidden").hidden(true)
                  else layout.ResizeHandle.row(layout.ResizeHandle.bottomId)
                }
              },
              state.route.render { route =>
                if route.isSettings then div.cssClass("bottom-hidden").hidden(true)
                else layout.RegionPanel.bottom(bottomRegion)
              }
            ),
            state.route.render { route =>
              state.right.render { visibility =>
                if route.isSettings || visibility.isCollapsed then div.cssClass("right-handle-hidden").hidden(true)
                else layout.ResizeHandle.column(layout.ResizeHandle.rightId)
              }
            },
            state.route.render { route =>
              if route.isSettings then div.cssClass("right-hidden").hidden(true)
              else layout.RegionPanel.right(rightRegion)
            }
          )
        )
      }
    }

  /**
   * Wires the shell to the live DOM: pointer drags on the resize strips, and the animated region sizes. Hosts call this
   * once, before mounting.
   */
  def attachShellDom(state: ShellState): Unit < Async =
    morphir.ui.internal.PointerResize.attach(state).andThen(morphir.ui.internal.PanelMotion.attach(state))
