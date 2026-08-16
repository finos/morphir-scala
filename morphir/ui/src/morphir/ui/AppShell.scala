package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.layout

/**
 * Application chrome shared by every morphir client: collapsible left sidebar, right panel and bottom panel around a
 * central panel grid, with a topbar carrying the breadcrumb, version chip and the right/bottom toggles. Composition
 * only — the pieces live in [[morphir.ui.layout]]; styling in [[Theme]].
 *
 * All three collapsed states live in a [[layout.ShellState]] of host-owned [[kyo.SignalRef]]s, so the shell stays a
 * pure value and the host decides whether the state is ephemeral or persisted.
 */
object AppShell:

  final case class NavItem(label: String, active: Boolean = false)

  /** A titled side/bottom region. */
  final case class Region(title: String, body: UI)

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
      onSettings: => Any < Async = (),
      customChrome: Boolean = false
  ): UI =
    div.cssClass(layout.Shell.Css.app).id("app-root")(
      state.left.render { visibility =>
        layout.Topbar.view(sectionTitle, version, state, customChrome, leftVisibility = visibility)
      },
      div.cssClass(layout.Shell.Css.body)(
        state.left.render { visibility =>
          if visibility.isCollapsed then div.cssClass("sidebar-hidden").hidden(true)
          else layout.Sidebar.view(nav, onSettings)
        },
        div.cssClass(layout.Shell.Css.main)(
          div.cssClass(layout.Shell.Css.content)(fragment(panels.toSeq*)),
          state.bottom.render { visibility =>
            if visibility.isCollapsed then div.cssClass("bottom-hidden").hidden(true)
            else layout.RegionPanel.bottom(bottomRegion)
          }
        ),
        state.right.render { visibility =>
          if visibility.isCollapsed then div.cssClass("right-hidden").hidden(true)
          else layout.RegionPanel.right(rightRegion)
        }
      )
    )
