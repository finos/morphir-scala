package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.icons.Icons

/**
 * Application chrome shared by every morphir client: collapsible left sidebar, right panel and bottom panel around a
 * central panel grid, with a topbar carrying the breadcrumb, version chip and the right/bottom toggles. Styled by
 * [[Theme.css]].
 *
 * All three collapsed states live in a [[ShellState]] of host-owned [[kyo.SignalRef]]s, so the shell stays a pure value
 * and the host decides whether the state is ephemeral or persisted.
 */
object AppShell:

  final case class NavItem(label: String, active: Boolean = false)

  /** A titled side/bottom region. */
  final case class Region(title: String, body: UI)

  /** Collapsed flags for the three shell regions. */
  final case class ShellState(
      left: SignalRef[Boolean],
      right: SignalRef[Boolean],
      bottom: SignalRef[Boolean]
  )

  object ShellState:
    def init(
        leftCollapsed: Boolean = false,
        rightCollapsed: Boolean = false,
        bottomCollapsed: Boolean = false
    ): ShellState < Sync =
      for
        left   <- Signal.initRef(leftCollapsed)
        right  <- Signal.initRef(rightCollapsed)
        bottom <- Signal.initRef(bottomCollapsed)
      yield ShellState(left, right, bottom)

  def panel(title: String, body: UI): UI =
    section.cssClass("panel")(h2(title), body)

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
    div.cssClass("app").id("app-root")(
      state.left.render { isCollapsed =>
        if isCollapsed then div.cssClass("sidebar-hidden").hidden(true)
        else fullSidebar(nav, state.left, onSettings, customChrome)
      },
      div.cssClass("main")(
        state.left.render { isCollapsed =>
          val leftGroup =
            if isCollapsed then
              div.cssClass("topbar-left")(
                div.cssClass("icon-btn").id("sidebar-toggle").onClick(state.left.set(false))(Icons.sidebar),
                div.cssClass("topbar-title")(span("morphir / ").cssClass("crumb"), sectionTitle)
              )
            else div.cssClass("topbar-title")(span("morphir / ").cssClass("crumb"), sectionTitle)
          val bar =
            if isCollapsed && customChrome then div.cssClass("topbar").cssClass("lights-inset")
            else div.cssClass("topbar")
          bar(
            leftGroup,
            div.cssClass("topbar-right")(
              span(s"v$version").cssClass("chip").id("app-version"),
              div.cssClass(
                "icon-btn"
              ).id("bottom-toggle").onClick(state.bottom.getAndUpdate(v => !v))(Icons.panelBottom),
              div.cssClass("icon-btn").id("right-toggle").onClick(state.right.getAndUpdate(v => !v))(Icons.panelRight)
            )
          )
        },
        div.cssClass("content")(fragment(panels.toSeq*)),
        state.bottom.render { isCollapsed =>
          if isCollapsed then div.cssClass("bottom-hidden").hidden(true)
          else
            div.cssClass("bottombar")(
              div.cssClass("region-head")(bottomRegion.title),
              div.cssClass("region-body")(bottomRegion.body)
            )
        }
      ),
      state.right.render { isCollapsed =>
        if isCollapsed then div.cssClass("right-hidden").hidden(true)
        else
          div.cssClass("rightbar")(
            div.cssClass("region-head")(rightRegion.title),
            div.cssClass("region-body")(rightRegion.body)
          )
      }
    )

  private def fullSidebar(
      nav: Chunk[NavItem],
      left: SignalRef[Boolean],
      onSettings: => Any < Async,
      customChrome: Boolean
  ): UI =
    div.cssClass("sidebar")(
      (if customChrome then div.cssClass("sidebar-head").cssClass("lights-inset").id("titlebar-drag")
       else div.cssClass("sidebar-head")) (
        div.cssClass("head-left")(
          div.cssClass("icon-btn").id("sidebar-toggle").onClick(left.set(true))(Icons.sidebar),
          div.cssClass("brand")(span("morphir").cssClass("brand-mark"), span("DESKTOP").cssClass("brand-sub"))
        )
      ),
      div.cssClass("nav-section")("Workspace"),
      fragment(nav.toSeq.map(_.navRow)*),
      div.cssClass("sidebar-foot")(
        div.cssClass("icon-btn").id("settings-button").onClick(onSettings)(Icons.gear),
        span("morphir-scala · kyo-ui").cssClass("foot-meta")
      )
    )

  extension (item: NavItem)
    private def navRow: UI =
      val base = div.cssClass("nav-item")
      val elem = if item.active then base.cssClass("active") else base
      elem(span("").cssClass("nav-dot"), item.label)
