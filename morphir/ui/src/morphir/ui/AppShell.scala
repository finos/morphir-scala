package morphir.ui

import kyo.*
import kyo.UI.*

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
                div.cssClass("icon-btn").id("sidebar-toggle").onClick(state.left.set(false))(sidebarIcon),
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
              div.cssClass("icon-btn").id("bottom-toggle").onClick(state.bottom.getAndUpdate(v => !v))(panelBottomIcon),
              div.cssClass("icon-btn").id("right-toggle").onClick(state.right.getAndUpdate(v => !v))(panelRightIcon)
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
          div.cssClass("icon-btn").id("sidebar-toggle").onClick(left.set(true))(sidebarIcon),
          div.cssClass("brand")(span("morphir").cssClass("brand-mark"), span("DESKTOP").cssClass("brand-sub"))
        )
      ),
      div.cssClass("nav-section")("Workspace"),
      fragment(nav.toSeq.map(_.navRow)*),
      div.cssClass("sidebar-foot")(
        div.cssClass("icon-btn").id("settings-button").onClick(onSettings)(gearIcon),
        span("morphir-scala · kyo-ui").cssClass("foot-meta")
      )
    )

  private def strokedRect: Svg.Rect =
    Svg.rect
      .fill(Svg.Paint.None)
      .stroke(Svg.Paint.CurrentColor)
      .strokeWidth(1.6)
      .x(3)
      .y(3)
      .width(18)
      .height(18)
      .rx(3)

  private def strokedLine(x1: Double, y1: Double, x2: Double, y2: Double): Svg.Line =
    Svg.line
      .stroke(Svg.Paint.CurrentColor)
      .strokeWidth(1.6)
      .strokeLinecap(Svg.StrokeLinecap.Round)
      .x1(x1)
      .y1(y1)
      .x2(x2)
      .y2(y2)

  /** Left-sidebar toggle: panel outline with a list-marked left column (T3-code style). */
  private def sidebarIcon: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(9.5, 3, 9.5, 21),
      strokedLine(5.5, 8, 7, 8),
      strokedLine(5.5, 12, 7, 12)
    )

  /** Right-panel toggle: panel outline with the divider on the right. */
  private def panelRightIcon: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(14.5, 3, 14.5, 21)
    )

  /** Bottom-panel toggle: panel outline with the divider along the bottom. */
  private def panelBottomIcon: UI =
    Svg.svg.viewBox(Svg.ViewBox(0, 0, 24, 24)).width(16).height(16)(
      strokedRect,
      strokedLine(3, 14.5, 21, 14.5)
    )

  /** Lucide `settings` glyph, stroke-drawn in the current text color. */
  private def gearIcon: UI =
    Svg.svg
      .viewBox(Svg.ViewBox(0, 0, 24, 24))
      .width(16)
      .height(16)(
        Svg.path
          .fill(Svg.Paint.None)
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .strokeLinecap(Svg.StrokeLinecap.Round)
          .strokeLinejoin(Svg.StrokeLinejoin.Round)
          .d(
            Svg.PathData.raw(
              "M12.22 2h-.44a2 2 0 0 0-2 2v.18a2 2 0 0 1-1 1.73l-.43.25a2 2 0 0 1-2 0l-.15-.08a2 2 0 0 0-2.73.73l-.22.38a2 2 0 0 0 .73 2.73l.15.1a2 2 0 0 1 1 1.72v.51a2 2 0 0 1-1 1.74l-.15.09a2 2 0 0 0-.73 2.73l.22.38a2 2 0 0 0 2.73.73l.15-.08a2 2 0 0 1 2 0l.43.25a2 2 0 0 1 1 1.73V20a2 2 0 0 0 2 2h.44a2 2 0 0 0 2-2v-.18a2 2 0 0 1 1-1.73l.43-.25a2 2 0 0 1 2 0l.15.08a2 2 0 0 0 2.73-.73l.22-.39a2 2 0 0 0-.73-2.73l-.15-.08a2 2 0 0 1-1-1.74v-.5a2 2 0 0 1 1-1.74l.15-.09a2 2 0 0 0 .73-2.73l-.22-.38a2 2 0 0 0-2.73-.73l-.15.08a2 2 0 0 1-2 0l-.43-.25a2 2 0 0 1-1-1.73V4a2 2 0 0 0-2-2z"
            )
          ),
        Svg.circle
          .fill(Svg.Paint.None)
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .cx(12)
          .cy(12)
          .r(3)
      )

  extension (item: NavItem)
    private def navRow: UI =
      val base = div.cssClass("nav-item")
      val elem = if item.active then base.cssClass("active") else base
      elem(span("").cssClass("nav-dot"), item.label)
