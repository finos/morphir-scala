package morphir.ui

import kyo.*
import kyo.UI.*

/**
 * Application chrome shared by every morphir client: collapsible left sidebar (brand + navigation + settings footer),
 * topbar (context + version chip), and a panel grid for content. Styled by [[Theme.css]].
 *
 * The sidebar's collapsed state is a [[kyo.SignalRef]] owned by the host, so the shell stays a pure value and the host
 * decides whether the state is ephemeral or persisted.
 */
object AppShell:

  final case class NavItem(label: String, active: Boolean = false)

  def panel(title: String, body: UI): UI =
    section.cssClass("panel")(h2(title), body)

  def shell(
      sectionTitle: String,
      version: String,
      nav: Chunk[NavItem],
      panels: Chunk[UI],
      collapsed: SignalRef[Boolean],
      onSettings: => Any < Async = (),
      customChrome: Boolean = false
  ): UI =
    div.cssClass("app").id("app-root")(
      collapsed.render { isCollapsed =>
        if isCollapsed then railSidebar(nav, collapsed, onSettings, customChrome)
        else fullSidebar(nav, collapsed, onSettings, customChrome)
      },
      div.cssClass("main")(
        div.cssClass("topbar")(
          div.cssClass("topbar-title")(span("morphir / ").cssClass("crumb"), sectionTitle),
          span(s"v$version").cssClass("chip").id("app-version")
        ),
        div.cssClass("content")(fragment(panels.toSeq*))
      )
    )

  private def fullSidebar(
      nav: Chunk[NavItem],
      collapsed: SignalRef[Boolean],
      onSettings: => Any < Async,
      customChrome: Boolean
  ): UI =
    div.cssClass("sidebar")(
      (if customChrome then div.cssClass("sidebar-head").cssClass("lights-inset").id("titlebar-drag")
       else div.cssClass("sidebar-head")) (
        div.cssClass("brand")(span("morphir").cssClass("brand-mark"), span("DESKTOP").cssClass("brand-sub")),
        div.cssClass("icon-btn").id("sidebar-toggle").onClick(collapsed.set(true))(panelIcon)
      ),
      div.cssClass("nav-section")("Workspace"),
      fragment(nav.toSeq.map(_.navRow)*),
      div.cssClass("sidebar-foot")(
        div.cssClass("icon-btn").id("settings-button").onClick(onSettings)(gearIcon),
        span("morphir-scala · kyo-ui").cssClass("foot-meta")
      )
    )

  private def railSidebar(
      nav: Chunk[NavItem],
      collapsed: SignalRef[Boolean],
      onSettings: => Any < Async,
      customChrome: Boolean
  ): UI =
    div.cssClass("sidebar").cssClass("rail")(
      if customChrome then div.cssClass("titlebar-drag").id("titlebar-drag") else UI.empty,
      div.cssClass("icon-btn").id("sidebar-toggle").onClick(collapsed.set(false))(panelIcon),
      div.cssClass("rail-divider"),
      fragment(nav.toSeq.map(_.railRow)*),
      div.cssClass("rail-spacer"),
      div.cssClass("icon-btn").id("settings-button").onClick(onSettings)(gearIcon)
    )

  /** Lucide `panel-left` glyph: the sidebar collapse/expand toggle. */
  private def panelIcon: UI =
    Svg.svg
      .viewBox(Svg.ViewBox(0, 0, 24, 24))
      .width(16)
      .height(16)(
        Svg.rect
          .fill(Svg.Paint.None)
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .x(3)
          .y(3)
          .width(18)
          .height(18)
          .rx(2),
        Svg.line
          .stroke(Svg.Paint.CurrentColor)
          .strokeWidth(2.0)
          .x1(9)
          .y1(3)
          .x2(9)
          .y2(21)
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

    private def railRow: UI =
      val base = div.cssClass("rail-nav-item")
      val elem = if item.active then base.cssClass("active") else base
      elem(span("").cssClass("nav-dot"))
