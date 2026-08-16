package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.AppShell.NavItem
import morphir.ui.icons.Icons

/** The expanded left sidebar: header (toggle + brand), nav list, settings footer. */
object Sidebar:

  object Css:
    val root        = "sidebar"
    val head        = "sidebar-head"
    val headLeft    = "head-left"
    val lightsInset = "lights-inset"
    val brand       = "brand"
    val brandMark   = "brand-mark"
    val brandSub    = "brand-sub"
    val navSection  = "nav-section"
    val navItem     = "nav-item"
    val navActive   = "active"
    val navDot      = "nav-dot"
    val foot        = "sidebar-foot"
    val footMeta    = "foot-meta"

  def view(
      nav: Chunk[NavItem],
      left: SignalRef[Boolean],
      onSettings: => Any < Async,
      customChrome: Boolean
  ): UI =
    div.cssClass(Css.root)(
      (if customChrome then div.cssClass(Css.head).cssClass(Css.lightsInset).id("titlebar-drag")
       else div.cssClass(Css.head)) (
        div.cssClass(Css.headLeft)(
          div.cssClass(Shell.Css.iconBtn).id("sidebar-toggle").onClick(left.set(true))(Icons.sidebar),
          div.cssClass(Css.brand)(span("morphir").cssClass(Css.brandMark), span("DESKTOP").cssClass(Css.brandSub))
        )
      ),
      div.cssClass(Css.navSection)("Workspace"),
      fragment(nav.toSeq.map(navRow)*),
      div.cssClass(Css.foot)(
        div.cssClass(Shell.Css.iconBtn).id("settings-button").onClick(onSettings)(Icons.gear),
        span("morphir-scala · kyo-ui").cssClass(Css.footMeta)
      )
    )

  private def navRow(item: NavItem): UI =
    val base = div.cssClass(Css.navItem)
    val elem = if item.active then base.cssClass(Css.navActive) else base
    elem(span("").cssClass(Css.navDot), item.label)
