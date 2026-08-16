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

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    val mono = Style.FontFamily.Custom(Tokens.monoFont)
    Stylesheet.empty
      .rule(
        Css.root,
        Style
          .display(_.flex)
          .column
          .width(224.px)
          .flexGrow(0)
          .flexShrink(0)
          .bg(Tokens.hex("#121017"))
          .borderRight(1.px, Tokens.hex("#241f30"))
          .padding(0.px, 12.px, 18.px, 12.px)
      )
      .rule(
        Css.head,
        Style.display(_.flex).row.align(_.center).height(52.px).flexShrink(0).padding(0.px, 2.px, 0.px, 0.px)
      )
      .rule(Selector.cls(s"${Css.head}.${Css.lightsInset}"), Style.padding(0.px, 0.px, 0.px, 64.px))
      .rule(Selector.cls(s"${Css.head}.${Css.lightsInset}").descendant(Selector.cls(Css.brandSub)), Style.displayNone)
      .rule(Css.headLeft, Style.display(_.flex).row.align(_.center).gap(8.px))
      .rule(
        Css.brand,
        Style
          .display(_.flex)
          .row
          .align(_.baseline)
          .gap(8.px)
          .padding(0.px, 10.px)
          .fontWeight(_.w700)
          .fontSize(17.px)
          .letterSpacing(-0.01.em)
      )
      .rule(
        Css.brandSub,
        Style.fontFamily(mono).fontSize(9.px).fontWeight(_.w600).letterSpacing(0.22.em).color(Tokens.cssVar("muted2"))
      )
      .rule(
        Css.navSection,
        Style
          .fontFamily(mono)
          .fontSize(10.px)
          .fontWeight(_.w600)
          .letterSpacing(0.16.em)
          .textTransform(_.uppercase)
          .color(Tokens.cssVar("muted2"))
          .padding(16.px, 10.px, 6.px, 10.px)
      )
      .rule(
        Css.navItem,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .gap(10.px)
          .padding(8.px, 10.px)
          .margin(1.px, 0.px)
          .rounded(8.px)
          .color(Tokens.hex("#a89fbe"))
          .fontWeight(_.w500)
          .cursor(_.defaultCursor)
          .hover(_.bg(Tokens.hex("#1a1622")).color(Tokens.cssVar("text")))
      )
      .rule(
        Selector.cls(s"${Css.navItem}.${Css.navActive}"),
        Style
          .bgGradient(
            _.toRight,
            (Style.Color.rgba(214, 64, 159, 0.16), 0.pct),
            (Style.Color.rgba(139, 92, 246, 0.10), 100.pct)
          )
          .color(Tokens.hex("#ffffff"))
      )
      .rule(Css.navDot, Style.width(6.px).height(6.px).rounded(50.pct).bg(Tokens.hex("#3d3550")).flexShrink(0))
      .rule(
        Selector.cls(s"${Css.navItem}.${Css.navActive}").descendant(Selector.cls(Css.navDot)),
        Style.bg(Tokens.cssVar("accent"))
      )
      .rule(
        Css.foot,
        Style.display(_.flex).row.align(_.center).justify(_.spaceBetween).padding(6.px, 4.px, 0.px, 4.px)
      )
      .rule(Css.footMeta, Style.fontFamily(mono).fontSize(11.px).color(Tokens.cssVar("muted2")))
