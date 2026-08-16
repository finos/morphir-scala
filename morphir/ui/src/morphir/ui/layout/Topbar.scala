package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.icons.Icons

/**
 * The full-width titlebar: a brand zone over the sidebar column (traffic-light inset, left toggle, brand), the
 * breadcrumb over the content, and — always at the far right of the window — the version chip and the bottom/right
 * region toggles. When the sidebar is collapsed the brand zone disappears and the left toggle joins the breadcrumb.
 */
object Topbar:

  object Css:
    val root        = "titlebar"
    val brandZone   = "brand-zone"
    val rest        = "titlebar-rest"
    val left        = "titlebar-left"
    val right       = "titlebar-right"
    val title       = "topbar-title"
    val crumb       = "crumb"
    val chip        = "chip"
    val lightsInset = "lights-inset"

  def view(
      sectionTitle: String,
      version: String,
      state: ShellState,
      customChrome: Boolean,
      leftVisibility: RegionVisibility
  ): UI =
    val crumbTitle =
      div.cssClass(Css.title)(span("morphir / ").cssClass(Css.crumb), sectionTitle)
    val rightCluster =
      div.cssClass(Css.right)(
        span(s"v$version").cssClass(Css.chip).id("app-version"),
        div.cssClass(Shell.Css.iconBtn).id("bottom-toggle").onClick(state.bottom.getAndUpdate(_.toggled))(
          Icons.panelBottom
        ),
        div.cssClass(Shell.Css.iconBtn).id("right-toggle").onClick(state.right.getAndUpdate(_.toggled))(
          Icons.panelRight
        )
      )
    if leftVisibility.isCollapsed then
      div.cssClass(Css.root).id("titlebar")(
        (if customChrome then div.cssClass(Css.left).cssClass(Css.lightsInset) else div.cssClass(Css.left)) (
          div.cssClass(Shell.Css.iconBtn).id("sidebar-toggle").onClick(state.left.set(RegionVisibility.Expanded))(
            Icons.sidebar
          ),
          crumbTitle
        ),
        rightCluster
      )
    else
      div.cssClass(Css.root).id("titlebar")(
        (if customChrome then div.cssClass(Css.brandZone).cssClass(Css.lightsInset) else div.cssClass(Css.brandZone)) (
          div.cssClass(Shell.Css.iconBtn).id("sidebar-toggle").onClick(state.left.set(RegionVisibility.Collapsed))(
            Icons.sidebar
          ),
          div.cssClass("brand")(span("morphir").cssClass("brand-mark"), span("DESKTOP").cssClass("brand-sub"))
        ),
        div.cssClass(Css.rest)(crumbTitle, rightCluster)
      )

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Css.root,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .height(52.px)
          .flexShrink(0)
          .borderBottom(1.px, Tokens.hex("#241f30"))
          .bg(Tokens.cssVar("surface"))
      )
      .rule(
        Css.brandZone,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .gap(8.px)
          .width(224.px)
          .height(100.pct)
          .flexShrink(0)
          .padding(0.px, 12.px)
          .bg(Tokens.hex("#121017"))
          .borderRight(1.px, Tokens.hex("#241f30"))
      )
      .rule(Selector.cls(s"${Css.brandZone}.${Css.lightsInset}"), Style.padding(0.px, 12.px, 0.px, 64.px))
      .rule(
        Selector.cls(s"${Css.brandZone}.${Css.lightsInset}").descendant(Selector.cls("brand-sub")),
        Style.displayNone
      )
      .rule(
        Css.rest,
        Style.display(_.flex).row.align(_.center).justify(_.spaceBetween).flexGrow(1).padding(0.px, 22.px)
      )
      .rule(
        Css.left,
        Style.display(_.flex).row.align(_.center).gap(12.px).flexGrow(1).padding(0.px, 0.px, 0.px, 22.px)
      )
      .rule(Selector.cls(s"${Css.left}.${Css.lightsInset}"), Style.padding(0.px, 0.px, 0.px, 78.px))
      .rule(
        Css.right,
        Style.display(_.flex).row.align(_.center).gap(8.px).padding(0.px, 22.px, 0.px, 0.px)
      )
      .rule(
        Css.title,
        Style.display(_.flex).row.align(_.baseline).gap(4.px).fontWeight(_.w600).fontSize(14.px)
      )
      .rule(
        Selector.cls(Css.title).descendant(Selector.cls(Css.crumb)),
        Style.color(Tokens.cssVar("muted2")).fontWeight(_.w400)
      )
      .rule(
        Css.chip,
        Style
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
          .fontSize(11.px)
          .fontWeight(_.w600)
          .padding(3.px, 10.px)
          .rounded(999.px)
          .color(Tokens.hex("#f2b7dd"))
          .bg(Style.Color.rgba(214, 64, 159, 0.14))
          .border(1.px, Style.Color.rgba(214, 64, 159, 0.35))
      )
