package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.icons.Icons

/**
 * The topbar slot: breadcrumb (plus the sidebar toggle when the sidebar is collapsed) on the left, version chip and the
 * right/bottom region toggles on the right.
 */
object Topbar:

  object Css:
    val root        = "topbar"
    val left        = "topbar-left"
    val right       = "topbar-right"
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
    val titleGroup =
      div.cssClass(Css.title)(span("morphir / ").cssClass(Css.crumb), sectionTitle)
    val leftGroup =
      if leftVisibility.isCollapsed then
        div.cssClass(Css.left)(
          div.cssClass(
            Shell.Css.iconBtn
          ).id("sidebar-toggle").onClick(state.left.set(RegionVisibility.Expanded))(Icons.sidebar),
          titleGroup
        )
      else titleGroup
    val bar =
      if leftVisibility.isCollapsed && customChrome then div.cssClass(Css.root).cssClass(Css.lightsInset)
      else div.cssClass(Css.root)
    bar(
      leftGroup,
      div.cssClass(Css.right)(
        span(s"v$version").cssClass(Css.chip).id("app-version"),
        div.cssClass(Shell.Css.iconBtn).id("bottom-toggle").onClick(state.bottom.getAndUpdate(_.toggled))(
          Icons.panelBottom
        ),
        div.cssClass(Shell.Css.iconBtn).id("right-toggle").onClick(state.right.getAndUpdate(_.toggled))(
          Icons.panelRight
        )
      )
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
          .justify(_.spaceBetween)
          .height(52.px)
          .flexShrink(0)
          .padding(0.px, 22.px)
          .borderBottom(1.px, Tokens.hex("#241f30"))
          .bg(Tokens.cssVar("surface"))
      )
      .rule(Selector.cls(s"${Css.root}.${Css.lightsInset}"), Style.padding(0.px, 22.px, 0.px, 78.px))
      .rule(Css.left, Style.display(_.flex).row.align(_.center).gap(12.px))
      .rule(Css.right, Style.display(_.flex).row.align(_.center).gap(8.px))
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
