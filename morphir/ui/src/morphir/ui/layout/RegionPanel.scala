package morphir.ui.layout

import kyo.*
import kyo.UI.*
import morphir.ui.AppShell.Region

/** The right and bottom shell regions: a titled header strip over a scrolling body. */
object RegionPanel:

  object Css:
    val rightbar         = "rightbar"
    val bottombar        = "bottombar"
    val head             = "region-head"
    val slideRightEnter  = "slide-right-enter"
    val slideRightLeave  = "slide-right-leave"
    val slideBottomEnter = "slide-bottom-enter"
    val slideBottomLeave = "slide-bottom-leave"
    val body             = "region-body"

  def right(region: Region, width: Signal[PanelSize]): UI =
    div.cssClass(Css.rightbar)(
      div.cssClass(Css.head)(region.title),
      div.cssClass(Css.body)(region.body)
    ).enterTransition(Css.slideRightEnter).leaveTransition(Css.slideRightLeave)
      .style(width.map(size => Style.width(size.px.px)))

  def bottom(region: Region, height: Signal[PanelSize]): UI =
    div.cssClass(Css.bottombar)(
      div.cssClass(Css.head)(region.title),
      div.cssClass(Css.body)(region.body)
    ).enterTransition(Css.slideBottomEnter).leaveTransition(Css.slideBottomLeave)
      .style(height.map(size => Style.height(size.px.px)))

  import kyo.*
  import kyo.Style
  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(
        Css.rightbar,
        Style
          .display(_.flex)
          .column
          .width(300.px)
          .flexGrow(0)
          .flexShrink(0)
          .bg(Tokens.hex("#121017"))
          .borderLeft(1.px, Tokens.hex("#241f30"))
          .transition(_.all, 180, _.easeInOut)
      )
      .rule(Css.slideRightEnter, Style.translate(24.px, 0.px).opacity(0))
      .rule(Css.slideRightLeave, Style.translate(24.px, 0.px).opacity(0))
      .rule(
        Css.bottombar,
        Style
          .display(_.flex)
          .column
          .height(180.px)
          .flexShrink(0)
          .bg(Tokens.hex("#121017"))
          .borderTop(1.px, Tokens.hex("#241f30"))
          .transition(_.all, 180, _.easeInOut)
      )
      .rule(Css.slideBottomEnter, Style.translate(0.px, 24.px).opacity(0))
      .rule(Css.slideBottomLeave, Style.translate(0.px, 24.px).opacity(0))
      .rule(
        Css.head,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .height(36.px)
          .flexShrink(0)
          .padding(0.px, 14.px)
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
          .fontSize(10.px)
          .fontWeight(_.w600)
          .letterSpacing(0.16.em)
          .textTransform(_.uppercase)
          .color(Tokens.cssVar("muted2"))
          .borderBottom(1.px, Tokens.hex("#1d1828"))
      )
      .rule(Css.body, Style.flexGrow(1).overflow(_.auto).padding(12.px, 14.px).fontSize(13.px))
