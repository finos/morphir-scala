package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.layout.ColorScheme

/**
 * The colour-scheme picker: one card per scheme, each showing a miniature of the shell painted in that scheme, with the
 * active card outlined. Reads the scheme signal and runs a command on click.
 */
object SchemePicker:

  object Css:
    val row     = "scheme-row"
    val card    = "scheme-card"
    val active  = "active"
    val preview = "scheme-preview"
    val rail    = "scheme-rail"
    val body    = "scheme-body"
    val line    = "scheme-line"
    val wide    = "wide"
    val label   = "scheme-label"

  def view(current: Signal[ColorScheme], onSelect: ColorScheme => Any < Async): UI =
    current.render { selected =>
      div.cssClass(Css.row).id("scheme-picker")(
        card(ColorScheme.System, selected, onSelect),
        card(ColorScheme.Light, selected, onSelect),
        card(ColorScheme.Dark, selected, onSelect)
      )
    }

  private def card(scheme: ColorScheme, selected: ColorScheme, onSelect: ColorScheme => Any < Async): UI =
    val base = div.cssClass(Css.card)
    val elem = if scheme == selected then base.cssClass(Css.active) else base
    elem
      .id(s"scheme-${scheme.label.toLowerCase}")
      .onClick(onSelect(scheme))(
        // The miniature paints itself with the scheme it offers, so the card previews the choice.
        div.cssClass(Css.preview).cssClass(scheme.cssClass)(
          div.cssClass(Css.rail)(
            div.cssClass(Css.line),
            div.cssClass(Css.line),
            div.cssClass(Css.line)
          ),
          div.cssClass(Css.body)(
            div.cssClass(Css.line).cssClass(Css.wide),
            div.cssClass(Css.line).cssClass(Css.wide),
            div.cssClass(Css.line)
          )
        ),
        span(scheme.label).cssClass(Css.label)
      )

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(Css.row, Style.display(_.flex).row.gap(12.px).padding(4.px, 0.px, 8.px, 0.px))
      .rule(
        Css.card,
        Style
          .display(_.flex)
          .column
          .gap(8.px)
          .width(150.px)
          .padding(8.px)
          .rounded(10.px)
          .border(1.px, Tokens.cssVar("panel-edge"))
          .bg(Tokens.cssVar("panel"))
          .cursor(_.pointer)
          .transition(_.all, 140, _.easeInOut)
          .hover(_.border(1.px, Tokens.cssVar("accent2")))
      )
      .rule(
        Selector.cls(s"${Css.card}.${Css.active}"),
        Style.border(1.px, Tokens.cssVar("accent")).shadow(0.px, 0.px, 0.px, 1.px, Tokens.cssVar("accent"))
      )
      .rule(
        Css.preview,
        Style
          .display(_.flex)
          .row
          .gap(4.px)
          .height(72.px)
          .padding(6.px)
          .rounded(7.px)
          .bg(Tokens.cssVar("bg"))
          .border(1.px, Tokens.cssVar("edge"))
      )
      .rule(
        Css.rail,
        Style.display(_.flex).column.gap(5.px).width(30.px).flexShrink(0).padding(4.px).rounded(4.px)
          .bg(Tokens.cssVar("rail"))
      )
      .rule(Css.body, Style.display(_.flex).column.gap(5.px).flexGrow(1).padding(4.px))
      .rule(Css.line, Style.height(4.px).width(60.pct).rounded(999.px).bg(Tokens.cssVar("dot")))
      .rule(Selector.cls(s"${Css.line}.${Css.wide}"), Style.width(100.pct).bg(Tokens.cssVar("panel-edge")))
      .rule(
        Css.label,
        Style.fontSize(12.5.px).color(Tokens.cssVar("muted")).textAlign(_.center)
      )
