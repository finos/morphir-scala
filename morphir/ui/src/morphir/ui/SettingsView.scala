package morphir.ui

import kyo.*
import kyo.UI.*

/**
 * Content views for the settings surface: titled groups of labelled rows, each row carrying an optional description and
 * a right-hand value. Pure views — the host supplies the values.
 */
object SettingsView:

  object Css:
    val group       = "settings-group"
    val groupTitle  = "settings-group-title"
    val row         = "settings-row"
    val rowText     = "settings-row-text"
    val label       = "settings-label"
    val description = "settings-description"
    val value       = "settings-value"

  final case class Row(label: String, description: String, value: String)

  def group(title: String, rows: Chunk[Row]): UI =
    section.cssClass(Css.group)(
      h2(title).cssClass(Css.groupTitle),
      fragment(rows.toSeq.map(rowView)*)
    )

  private def rowView(entry: Row): UI =
    div.cssClass(Css.row)(
      div.cssClass(Css.rowText)(
        div.cssClass(Css.label)(entry.label),
        div.cssClass(Css.description)(entry.description)
      ),
      span(entry.value).cssClass(Css.value)
    )

  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    Stylesheet.empty
      .rule(Css.group, Style.display(_.flex).column.padding(0.px, 0.px, 28.px, 0.px))
      .rule(
        Css.groupTitle,
        Style.fontSize(18.px).fontWeight(_.w600).padding(0.px, 0.px, 14.px, 0.px)
      )
      .rule(
        Css.row,
        Style
          .display(_.flex)
          .row
          .align(_.center)
          .justify(_.spaceBetween)
          .gap(24.px)
          .padding(12.px, 0.px)
          .borderBottom(1.px, Tokens.hex("#221d2e"))
      )
      .rule(Css.rowText, Style.display(_.flex).column.gap(3.px).minWidth(0.px))
      .rule(Css.label, Style.fontWeight(_.w500))
      .rule(Css.description, Style.fontSize(12.5.px).color(Tokens.cssVar("muted2")))
      .rule(
        Css.value,
        Style
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
          .fontSize(12.px)
          .color(Tokens.hex("#f2b7dd"))
          .flexShrink(0)
      )
