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
    val control     = "settings-control"
    val intro       = "settings-intro"

  /** A settings row's trailing cell: a read-only value, or an interactive control such as a [[Toggle]]. */
  enum Trailing:
    case Value(text: String)
    case Control(ui: UI)

  final case class Row(label: String, description: String, trailing: Trailing)

  object Row:
    def value(label: String, description: String, text: String): Row =
      Row(label, description, Trailing.Value(text))
    def control(label: String, description: String, control: UI): Row =
      Row(label, description, Trailing.Control(control))

  /** A group whose body is arbitrary content — a picker, a preview — rather than labelled rows. */
  def contentGroup(title: String, description: String, body: UI): UI =
    section.cssClass(Css.group)(
      h2(title).cssClass(Css.groupTitle),
      div.cssClass(Css.intro)(description),
      body
    )

  /** The shared settings group for host-backed external connections. */
  def connections(body: UI): UI =
    contentGroup("Connections", "Connect Morphir to external services.", body)

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
      entry.trailing match
        case Trailing.Value(text) => span(text).cssClass(Css.value)
        case Trailing.Control(ui) => div.cssClass(Css.control)(ui)
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
          .borderBottom(1.px, Tokens.cssVar("row-edge"))
      )
      .rule(Css.rowText, Style.display(_.flex).column.gap(3.px).minWidth(0.px))
      .rule(Css.label, Style.fontWeight(_.w500))
      .rule(Css.description, Style.fontSize(12.5.px).color(Tokens.cssVar("muted2")))
      .rule(
        Css.intro,
        Style.fontSize(12.5.px).color(Tokens.cssVar("muted2")).padding(0.px, 0.px, 12.px, 0.px)
      )
      .rule(Css.control, Style.display(_.flex).row.align(_.center).flexShrink(0))
      .rule(
        Css.value,
        Style
          .fontFamily(Style.FontFamily.Custom(Tokens.monoFont))
          .fontSize(12.px)
          .color(Tokens.cssVar("accent-text"))
          .flexShrink(0)
      )
