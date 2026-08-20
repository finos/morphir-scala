package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.langkit.markdown.Parser
import morphir.langkit.markdown.kyoui.KyoUiCompiler
import morphir.ui.services.*

object KnowledgeBrowserView:

  def bundleList(bundles: Chunk[BundleInfo]): UI =
    ul(bundles.map(b => li(s"${b.title} (${b.conceptCount})"))*).id("kb-bundles")

  def conceptView(concept: ConceptDetail): UI =
    div(
      h2(concept.title),
      p(concept.conceptType),
      conceptBody(concept.body)
    ).id("kb-concept")

  /**
   * Concept bodies are Markdown, so they are parsed and compiled rather than shown as source.
   *
   * A body that will not parse renders as a diagnostic instead of raising: one malformed concept should not blank the
   * browser, and the reader is better served seeing that something is wrong than seeing nothing.
   */
  private def conceptBody(body: String): UI =
    Parser.parse(body) match
      case Result.Success(document) => KyoUiCompiler.compile(document)
      case failure                  => p(s"This concept body could not be parsed: $failure")

  def intentTable(intents: Chunk[IntentSummary]): UI =
    val header = tr(th("No"), th("Title"), th("State"), th("Kind"))
    val rows   = intents.map(i => tr(td(i.number), td(i.title), td(i.state), td(i.kind)))
    table((header +: rows).toSeq.map(r => r: Ast.HtmlChildVal)*).id("kb-intents")

  object Css:
    val bundles = "kb-bundles"
    val concept = "kb-concept"
    val intents = "kb-intents"

  import kyo.Style
  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    val mono  = Style.FontFamily.Custom(Tokens.monoFont)
    val table = Selector.id(Css.intents)
    Stylesheet.empty
      .rule(Selector.id(Css.bundles), Style.listStyle(_.none))
      .rule(table, Style.width(100.pct).borderCollapse(_.collapse).fontSize(13.px))
      .rule(
        table.descendant(Selector.tag("th")),
        Style
          .textAlign(_.left)
          .fontFamily(mono)
          .fontSize(10.px)
          .fontWeight(_.w600)
          .letterSpacing(0.14.em)
          .textTransform(_.uppercase)
          .color(Tokens.cssVar("muted2"))
          .padding(4.px, 10.px, 10.px, 4.px)
          .borderBottom(1.px, Tokens.cssVar("panel-edge"))
      )
      .rule(
        table.descendant(Selector.tag("td")),
        Style.padding(9.px, 10.px, 9.px, 4.px).borderBottom(1.px, Tokens.cssVar("row-edge"))
      )
      .rule(
        table.descendant(Selector.tag("td").pseudo("first-child")),
        Style.fontFamily(mono).fontSize(12.px).fontWeight(_.w600).color(Tokens.cssVar("accent2"))
      )
      .rule(
        table.descendant(Selector.tag("td").pseudo("nth-child(3)")),
        Style.fontFamily(mono).fontSize(11.px).fontWeight(_.w600).color(Tokens.cssVar("accent-text"))
      )
      .rule(
        table.descendant(Selector.tag("td").pseudo("nth-child(4)")),
        Style.fontFamily(mono).fontSize(12.px).color(Tokens.cssVar("muted2"))
      )
