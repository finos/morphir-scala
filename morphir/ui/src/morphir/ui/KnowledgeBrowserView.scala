package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.services.*

object KnowledgeBrowserView:

  def bundleList(bundles: Chunk[BundleInfo]): UI =
    ul(bundles.map(b => li(s"${b.title} (${b.conceptCount})"))*).id("kb-bundles")

  def conceptView(concept: ConceptDetail): UI =
    div(
      h2(concept.title),
      p(concept.conceptType),
      p(concept.body)
    ).id("kb-concept")

  def intentTable(intents: Chunk[IntentSummary]): UI =
    val header = tr(th("No"), th("Title"), th("State"), th("Kind"))
    val rows   = intents.map(i => tr(td(i.number), td(i.title), td(i.state), td(i.kind)))
    table((header +: rows).toSeq.map(r => r: Ast.HtmlChildVal)*).id("kb-intents")
