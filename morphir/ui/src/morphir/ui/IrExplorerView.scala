package morphir.ui

import kyo.*
import kyo.UI.*
import morphir.ui.services.*

object IrExplorerView:

  def packageList(packages: Chunk[PackageInfo]): UI =
    ul(packages.map(p => li(s"${p.name} (${p.moduleCount})"))*).id("ir-packages")

  def moduleList(modules: Chunk[ModuleInfo]): UI =
    ul(modules.map(m => li(s"${m.name} — ${m.typeCount} types, ${m.valueCount} values"))*).id("ir-modules")

  def definitionCard(detail: DefinitionDetail): UI =
    div(
      h2(s"${detail.ref.moduleName}.${detail.ref.localName}"),
      p(detail.kind.toString),
      pre(code(detail.summary))
    ).id("ir-definition")

  object Css:
    val packages   = "ir-packages"
    val modules    = "ir-modules"
    val definition = "ir-definition"

  import kyo.Style
  import morphir.ui.theme.Tokens

  def sheet: Stylesheet =
    val mono   = Style.FontFamily.Custom(Tokens.monoFont)
    val rowsOf = (id: String) =>
      Stylesheet.empty
        .rule(Selector.id(id), Style.listStyle(_.none))
        .rule(
          Selector.id(id).child(Selector.tag("li")),
          Style
            .padding(9.px, 4.px)
            .borderBottom(1.px, Tokens.cssVar("row-edge"))
            .fontFamily(mono)
            .fontSize(13.px)
            .hover(_.bg(Tokens.cssVar("hover")))
        )
    rowsOf(Css.packages) ++ rowsOf(Css.modules) ++
      Stylesheet.empty.rule(
        Selector.id(Css.definition).descendant(Selector.tag("pre")),
        Style
          .bg(Tokens.cssVar("code-bg"))
          .border(1.px, Tokens.cssVar("edge"))
          .rounded(8.px)
          .padding(12.px, 14.px)
          .fontFamily(mono)
          .fontSize(12.px)
          .overflowX(_.auto)
      )
