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
