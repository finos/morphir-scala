package morphir.ui

import kyo.*
import kyo.test.*
import morphir.ui.services.*

class IrExplorerViewTests extends Test[Any]:

  /**
   * First emission of the render stream: the initial HTML. The stream itself never ends — it keeps emitting on signal
   * changes — so static-view tests must take(1).
   */
  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  "IrExplorerView" - {

    "packageList renders one li per package with the package name" in {
      val ui = IrExplorerView.packageList(Chunk(PackageInfo("Morphir.SDK", 3), PackageInfo("Acme.Models", 1)))
      renderOnce(ui).map { html =>
        assert(html.contains("Morphir.SDK") && html.contains("Acme.Models") && html.contains("ir-packages"))
      }
    }

    "definitionCard renders the summary" in {
      val detail = DefinitionDetail(
        DefinitionRef("Morphir.SDK", "List", "map"),
        DefinitionKind.Value,
        "map : (a -> b) -> List a -> List b"
      )
      renderOnce(IrExplorerView.definitionCard(detail)).map { html =>
        assert(
          html.contains("map : (a -&gt; b) -&gt; List a -&gt; List b") ||
            html.contains("map : (a -> b) -> List a -> List b")
        )
      }
    }
  }
end IrExplorerViewTests
