package morphir.ui

import kyo.*
import kyo.test.*
import morphir.ui.services.*

class KnowledgeBrowserViewTests extends Test[Any]:

  def renderOnce(ui: UI): String < Async =
    UI.runRender(ui).take(1).run.map(_.mkString)

  "KnowledgeBrowserView" - {

    "intentTable renders number, title and state per row" in {
      val ui = KnowledgeBrowserView.intentTable(
        Chunk(
          IntentSummary("0025", "Electron appkit", "Refinement", "feature"),
          IntentSummary("0029", "morphir-ui kyo-ui client library", "Backlog", "feature")
        )
      )
      renderOnce(ui).map { html =>
        assert(html.contains("0025") && html.contains("Refinement") && html.contains("kb-intents"))
      }
    }

    "conceptView renders title and body" in {
      val ui = KnowledgeBrowserView.conceptView(
        ConceptDetail(
          ConceptRef("intent", "0025-electron-appkit.md"),
          "Intent",
          "Electron appkit",
          "Publish morphir-appkit-electron."
        )
      )
      renderOnce(ui).map { html =>
        assert(html.contains("Electron appkit") && html.contains("kb-concept"))
      }
    }
  }
end KnowledgeBrowserViewTests
