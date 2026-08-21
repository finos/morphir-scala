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

    "conceptView compiles the Markdown body instead of showing its source" in {
      val ui = KnowledgeBrowserView.conceptView(
        ConceptDetail(
          ConceptRef("intent", "0033-markdown-compilation.md"),
          "Intent",
          "Markdown compilation",
          "## Problem\n\nCompile the AST.\n\n- one\n- two\n"
        )
      )
      renderOnce(ui).map { html =>
        assert(html.contains("<h2"), "the heading should be a heading element")
        assert(html.contains("<ul"), "the bullet list should be a list element")
        assert(html.contains("<li"), "the list should hold item elements")
        assert(!html.contains("## Problem"), "the raw Markdown source should not survive to the page")
        assert(!html.contains("- one"), "the raw bullet markers should not survive to the page")
      }
    }

    "conceptView shows a diagnostic rather than blanking when a body cannot be parsed" in {
      val ui = KnowledgeBrowserView.conceptView(
        ConceptDetail(ConceptRef("intent", "x.md"), "Intent", "Broken", "\u0000")
      )
      renderOnce(ui).map(html => assert(html.contains("kb-concept")))
    }
  }
end KnowledgeBrowserViewTests
