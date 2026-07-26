package morphir.langkit.elm.compiler

import kyo.test.*

class SourceSnippetBuilderSpec extends Test[Any]:

  "SourceSnippetBuilder" - {
    "includes two lines before and one line after when available" in {
      val source  = "module Demo exposing (..)\n\nmain =\n"
      val snippet = SourceSnippetBuilder.build(
        source = source,
        errorLine = 3,
        column = 7,
        errorWidth = 0
      )
      assert(snippet.contextLines.map(_.line) == List(1, 2, 3, 4))
      assert(snippet.contextLines.count(_.isErrorLine) == 1)
      assert(snippet.rendered.contains("1| module Demo exposing (..)"))
      assert(snippet.rendered.contains("3| main ="))
      assert(snippet.rendered.contains("4|"))
      assert(snippet.rendered.linesIterator.toList.last == "         ^")
    }
    "clamps context at the start of the file" in {
      val snippet = SourceSnippetBuilder.build(
        source = "module M",
        errorLine = 1,
        column = 7,
        errorWidth = 0
      )
      assert(snippet.contextLines.map(_.line) == List(1))
      assert(snippet.rendered.startsWith("1| module M"))
    }
    "preserves a trailing empty line after a final newline" in {
      val snippet = SourceSnippetBuilder.build(
        source = "x =\n",
        errorLine = 2,
        column = 1,
        errorWidth = 0
      )
      assert(snippet.contextLines.exists(line => line.line == 2 && line.text.isEmpty))
      assert(snippet.rendered.contains("2| "))
    }
  }
