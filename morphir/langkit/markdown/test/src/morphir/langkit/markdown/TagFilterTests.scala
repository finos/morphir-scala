package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{Cst, CstParser, Parser}

/**
 * GFM's disallowed-raw-HTML filter.
 *
 * Nine tag names are escaped rather than passed through, by replacing the leading `<` with `&lt;` and leaving the rest
 * — including the closing `>` — untouched. The specification chose these nine because each changes how the HTML around
 * it is interpreted.
 *
 * The filter is applied at lowering rather than in the writers, so the concrete syntax tree still holds the tag exactly
 * as written and printing it reproduces the source byte for byte.
 */
class TagFilterTests extends Test[Any]:

  private def htmlOf(source: String)(using MdProfile): String =
    Parser.parse(source).getOrThrow.children.collect {
      case MdNode.Html(value, _) => value
    }.mkString

  private def inlineHtmlOf(source: String)(using MdProfile): String =
    Parser.parse(source).getOrThrow.children.head match
      case MdNode.Paragraph(content, _) => content.collect { case MdNode.InlineHtml(value, _) => value }.mkString
      case other                        => throw new AssertionError(s"expected a paragraph, got $other")

  "the tag filter" - {

    "escapes a disallowed inline tag and leaves an allowed one alone (spec example 652)" in {
      given MdProfile = MdProfile.gfm
      val filtered    = inlineHtmlOf("<strong> <title> <style> <em>\n")
      assert(filtered.contains("<strong>"))
      assert(filtered.contains("&lt;title>"))
      assert(filtered.contains("&lt;style>"))
      assert(filtered.contains("<em>"))
    }

    "escapes inside an HTML block, case-insensitively (spec example 652)" in {
      given MdProfile = MdProfile.gfm
      val block       = htmlOf("<blockquote>\n  <xmp> is disallowed.  <XMP> is also disallowed.\n</blockquote>\n")
      assert(block.contains("&lt;xmp>"))
      assert(block.contains("&lt;XMP>"))
      assert(!block.contains("<xmp>"))
    }

    "escapes a closing tag too" in {
      given MdProfile = MdProfile.gfm
      assert(inlineHtmlOf("<em></script></em>\n").contains("&lt;/script>"))
    }

    "is off under the CommonMark profile" in {
      given MdProfile = MdProfile.commonmark
      assert(inlineHtmlOf("<strong> <title>\n").contains("<title>"))
    }

    "leaves the concrete syntax tree verbatim" in {
      given MdProfile = MdProfile.gfm
      val source      = "<strong> <title>\n"
      assert(Cst.print(CstParser.parse(source)) == source)
    }
  }
