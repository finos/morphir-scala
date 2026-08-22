package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{Cst, CstParser, Lower, MdWriter, Parser}

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

    /**
     * Case folding a tag name is an ASCII operation, not a linguistic one. `"TITLE".toLowerCase` is `"tıtle"` under a
     * Turkish default locale, which would leave `<TITLE>` unfiltered on a machine configured that way; the fold that
     * replaced it reads the ASCII range and nothing else. `TagFilterLocaleTests` pins the locale itself, which only the
     * JVM can set.
     */
    "escapes an all-caps and a mixed-case tag name" in {
      given MdProfile = MdProfile.gfm
      // Led by `<strong>`, which opens no HTML block, so the line stays a paragraph and the tags are inline ones.
      val filtered = inlineHtmlOf("<strong> <TITLE> <Iframe> <EM>\n")
      assert(filtered.contains("&lt;TITLE>"), filtered)
      assert(filtered.contains("&lt;Iframe>"), filtered)
      assert(filtered.contains("<EM>"), filtered)
    }

    "leaves the concrete syntax tree verbatim" in {
      given MdProfile = MdProfile.gfm
      val source      = "<strong> <title>\n"
      assert(Cst.print(CstParser.parse(source)) == source)
    }
  }

  /**
   * The writer cannot tell a filtered `<title>` apart from an author who typed `&lt;title>` outright by looking at the
   * value alone — both are the identical string once filtering runs. [[Lower.rawHtmlMeta]] records the pre-filter
   * original under [[Lower.unfilteredHtml]] exactly when filtering changed the value, and [[MdWriter]] writes that
   * recorded original back rather than guessing one from the string's shape. This roster is what a string-shape
   * inversion could not pass: in particular "a node holding both forms" below, where the same value carries a filtered
   * tag and an author-written look-alike side by side.
   */
  "round-trips through the Markdown writer" - {

    "a filtered inline tag writes as its recorded original and reparses to the same tree" in {
      given MdProfile = MdProfile.gfm
      val original    = Parser.parse("<em><title>x</title></em>\n").getOrThrow
      val written     = MdWriter.write(original)
      assert(written.contains("<title>x</title>"), written)
      assert(!written.contains("&lt;"), written)
      assert(Parser.parse(written).getOrThrow.unpositioned == original.unpositioned, written)
    }

    "a filtered closing tag writes as its recorded original and reparses to the same tree" in {
      given MdProfile = MdProfile.gfm
      val original    = Parser.parse("<em></script></em>\n").getOrThrow
      val written     = MdWriter.write(original)
      assert(written.contains("</script>"), written)
      assert(!written.contains("&lt;"), written)
      assert(Parser.parse(written).getOrThrow.unpositioned == original.unpositioned, written)
    }

    "case-insensitive filtering round-trips through the writer" in {
      given MdProfile = MdProfile.gfm
      val original    = Parser.parse("<blockquote>\n  <XMP>x</XMP>\n</blockquote>\n").getOrThrow
      val written     = MdWriter.write(original)
      assert(written.contains("<XMP>x</XMP>"), written)
      assert(!written.contains("&lt;"), written)
      assert(Parser.parse(written).getOrThrow.unpositioned == original.unpositioned, written)
    }

    "an author-written &lt;script> under the CommonMark profile survives the writer verbatim" in {
      given MdProfile = MdProfile.commonmark
      val source      = "<div>Use &lt;script> here</div>\n"
      val original    = Parser.parse(source).getOrThrow
      val written     = MdWriter.write(original)
      assert(written.contains("&lt;script>"), written)
      assert(!written.contains("<script>"), written)
    }

    "a node holding both a filtered tag and an author-written look-alike writes each correctly" in {
      given MdProfile = MdProfile.gfm
      val source      = "<div>Use &lt;script> here, <title>x</title></div>\n"
      val original    = Parser.parse(source).getOrThrow
      val html        = original.children.collect { case h: MdNode.Html => h }.head
      // The author's text was never a real `<`, so filtering never touched it: it is the identical substring before
      // and after. The real `<title>` tag is what changed, which is what makes the recorded original non-trivial
      // here rather than merely equal to the filtered value.
      assert(html.value.contains("&lt;script>"), html.value)
      assert(html.value.contains("&lt;title>x&lt;/title>"), html.value)
      val written = MdWriter.write(original)
      assert(written.contains("&lt;script>"), written)
      assert(written.contains("<title>x</title>"), written)
      assert(Parser.parse(written).getOrThrow.unpositioned == original.unpositioned, written)
    }
  }
