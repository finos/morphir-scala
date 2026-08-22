package morphir.langkit.markdown

import java.util.Locale
import kyo.test.*
import morphir.langkit.markdown.internal.Parser

/**
 * Tag matching under a hostile default locale.
 *
 * `String.toLowerCase` with no locale argument folds against the JVM's default one, and Turkish maps `I` to the dotless
 * `ı` rather than to `i`: `"TITLE".toLowerCase` is `"tıtle"` there, which matches no name in the disallowed roster and
 * lets `<TITLE>` through unescaped. The same trap sits under the HTML block start conditions, where `<SCRIPT>` would
 * stop opening a script-like block.
 *
 * The lowercasing this defends is an ASCII fold that reads no locale at all, and setting the default is the only way to
 * observe that it is one — which is why this suite is JVM-only rather than sitting beside the rest of the tag filter's
 * tests. Scala.js and Scala Native have no default locale to set, and the fold is the same code on all three.
 */
class TagFilterLocaleTests extends Test[Any]:

  /** Runs `body` with the default locale set to `locale`, restoring whatever was there before. */
  private def underLocale[A](locale: Locale)(body: => A): A =
    val previous = Locale.getDefault
    Locale.setDefault(locale)
    try body
    finally Locale.setDefault(previous)

  private def inlineHtmlOf(source: String)(using MdProfile): String =
    Parser.parse(source).getOrThrow.children.head match
      case MdNode.Paragraph(content, _) => content.collect { case MdNode.InlineHtml(value, _) => value }.mkString
      case other                        => throw new AssertionError(s"expected a paragraph, got $other")

  "under a Turkish default locale" - {

    "the tag filter still escapes an upper-case disallowed tag" in {
      given MdProfile = MdProfile.gfm
      // Guards the premise rather than assuming it: if this fold ever stopped being locale-sensitive the test below
      // would pass for the wrong reason, and say nothing about the code it defends.
      assert(underLocale(Locale.forLanguageTag("tr"))("TITLE".toLowerCase) == "tıtle")

      // Led by `<strong>`, which opens no HTML block, so the line stays a paragraph and the tags are inline ones.
      val filtered = underLocale(Locale.forLanguageTag("tr"))(inlineHtmlOf("<strong> <TITLE> <Iframe>\n"))
      assert(filtered.contains("&lt;TITLE>"), filtered)
      assert(filtered.contains("&lt;Iframe>"), filtered)
    }

    "an upper-case script tag still opens an HTML block" in {
      given MdProfile = MdProfile.commonmark
      val root = underLocale(Locale.forLanguageTag("tr"))(Parser.parse("<SCRIPT>\nx < y\n</SCRIPT>\n").getOrThrow)
      // Condition one runs to its closing tag rather than to a blank line, so the whole run is one raw HTML block and
      // the `<` inside it is never read as an inline construct.
      val html = root.children.collect { case MdNode.Html(value, _) => value }.mkString
      assert(html.contains("x < y"), html)
      assert(html.contains("</SCRIPT>"), html)
    }
  }
