package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{Cst, CstParser, Lower, MdWriter, Parser}

/**
 * GFM's extended autolinks: URLs, `www.` hostnames and email addresses linked without angle brackets.
 *
 * Almost all of the difficulty is in where the link *ends*. Trailing punctuation is trimmed back off, closing
 * parentheses are balanced against the ones inside the destination, and a trailing entity reference is excluded — so a
 * link at the end of a sentence does not swallow the full stop.
 *
 * Recognition runs over the phrasing content the inline parser produced, so it never reaches inside a code span, an
 * existing link or raw HTML.
 *
 * The eleven examples the 0.29-gfm specification gives for the extension are cited by number below. Three rules have no
 * example of their own, because the published corpus scores every untagged example with the extension *off* and so
 * cannot pin a negative: that a `www.` host needs a period in it, that a match needs a word boundary before it, and
 * that a bare URL inside a link's own label stays text. Those are written out by hand, under [[MdProfile.gfm]].
 */
class ExtendedAutolinkTests extends Test[Any]:

  private def paragraphAt(source: String, index: Int)(using MdProfile): Chunk[MdNode.PhrasingContent] =
    Parser.parse(source).getOrThrow.children(index) match
      case MdNode.Paragraph(content, _) => content
      case other                        => throw new AssertionError(s"expected a paragraph, got $other")

  /** Every link the phrasing content holds, at any depth, as `(destination, text)`. */
  private def walk(nodes: Chunk[MdNode.PhrasingContent]): Chunk[(String, String)] =
    nodes.flatMap {
      case MdNode.Link(url, _, children, _) => Chunk((url, textOf(children))) ++ walk(children)
      case MdNode.Emphasis(inner, _)        => walk(inner)
      case MdNode.Strong(inner, _)          => walk(inner)
      case MdNode.Delete(inner, _)          => walk(inner)
      case _                                => Chunk.empty
    }

  private def links(source: String)(using MdProfile): Chunk[(String, String)] = walk(paragraphAt(source, 0))

  private def linksIn(source: String, paragraph: Int)(using MdProfile): Chunk[(String, String)] =
    walk(paragraphAt(source, paragraph))

  /** What the phrasing content says with its markup taken off, so a test can pin what stayed outside a link. */
  private def textOf(nodes: Chunk[MdNode.PhrasingContent]): String =
    nodes.map {
      case MdNode.Text(value, _)       => value
      case MdNode.Link(_, _, inner, _) => textOf(inner)
      case MdNode.Emphasis(inner, _)   => textOf(inner)
      case MdNode.Strong(inner, _)     => textOf(inner)
      case MdNode.Delete(inner, _)     => textOf(inner)
      case other                       => other.literal.getOrElse("")
    }.mkString

  private def plainAt(source: String, paragraph: Int)(using MdProfile): String = textOf(paragraphAt(source, paragraph))

  /**
   * Adjacent `Text` nodes merged back into one, the same normalization [[MdWriterTests]] applies before a structural
   * comparison: a parse splits prose at every escape and every entity, so the writer's `&#46;` spelling of a bare
   * hostname hands back three `Text` nodes where the direct parse produced one, without the meaning having changed.
   */
  private def mergedTexts(nodes: Chunk[MdNode.PhrasingContent]): Chunk[MdNode.PhrasingContent] =
    val out = scala.collection.mutable.ListBuffer.empty[MdNode.PhrasingContent]
    nodes.foreach { node =>
      (out.lastOption, normalizePhrasing(node)) match
        case (Some(MdNode.Text(before, meta)), MdNode.Text(after, _)) =>
          out.remove(out.size - 1)
          out += MdNode.Text(before + after, meta)
        case (_, normalized) => out += normalized
    }
    Chunk.from(out.toList)

  private def normalizePhrasing(node: MdNode.PhrasingContent): MdNode.PhrasingContent = node match
    case MdNode.Link(url, title, children, meta) => MdNode.Link(url, title, mergedTexts(children), meta)
    case leaf                                    => leaf

  private def normalize(node: MdNode): MdNode = node match
    case MdNode.Root(children, frontmatter, meta) =>
      MdNode.Root(children.map(child => normalize(child).asInstanceOf[MdNode.FlowContent]), frontmatter, meta)
    case MdNode.Paragraph(children, meta) => MdNode.Paragraph(mergedTexts(children), meta)
    case leaf                             => leaf

  "extended autolinks" - {

    "link a bare http URL" in {
      given MdProfile = MdProfile.gfm
      assert(links("visit www.commonmark.org\n") == Chunk(("http://www.commonmark.org", "www.commonmark.org")))
    }

    "link a `www.` hostname on its own (spec example 621)" in {
      given MdProfile = MdProfile.gfm
      assert(links("www.commonmark.org\n") == Chunk(("http://www.commonmark.org", "www.commonmark.org")))
    }

    "keep the path and leave the sentence around it (spec example 622)" in {
      given MdProfile = MdProfile.gfm
      val source      = "Visit www.commonmark.org/help for more information.\n"
      assert(links(source) == Chunk(("http://www.commonmark.org/help", "www.commonmark.org/help")))
      assert(plainAt(source, 0) == "Visit www.commonmark.org/help for more information.")
    }

    "trim a trailing full stop" in {
      given MdProfile = MdProfile.gfm
      assert(links("visit www.commonmark.org.\n") == Chunk(("http://www.commonmark.org", "www.commonmark.org")))
    }

    "trim a trailing full stop but keep the interior ones (spec example 623)" in {
      given MdProfile = MdProfile.gfm
      val source      = "Visit www.commonmark.org.\n\nVisit www.commonmark.org/a.b.\n"
      assert(linksIn(source, 0) == Chunk(("http://www.commonmark.org", "www.commonmark.org")))
      assert(linksIn(source, 1) == Chunk(("http://www.commonmark.org/a.b", "www.commonmark.org/a.b")))
      assert(plainAt(source, 1) == "Visit www.commonmark.org/a.b.")
    }

    /**
     * The four paragraphs of example 624, which is the whole of the parenthesis rule: a balanced pair inside the
     * destination is kept, unmatched trailing ones are given back, and a `(` before the match is never part of it — so
     * the count that decides is taken over the destination alone.
     */
    "balance closing parentheses against the destination (spec example 624)" in {
      given MdProfile = MdProfile.gfm
      val source      =
        "www.google.com/search?q=Markup+(business)\n\n" +
          "www.google.com/search?q=Markup+(business)))\n\n" +
          "(www.google.com/search?q=Markup+(business))\n\n" +
          "(www.google.com/search?q=Markup+(business)\n"
      val expected = "www.google.com/search?q=Markup+(business)"
      assert(linksIn(source, 0) == Chunk((s"http://$expected", expected)))
      assert(linksIn(source, 1) == Chunk((s"http://$expected", expected)))
      assert(plainAt(source, 1) == s"$expected))")
      assert(linksIn(source, 2) == Chunk((s"http://$expected", expected)))
      assert(plainAt(source, 2) == s"($expected)")
      assert(linksIn(source, 3) == Chunk((s"http://$expected", expected)))
      assert(plainAt(source, 3) == s"($expected")
    }

    "balance closing parentheses against the destination" in {
      given MdProfile = MdProfile.gfm
      assert(links("www.google.com/search?q=(business))+ok\n").head._2 == "www.google.com/search?q=(business))+ok")
    }

    "leave an interior ampersand alone (spec example 626)" in {
      given MdProfile = MdProfile.gfm
      val destination = "www.google.com/search?q=commonmark&hl=en"
      assert(links(s"$destination\n") == Chunk((s"http://$destination", destination)))
    }

    "exclude a trailing entity reference" in {
      given MdProfile = MdProfile.gfm
      assert(links("www.google.com/search?q=commonmark&amp;hl;\n").head._2 ==
        "www.google.com/search?q=commonmark")
    }

    /**
     * The rule the specification states, and the one the brief's `&amp;hl;` case cannot reach: `hl` is not a named
     * entity, and the rule asks only that a `&` and one or more alphanumerics precede the `;`.
     */
    "exclude a trailing entity reference, entity name or not (spec example 626)" in {
      given MdProfile = MdProfile.gfm
      val source      = "www.google.com/search?q=commonmark&hl;\n"
      assert(links(source) ==
        Chunk(("http://www.google.com/search?q=commonmark", "www.google.com/search?q=commonmark")))
      assert(plainAt(source, 0) == "www.google.com/search?q=commonmark&hl;")
    }

    "stop at a `<` (spec example 627)" in {
      given MdProfile = MdProfile.gfm
      val source      = "www.commonmark.org/he<lp\n"
      assert(links(source) == Chunk(("http://www.commonmark.org/he", "www.commonmark.org/he")))
      assert(plainAt(source, 0) == "www.commonmark.org/he<lp")
    }

    "link the three schemes the extension knows (spec example 628)" in {
      given MdProfile = MdProfile.gfm
      val source      =
        "http://commonmark.org\n\n" +
          "(Visit https://encrypted.google.com/search?q=Markup+(business))\n\n" +
          "Anonymous FTP is available at ftp://foo.bar.baz.\n"
      assert(linksIn(source, 0) == Chunk(("http://commonmark.org", "http://commonmark.org")))
      val google = "https://encrypted.google.com/search?q=Markup+(business)"
      assert(linksIn(source, 1) == Chunk((google, google)))
      assert(linksIn(source, 2) == Chunk(("ftp://foo.bar.baz", "ftp://foo.bar.baz")))
      assert(plainAt(source, 2) == "Anonymous FTP is available at ftp://foo.bar.baz.")
    }

    "link an email address through mailto" in {
      given MdProfile = MdProfile.gfm
      assert(links("foo@bar.baz\n") == Chunk(("mailto:foo@bar.baz", "foo@bar.baz")))
    }

    /** A `+` is a local-part character and not a domain one, which is the whole of example 630. */
    "take `+` in an email's local part but not in its domain (spec example 630)" in {
      given MdProfile = MdProfile.gfm
      val source      = "hello@mail+xyz.example isn't valid, but hello+xyz@mail.example is.\n"
      assert(links(source) == Chunk(("mailto:hello+xyz@mail.example", "hello+xyz@mail.example")))
    }

    /**
     * Example 631's four paragraphs. A trailing `.` is left outside the address, while a trailing `-` or `_` is not
     * trimmed at all — it disqualifies the whole match, which is why the third and fourth paragraphs hold no link
     * rather than a shortened one.
     */
    "end an email address at the domain's last label (spec example 631)" in {
      given MdProfile = MdProfile.gfm
      val source      = "a.b-c_d@a.b\n\na.b-c_d@a.b.\n\na.b-c_d@a.b-\n\na.b-c_d@a.b_\n"
      val expected    = Chunk(("mailto:a.b-c_d@a.b", "a.b-c_d@a.b"))
      assert(linksIn(source, 0) == expected)
      assert(linksIn(source, 1) == expected)
      assert(plainAt(source, 1) == "a.b-c_d@a.b.")
      assert(linksIn(source, 2).isEmpty)
      assert(linksIn(source, 3).isEmpty)
    }

    "do not link inside a code span" in {
      given MdProfile = MdProfile.gfm
      assert(links("`www.commonmark.org`\n").isEmpty)
    }

    "do not link inside an existing link's destination" in {
      given MdProfile = MdProfile.gfm
      assert(links("[text](http://example.com)\n") == Chunk(("http://example.com", "text")))
    }

    "do not link a bare URL inside a link's label" in {
      given MdProfile = MdProfile.gfm
      assert(links("[see www.example.com](/uri)\n") == Chunk(("/uri", "see www.example.com")))
    }

    "do not link inside raw HTML" in {
      given MdProfile = MdProfile.gfm
      assert(links("""<span title="www.example.com">x</span>""" + "\n").isEmpty)
    }

    /** A `www.` host needs a domain after it, and a domain needs a period: `www.foo` is one label and no link. */
    "require a period in a `www.` host" in {
      given MdProfile = MdProfile.gfm
      assert(links("www.foo\n").isEmpty)
      assert(links("www.foo/bar\n").isEmpty)
    }

    /**
     * The extension is recognized at the start of a text node, after whitespace, or after one of `*`, `_`, `~`, `(`.
     */
    "require a word boundary before the match" in {
      given MdProfile = MdProfile.gfm
      assert(links("a-www.example.com\n").isEmpty)
      assert(links("(www.example.com\n").head._2 == "www.example.com")
    }

    /**
     * A run the parse rewrote is left as prose. Every span this pass makes points at real source bytes, and a value
     * holding a resolved escape or character reference has none to point at — `www\.example.com` is fifteen characters
     * of text over sixteen of source. cmark-gfm links both of these; here the concrete syntax tree could not tile the
     * result, so recognition stops where the rewrite begins, and an escape becomes a way to spell a bare URL that stays
     * text.
     */
    "leave a destination the source rewrote as prose" in {
      given MdProfile = MdProfile.gfm
      assert(links("www\\.example.com\n").isEmpty)
      assert(links("www.google.com/search?q=a&amp;b\n").head._2 == "www.google.com/search?q=a")
    }

    "are off under the CommonMark profile" in {
      given MdProfile = MdProfile.commonmark
      assert(links("visit www.commonmark.org\n").isEmpty)
      assert(links("foo@bar.baz\n").isEmpty)
      assert(links("http://commonmark.org\n").isEmpty)
    }

    "tile the source exactly and print it back byte for byte" in {
      given MdProfile = MdProfile.gfm
      val source      = "> see www.example.com/a(b) and mail foo@bar.baz.\n"
      val document    = CstParser.parse(source)
      assert(Cst.print(document) == source, s"printed ${Cst.print(document)}")
      assert(
        Cst.tilingErrors(document, source.length).isEmpty,
        Cst.tilingErrors(document, source.length).mkString("; ")
      )
    }

    /**
     * A quote whose paragraph runs over two lines is where a span has to be *mapped* rather than counted: the text the
     * inline parser reads is the two lines joined, and it is two characters shorter than the source it came from,
     * because the second line's `> ` marker is not in it. A link's span is taken through the same `Int => Int` map the
     * rest of inline parsing uses, so it still points at the bytes the author wrote.
     */
    "span a link correctly inside a quote's continuation line" in {
      given MdProfile = MdProfile.gfm
      val source      = "> a\n> www.example.com\n"
      val document    = CstParser.parse(source)
      assert(Cst.print(document) == source, s"printed ${Cst.print(document)}")
      assert(
        Cst.tilingErrors(document, source.length).isEmpty,
        Cst.tilingErrors(document, source.length).mkString("; ")
      )
      val quoted = Parser.parse(source).getOrThrow.children.head match
        case MdNode.Blockquote(children, _) => children.head
        case other                          => throw new AssertionError(s"expected a quote, got $other")
      assert(walk(quoted.asInstanceOf[MdNode.Paragraph].children) ==
        Chunk(("http://www.example.com", "www.example.com")))
    }

    /** A table cell is its own inline region, with its own offset map and its own tiling. */
    "link a bare URL inside a table cell" in {
      given MdProfile = MdProfile.gfm
      val source      = "| a | b |\n| - | - |\n| see www.example.com | x |\n"
      val document    = CstParser.parse(source)
      assert(Cst.print(document) == source, s"printed ${Cst.print(document)}")
      assert(
        Cst.tilingErrors(document, source.length).isEmpty,
        Cst.tilingErrors(document, source.length).mkString("; ")
      )
      val cell = Parser.parse(source).getOrThrow.children.head match
        case MdNode.Table(_, _, rows, _) => rows.head.children.head
        case other                       => throw new AssertionError(s"expected a table, got $other")
      assert(walk(cell.children) == Chunk(("http://www.example.com", "www.example.com")))
    }

    "lower from the CST to the tree the direct parse produces" in {
      given MdProfile = MdProfile.gfm
      val source      = "see www.example.com now\n"
      assert(
        normalize(Lower.lower(CstParser.parse(source)).unpositioned) ==
          normalize(Parser.parse(source).getOrThrow.unpositioned)
      )
    }

    "round-trip a bare URL in text without inventing a link" in {
      given MdProfile = MdProfile.gfm
      given MdStyle   = MdStyle()
      val tree    = MdNode.Root(Chunk(MdNode.Paragraph(Chunk(MdNode.Text("see www.example.com and foo@bar.baz now")))))
      val written = MdWriter.write(tree)
      assert(written == "see www&#46;example.com and foo&#64;bar.baz now\n", written)
      assert(
        normalize(Parser.parse(written).getOrThrow.unpositioned) == normalize(tree.unpositioned),
        s"round trip changed: $written"
      )
      // What the writer spells still tiles: the references it spent are entities like any other.
      val raised = MdWriter.raise(tree)
      assert(Cst.tilingErrors(raised, written.length).isEmpty, Cst.tilingErrors(raised, written.length).mkString("; "))
      assert(Cst.print(raised) == written)
    }
  }
