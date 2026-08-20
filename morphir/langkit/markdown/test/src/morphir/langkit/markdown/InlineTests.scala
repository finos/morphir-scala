package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

/**
 * Inline content is the AST's second level: a block that can hold prose holds a sequence of [[Inline]] rather than a
 * `String`.
 *
 * Only `Inline.Text` exists at this point, so a parse produces exactly one of them per block and no output changes. The
 * shape is what later slices need — code spans, links and emphasis all arrive as further `Inline` cases.
 */
class InlineTests extends Test[Any]:

  /** The inline content of a source's first block. */
  private def inlines(source: String): Chunk[Inline] =
    parse(source).blocks.head match
      case Block.Paragraph(content, _) => content
      case other                       => throw new AssertionError(s"expected a paragraph, got $other")

  private def destinations(source: String): Chunk[String] =
    inlines(source).collect { case Inline.Link(destination, _, _, _) => destination }

  /** The literal text of inline content, ignoring how it is split into nodes. */
  /**
   * The prose of a one-paragraph list item.
   *
   * A list item holds blocks, so even the shortest one is a paragraph. Tests that only care what an item says go
   * through this rather than repeating the unwrap.
   */
  private def paragraphOf(item: ListItem): Chunk[Inline] =
    item.content.headOption match
      case Some(Block.Paragraph(content, _)) => content
      case _                                 => Chunk.empty

  private def textOf(content: Chunk[Inline]): String =
    content.map {
      case Inline.Text(value, _)           => value
      case Inline.CodeSpan(value, _)       => value
      case Inline.Link(_, _, inner, _)     => textOf(inner)
      case Inline.Image(_, _, alt, _)      => alt
      case Inline.Emphasis(inner, _)       => textOf(inner)
      case Inline.StrongEmphasis(inner, _) => textOf(inner)
      // Raw HTML is markup rather than text, and contributes none: a test asserting on it matches the node itself.
      case Inline.RawHtml(_, _) => ""
      // A hard break reads as the line ending it stands for, so a test that only cares what the prose says need not
      // know which kind of break produced it.
      case Inline.LineBreak(_) => "\n"
    }.mkString

  /** The values of every code span a source produces, ignoring the text around them. */
  private def codeSpans(source: String): Chunk[String] =
    parse(source).blocks.head match
      case Block.Paragraph(content, _) => content.collect { case Inline.CodeSpan(value, _) => value }
      case other                       => throw new AssertionError(s"expected a paragraph, got $other")

  private def parse(source: String): Document =
    Parser.parse(source) match
      case Result.Success(document) => document
      case other                    => throw new AssertionError(s"parse failed: $other")

  "a heading" - {
    "carries its prose as inline content" in {
      val heading = parse("# Title").blocks.head
      assert(heading == Block.Heading(HeadingLevel.One, Chunk(Inline.Text("Title", Span(2, 5))), Span(0, 7)))
    }
    "reports the span its text occupies in the source, not the whole line" in {
      parse("#   Title").blocks.head match
        case Block.Heading(_, Chunk(Inline.Text(value, span)), _) =>
          assert(value == "Title")
          assert(span == Span(4, 5))
        case other => assert(false, s"expected a heading, got $other")
    }
  }

  "a paragraph" - {
    "carries its prose as one text node" in {
      parse("hello").blocks.head match
        case Block.Paragraph(content, _) =>
          assert(content == Chunk(Inline.Text("hello", Span(0, 5))))
        case other => assert(false, s"expected a paragraph, got $other")
    }
    "keeps a soft line break inside the text, uncollapsed" in {
      parse("alpha\nbeta").blocks.head match
        case Block.Paragraph(Chunk(Inline.Text(value, _)), _) => assert(value == "alpha\nbeta")
        case other                                            => assert(false, s"expected one text node, got $other")
    }
  }

  "a bullet list" - {
    // An item spans its whole line, marker and all, because that is what it occupies. Its paragraph spans only the
    // content, so an inline node inside it still points at the source.
    "gives every item its own content and span" in {
      parse("- one\n- two").blocks.head match
        case Block.UnorderedList(items, _, _) =>
          assert(items.size == 2)
          assert(items(0) ==
            ListItem(Chunk(Block.Paragraph(Chunk(Inline.Text("one", Span(2, 3))), Span(2, 3))), Span(0, 5)))
          assert(items(1) ==
            ListItem(Chunk(Block.Paragraph(Chunk(Inline.Text("two", Span(8, 3))), Span(8, 3))), Span(6, 5)))
        case other => assert(false, s"expected a list, got $other")
    }
  }

  "a code span" - {
    "is parsed out of surrounding text (spec example 328)" in {
      parse("`foo`").blocks.head match
        case Block.Paragraph(content, _) => assert(content == Chunk(Inline.CodeSpan("foo", Span(0, 5))))
        case other                       => assert(false, s"expected a paragraph, got $other")
    }
    "splits a run of text into the pieces around it" in {
      parse("a `b` c").blocks.head match
        case Block.Paragraph(content, _) =>
          assert(content.size == 3)
          assert(content(0) == Inline.Text("a ", Span(0, 2)))
          assert(content(1) == Inline.CodeSpan("b", Span(2, 3)))
          assert(content(2) == Inline.Text(" c", Span(5, 2)))
        case other => assert(false, s"expected a paragraph, got $other")
    }
    "closes on a backtick run of the same length, not a shorter one (spec example 339)" in
      assert(codeSpans("``foo`bar``") == Chunk("foo`bar"))
    "strips one space from each end when both ends have one (spec example 329)" in
      assert(codeSpans("`` foo ` bar ``") == Chunk("foo ` bar"))
    "strips only one space, leaving the rest (spec example 331)" in
      assert(codeSpans("`  ``  `") == Chunk(" `` "))
    "does not strip when only one end has a space (spec example 332)" in
      assert(codeSpans("` a`") == Chunk(" a"))
    "does not strip when the content is all spaces (spec example 334)" in
      assert(codeSpans("`  `") == Chunk("  "))
    "treats a non-breaking space as content, not as strippable space (spec example 333)" in
      assert(codeSpans("`\u00a0b\u00a0`") == Chunk("\u00a0b\u00a0"))
    "turns line endings into spaces (spec example 335)" in
      assert(codeSpans("``\nfoo\nbar  \nbaz\n``") == Chunk("foo bar   baz"))
    "does not honour a backslash escape (spec example 338)" in
      assert(codeSpans("`foo\\`bar`") == Chunk("foo\\"))
    "leaves an unmatched backtick run as literal text (spec example 348)" in {
      parse("`foo").blocks.head match
        case Block.Paragraph(content, _) => assert(content == Chunk(Inline.Text("`foo", Span(0, 4))))
        case other                       => assert(false, s"expected a paragraph, got $other")
    }
    "leaves a run with no equal-length closer as literal text (spec example 347)" in {
      parse("```foo``").blocks.head match
        case Block.Paragraph(content, _) => assert(content == Chunk(Inline.Text("```foo``", Span(0, 8))))
        case other                       => assert(false, s"expected a paragraph, got $other")
    }
    "is not scanned inside fenced code" in {
      parse("```\n`foo`\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "`foo`\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }

  "a link" - {
    "carries destination, title and label content (spec example 482)" in {
      inlines("[link](/uri \"title\")") match
        case Chunk(Inline.Link(destination, title, content, _)) =>
          assert(destination == "/uri")
          assert(title == Present("title"))
          assert(content == Chunk(Inline.Text("link", Span(1, 4))))
        case other => assert(false, s"expected one link, got $other")
    }
    "has no title when none is given (spec example 483)" in {
      inlines("[link](/uri)") match
        case Chunk(Inline.Link(destination, title, _, _)) =>
          assert(destination == "/uri")
          assert(title == Absent)
        case other => assert(false, s"expected one link, got $other")
    }
    "allows an empty label (spec example 484)" in {
      inlines("[](./target.md)") match
        case Chunk(Inline.Link(destination, _, content, _)) =>
          assert(destination == "./target.md")
          assert(content.isEmpty)
        case other => assert(false, s"expected one link, got $other")
    }
    "takes an angle-bracketed destination, spaces and all (spec example 489)" in
      assert(destinations("[link](</my uri>)") == Chunk("/my%20uri"))
    "percent-encodes what a URI cannot carry literally (spec example 502)" in
      assert(destinations("[link](foo\\bar)") == Chunk("foo%5Cbar"))
    "keeps an ampersand for the writer to escape (spec example 595 shape)" in
      assert(destinations("[a](http://x/?q=1&r=2)") == Chunk("http://x/?q=1&r=2"))
    "nests balanced brackets inside the label (spec example 512)" in {
      inlines("[link [foo [bar]]](/uri)") match
        case Chunk(Inline.Link(_, _, content, _)) =>
          assert(textOf(content) == "link [foo [bar]]")
        case other => assert(false, s"expected one link, got $other")
    }
    "loses to a code span, which binds tighter (spec example 342)" in {
      val content = inlines("[not a `link](/foo`)")
      assert(content.exists { case _: Inline.CodeSpan => true; case _ => false })
      assert(!content.exists { case _: Inline.Link => true; case _ => false })
    }
    "parses its label as inline content, so a code span inside it survives" in {
      inlines("[a `b` c](/u)") match
        case Chunk(Inline.Link(_, _, content, _)) =>
          assert(content.size == 3)
          assert(content(1) == Inline.CodeSpan("b", Span(3, 3)))
        case other => assert(false, s"expected one link, got $other")
    }
  }

  "an image" - {
    "carries destination, title and flattened alt text (spec example 572)" in {
      inlines("![foo](/url \"title\")") match
        case Chunk(Inline.Image(destination, title, alt, _)) =>
          assert(destination == "/url")
          assert(title == Present("title"))
          assert(alt == "foo")
        case other => assert(false, s"expected one image, got $other")
    }
    "flattens a nested image into its alt text (spec example 574)" in {
      inlines("![foo ![bar](/url)](/url2)") match
        case Chunk(Inline.Image(destination, _, alt, _)) =>
          assert(destination == "/url2")
          assert(alt == "foo bar")
        case other => assert(false, s"expected one image, got $other")
    }
    "flattens a nested link into its alt text (spec example 575)" in {
      inlines("![foo [bar](/url)](/url2)") match
        case Chunk(Inline.Image(_, _, alt, _)) => assert(alt == "foo bar")
        case other                             => assert(false, s"expected one image, got $other")
    }
    "allows an empty alt (spec example 581)" in {
      inlines("![](/url)") match
        case Chunk(Inline.Image(_, _, alt, _)) => assert(alt.isEmpty)
        case other                             => assert(false, s"expected one image, got $other")
    }
  }

  "an autolink" - {
    "becomes a link whose text is the raw URI (spec example 594 shape)" in {
      inlines("<https://foo.bar.baz>") match
        case Chunk(Inline.Link(destination, title, content, _)) =>
          assert(destination == "https://foo.bar.baz")
          assert(title == Absent)
          assert(textOf(content) == "https://foo.bar.baz")
        case other => assert(false, s"expected one link, got $other")
    }
    "encodes the destination while leaving the text raw (spec example 346)" in {
      inlines("<https://foo.bar.`baz>") match
        case Chunk(Inline.Link(destination, _, content, _)) =>
          assert(destination == "https://foo.bar.%60baz")
          assert(textOf(content) == "https://foo.bar.`baz")
        case other => assert(false, s"expected one link, got $other")
    }
    "is not made from something that is not an absolute URI" in
      // `<33>` is neither an autolink nor a valid tag, so it stays escaped text. `<not a link>` would not do
      // here: it IS a valid open tag -- name `not`, attributes `a` and `link` -- so CommonMark reads it as raw HTML.
      assert(inlines("<33>").forall { case _: Inline.Link => false; case _ => true })
  }

  "emphasis" - {
    "wraps a single delimiter run (spec example 350)" in {
      inlines("*foo bar*") match
        case Chunk(Inline.Emphasis(content, _)) => assert(textOf(content) == "foo bar")
        case other                              => assert(false, s"expected emphasis, got $other")
    }
    "wraps a double run as strong (spec example 378)" in {
      inlines("**foo bar**") match
        case Chunk(Inline.StrongEmphasis(content, _)) => assert(textOf(content) == "foo bar")
        case other                                    => assert(false, s"expected strong emphasis, got $other")
    }
    "underscores work the same at a word boundary (spec example 382)" in {
      inlines("__foo bar__") match
        case Chunk(Inline.StrongEmphasis(_, _)) => assert(true)
        case other                              => assert(false, s"expected strong emphasis, got $other")
    }
    "stays literal when the opener is followed by whitespace (spec example 351)" in {
      assert(textOf(inlines("a * foo bar*")) == "a * foo bar*")
      assert(!inlines("a * foo bar*").exists { case _: Inline.Emphasis => true; case _ => false })
    }
    "keeps an intraword underscore literal (spec example 360)" in
      assert(!inlines("foo_bar_").exists { case _: Inline.Emphasis => true; case _ => false })
    "nests strong inside emphasis (spec example 410)" in {
      inlines("*foo **bar** baz*") match
        case Chunk(Inline.Emphasis(content, _)) =>
          assert(content.exists { case _: Inline.StrongEmphasis => true; case _ => false })
        case other => assert(false, s"expected emphasis, got $other")
    }
    "leaves an unmatched delimiter behind as text (spec example 443)" in {
      val content = inlines("*foo**")
      assert(content.size == 2)
      assert(content(0).isInstanceOf[Inline.Emphasis])
      assert(content(1) == Inline.Text("*", Span(5, 1)))
    }
    "does not treat a lone run as a delimiter pair (spec example 436)" in
      assert(textOf(inlines("foo ***")) == "foo ***")
    "a backslash-escaped asterisk never opens emphasis" in
      assert(!inlines("\\*foo\\*").exists { case _: Inline.Emphasis => true; case _ => false })
  }

  "indented code" - {
    "reads four spaces of indentation as a code block (spec example 107)" in {
      parse("    a simple\n      indented code block").blocks.head match
        case Block.IndentedCode(content, _) => assert(content == "a simple\n  indented code block\n")
        case other                          => assert(false, s"expected indented code, got $other")
    }
    "keeps blank lines between chunks but not at the end (spec example 111)" in {
      parse("    chunk1\n\n    chunk2\n").blocks.head match
        case Block.IndentedCode(content, _) => assert(content == "chunk1\n\nchunk2\n")
        case other                          => assert(false, s"expected indented code, got $other")
    }
    "beats every other block opener, so an indented hash is not a heading" in {
      parse("    # not a heading").blocks.head match
        case Block.IndentedCode(content, _) => assert(content == "# not a heading\n")
        case other                          => assert(false, s"expected indented code, got $other")
    }
    // The indentation goes with it: a paragraph's content is its lines with their leading whitespace removed, so the
    // second line reads `bar` and not `    bar`. This test asserted the indentation was kept, which was our behaviour
    // rather than the spec's -- the fixture renders `<p>Foo\nbar</p>`.
    "does not interrupt a paragraph, and loses its indentation to it (spec example 113)" in {
      parse("Foo\n    bar").blocks.head match
        case Block.Paragraph(content, _) => assert(textOf(content) == "Foo\nbar")
        case other                       => assert(false, s"expected a paragraph, got $other")
    }
    "strips only four spaces, leaving deeper indentation as content (spec example 116)" in {
      parse("        foo\n    bar").blocks.head match
        case Block.IndentedCode(content, _) => assert(content == "    foo\nbar\n")
        case other                          => assert(false, s"expected indented code, got $other")
    }
  }

  "a link reference definition" - {
    "resolves a shortcut reference (spec example 192)" in {
      inlines("[foo]: /url \"title\"\n\n[foo]") match
        case Chunk(Inline.Link(destination, title, content, _)) =>
          assert(destination == "/url")
          assert(title == Present("title"))
          assert(textOf(content) == "foo")
        case other => assert(false, s"expected one link, got $other")
    }
    "resolves a reference declared after its use (spec example 561)" in
      assert(destinations("[Foo]\n\n[foo]: /url \"title\"") == Chunk("/url"))
    "matches labels case-insensitively and on collapsed whitespace" in
      assert(destinations("[Foo   Bar]\n\n[foo bar]: /url") == Chunk("/url"))
    "resolves a full reference (spec example 570 shape)" in
      assert(destinations("[foo][bar]\n\n[bar]: /url2") == Chunk("/url2"))
    "resolves a collapsed reference" in
      assert(destinations("[foo][]\n\n[foo]: /url") == Chunk("/url"))
    "resolves an image reference (spec example 588)" in {
      inlines("![foo]\n\n[foo]: /url \"title\"") match
        case Chunk(Inline.Image(destination, title, alt, _)) =>
          assert(destination == "/url")
          assert(title == Present("title"))
          assert(alt == "foo")
        case other => assert(false, s"expected one image, got $other")
    }
    "contributes no block of its own" in {
      val document = parse("[foo]: /url\n\nbody")
      assert(document.blocks.size == 1)
      document.blocks.head match
        case Block.Paragraph(content, _) => assert(textOf(content) == "body")
        case other                       => assert(false, s"expected one paragraph, got $other")
    }
    "keeps the first of two definitions for the same label" in
      assert(destinations("[foo]: /first\n[foo]: /second\n\n[foo]") == Chunk("/first"))
    "is not a definition when the line carries trailing content (spec example 209)" in {
      val document = parse("[foo]: /url \"title\" ok")
      assert(document.blocks.size == 1)
      assert(document.blocks.head.isInstanceOf[Block.Paragraph])
    }
    "leaves an unresolved reference as literal text" in {
      assert(destinations("[nope]") == Chunk.empty)
      assert(textOf(inlines("[nope]")) == "[nope]")
    }
  }

  "a setext heading" - {
    "an equals underline makes a level-one heading" in {
      parse("Title\n=====").blocks.head match
        case Block.Heading(level, content, _) =>
          assert(level == HeadingLevel.One)
          assert(textOf(content) == "Title")
        case other => assert(false, s"expected a heading, got $other")
    }
    "a dash underline makes a level-two heading" in {
      parse("Title\n-----").blocks.head match
        case Block.Heading(level, _, _) => assert(level == HeadingLevel.Two)
        case other                      => assert(false, s"expected a heading, got $other")
    }
    "a dash run with nothing above it stays a thematic break" in {
      parse("-----").blocks.head match
        case Block.ThematicBreak(_) => assert(true)
        case other                  => assert(false, s"expected a thematic break, got $other")
    }
    "its content is inline, so a code span inside survives" in {
      parse("a `b`\n===").blocks.head match
        case Block.Heading(_, content, _) =>
          assert(content.exists { case _: Inline.CodeSpan => true; case _ => false })
        case other => assert(false, s"expected a heading, got $other")
    }
  }

  "an ordered list" - {
    "reads consecutive numbered items as one list" in {
      parse("1. one\n2. two").blocks.head match
        case Block.OrderedList(start, items, _, _) =>
          assert(start == 1)
          assert(items.map(item => textOf(paragraphOf(item))) == Chunk("one", "two"))
        case other => assert(false, s"expected an ordered list, got $other")
    }
    "keeps the first marker's number as the start" in {
      parse("3. three\n4. four").blocks.head match
        case Block.OrderedList(start, items, _, _) =>
          assert(start == 3)
          assert(items.size == 2)
        case other => assert(false, s"expected an ordered list, got $other")
    }
    "a change of delimiter begins a new list (spec example 302)" in {
      val blocks = parse("1. foo\n2. bar\n3) baz").blocks
      assert(blocks.size == 2)
      assert(blocks(0).isInstanceOf[Block.OrderedList])
      assert(blocks(1).isInstanceOf[Block.OrderedList])
    }
    "only a list starting at one may interrupt a paragraph (spec example 304)" in {
      val blocks = parse("The number of windows in my house is\n14.  The number of doors is 6.").blocks
      assert(blocks.size == 1)
      assert(blocks.head.isInstanceOf[Block.Paragraph])
    }
    "a list starting at one does interrupt a paragraph" in {
      val blocks = parse("text\n1. item").blocks
      assert(blocks.size == 2)
      assert(blocks(1).isInstanceOf[Block.OrderedList])
    }
  }

  "an HTML block" - {
    "passes a known block tag through verbatim, Markdown and all (spec example 189)" in {
      parse("<div>\n*Emphasized* text.\n</div>").blocks.head match
        case Block.HtmlBlock(content, _) => assert(content == "<div>\n*Emphasized* text.\n</div>")
        case other                       => assert(false, s"expected an HTML block, got $other")
    }
    "ends at a blank line for a known tag (spec example 190)" in {
      val blocks = parse("<table>\n\n<tr>\n").blocks
      assert(blocks.size == 2)
      assert(blocks.forall(_.isInstanceOf[Block.HtmlBlock]))
    }
    "runs to the closing tag for a script-like element" in {
      parse("<style>\n\nh1 {}\n</style>").blocks.head match
        case Block.HtmlBlock(content, _) => assert(content.contains("h1 {}"))
        case other                       => assert(false, s"expected an HTML block, got $other")
    }
    "takes a comment to its terminator" in {
      parse("<!-- a\n\nb -->").blocks.head match
        case Block.HtmlBlock(content, _) => assert(content == "<!-- a\n\nb -->")
        case other                       => assert(false, s"expected an HTML block, got $other")
    }
    "needs a valid tag for the any-tag condition (spec example 619)" in {
      // `h*#ref` is not a valid attribute name, so this is not a tag and stays prose.
      parse("<a h*#ref=\"hi\">").blocks.head match
        case Block.Paragraph(_, _) => assert(true)
        case other                 => assert(false, s"expected a paragraph, got $other")
    }
    "opens on a bare closing tag, which the any-tag condition allows" in {
      // Regression guard: this was unreachable while closingTagEnd checked charAt(1) -- the `/` -- rather than the
      // first character of the name.
      parse("</div>\ncontent").blocks.head match
        case Block.HtmlBlock(content, _) => assert(content == "</div>\ncontent")
        case other                       => assert(false, s"expected an HTML block, got $other")
    }
    "rejects a closing tag carrying attributes (spec example 624)" in {
      parse("</a href=\"foo\">").blocks.head match
        case Block.Paragraph(_, _) => assert(true)
        case other                 => assert(false, s"expected a paragraph, got $other")
    }
    "the any-tag condition does not interrupt a paragraph (spec example 187)" in {
      val blocks = parse("Foo\n<a href=\"bar\">\nbaz").blocks
      assert(blocks.size == 1)
      assert(blocks.head.isInstanceOf[Block.Paragraph])
    }
    "an indented opener is code, not an HTML block (spec example 231)" in {
      parse("    <div>").blocks.head match
        case Block.IndentedCode(content, _) => assert(content == "<div>\n")
        case other                          => assert(false, s"expected indented code, got $other")
    }
  }

  "a link" - {

    def inlines(source: String): Chunk[Inline] =
      parse(source).blocks.head match
        case Block.Paragraph(content, _) => content
        case other                       => throw new AssertionError(s"expected a paragraph, got $other")

    def destinations(source: String): Chunk[String] =
      def walk(node: Inline): Chunk[String] = node match
        case Inline.Link(destination, _, inner, _) => Chunk(destination) ++ inner.flatMap(walk)
        case Inline.Emphasis(inner, _)             => inner.flatMap(walk)
        case Inline.StrongEmphasis(inner, _)       => inner.flatMap(walk)
        case _                                     => Chunk.empty
      inlines(source).flatMap(walk)

    // The bracket that would have opened the outer link is ordinary text instead, and the link inside it stands.
    "may not contain a link (spec examples 518 and 532)" in {
      assert(destinations("[foo [bar](/uri)](/uri)\n") == Chunk("/uri"), "the outer bracket became a link")
      assert(destinations("[foo [bar](/uri)][ref]\n\n[ref]: /uri\n").size == 2, "expected the inner link and [ref]")
    }
    "sees a link through emphasis when it applies that rule (spec example 519)" in
      assert(destinations("[foo *[bar [baz](/uri)](/uri)*](/uri)\n") == Chunk("/uri"))

    // An image is not bound by it: its content becomes alt text, where a nested link flattens to what it says.
    "lets an image hold what a link may not (spec example 520)" in {
      inlines("![[[foo](uri1)](uri2)](uri3)\n").head match
        case Inline.Image(destination, _, alt, _) =>
          assert(destination == "uri3")
          assert(alt == "[foo](uri2)")
        case other => assert(false, s"expected an image, got $other")
    }

    // A label is not link text. Text may hold balanced brackets; a label may hold none, and must hold something.
    "will not take a label that holds a bracket or nothing (spec examples 547, 548, 551 and 552)" in {
      val notLinks = Chunk(
        "[foo][ref[bar]]\n\n[ref[bar]]: /uri\n",
        "[[[foo]]]\n\n[[[foo]]]: /url\n",
        "[]\n\n[]: /uri\n",
        "[\n ]\n\n[\n ]: /uri\n"
      )
      notLinks.foreach { source =>
        assert(destinations(source).isEmpty, s"a link was formed in $source")
        // The definition is no more valid than the reference, so both lines stay prose.
        assert(parse(source).blocks.size == 2, s"expected two paragraphs from $source")
      }
    }
    "still takes balanced brackets in link text (spec example 512)" in
      assert(destinations("[link [foo [bar]]](/uri)\n") == Chunk("/uri"))

    // Case *folding*, not lowercasing: `ẞ` lowercases to `ß` and would never meet `SS`, but both fold to `ss`.
    "folds case when it matches a label (spec example 540)" in
      assert(destinations("[ẞ]\n\n[SS]: /url\n") == Chunk("/url"))

    "resolves references in a destination before encoding it (spec example 503)" in
      assert(destinations("[link](foo%20b&auml;)\n") == Chunk("foo%20b%C3%A4"))

    "encodes a reference link's destination the same way (spec example 206)" in
      assert(destinations("[ΑΓΩ]: /φου\n\n[αγω]\n") == Chunk("/%CF%86%CE%BF%CF%85"))

    "resolves references in a title (spec example 506)" in {
      inlines("[link](/url \"title \\\"&quot;\")\n").head match
        case Inline.Link(_, Present(title), _, _) => assert(title == "title \"\"")
        case other                                => assert(false, s"expected a titled link, got $other")
    }

    // An angle-bracketed destination that does not close disqualifies the link outright rather than falling back to
    // the bare form.
    "will not take an angle destination holding a line ending or an escaped close (examples 491 and 493)" in {
      assert(destinations("[link](<foo\nbar>)\n").isEmpty)
      assert(destinations("[link](<foo\\>)\n").isEmpty)
    }
  }

  "an ATX heading" - {

    def headingOf(source: String): (HeadingLevel, String) =
      parse(source).blocks.head match
        case Block.Heading(level, content, _) => (level, textOf(content))
        case other                            => throw new AssertionError(s"expected a heading, got $other")

    // The closing run says nothing: `### foo ###` is the same heading as `### foo`.
    "drops a closing run of hashes (spec examples 71 to 73)" in {
      assert(headingOf("## foo ##\n") == (HeadingLevel.Two, "foo"))
      assert(headingOf("  ###   bar    ###\n") == (HeadingLevel.Three, "bar"))
      assert(headingOf("# foo ##################################\n") == (HeadingLevel.One, "foo"))
      assert(headingOf("### foo ###     \n") == (HeadingLevel.Three, "foo"), "whitespace may follow the closing run")
    }
    // It closes only when whitespace precedes it, so a hash hard against the text is part of the text.
    "keeps a hash the text runs into (spec example 74)" in
      assert(headingOf("# foo#\n") == (HeadingLevel.One, "foo#"))

    "reads a heading that says nothing (spec example 79)" in {
      assert(headingOf("## \n") == (HeadingLevel.Two, ""))
      assert(headingOf("#\n") == (HeadingLevel.One, ""))
      assert(headingOf("### ###\n") == (HeadingLevel.Three, ""))
    }
  }

  "an autolink" - {

    def linkOf(source: String): (String, String) =
      parse(source).blocks.head match
        case Block.Paragraph(content, _) =>
          content.head match
            case Inline.Link(destination, _, inner, _) => (destination, textOf(inner))
            case other                                 => throw new AssertionError(s"expected a link, got $other")
        case other => throw new AssertionError(s"expected a paragraph, got $other")

    def isLink(source: String): Boolean =
      parse(source).blocks.head match
        case Block.Paragraph(content, _) => content.exists(_.isInstanceOf[Inline.Link])
        case _                           => false

    // The address is what it says; `mailto:` is where it points.
    "takes an email address to a mailto destination (spec examples 604 and 605)" in {
      assert(linkOf("<foo@bar.example.com>\n") == ("mailto:foo@bar.example.com", "foo@bar.example.com"))
      assert(linkOf("<foo+special@Bar.baz-bar0.com>\n")._1 == "mailto:foo+special@Bar.baz-bar0.com")
    }
    // A scheme is two to thirty-two characters. The lower bound is load-bearing: without it `<m:abc>` is a link.
    "will not take a scheme of one character (spec example 609)" in
      assert(!isLink("<m:abc>\n"))

    "still takes an ordinary absolute URI (spec example 594)" in
      assert(linkOf("<http://foo.bar.baz>\n") == ("http://foo.bar.baz", "http://foo.bar.baz"))
  }

  "a line ending" - {

    def inlines(source: String): Chunk[Inline] =
      parse(source).blocks.head match
        case Block.Paragraph(content, _) => content
        case other                       => throw new AssertionError(s"expected a paragraph, got $other")

    def breaks(source: String): Int = inlines(source).count(_.isInstanceOf[Inline.LineBreak])

    "is hard when two or more spaces or a backslash precede it (spec examples 633 to 635)" in {
      assert(breaks("foo  \nbaz\n") == 1)
      assert(breaks("foo\\\nbaz\n") == 1, "a backslash before the line ending asks for a break too")
      assert(breaks("foo       \nbaz\n") == 1, "more than two spaces is still one break")
    }
    // One trailing space is neither a break nor content. Deciding that means looking at the whitespace, which is why
    // it is consumed here rather than stripped from the line beforehand -- stripping first would throw away the very
    // thing that says which of the two this is.
    "is soft when one space precedes it, and the space is not content (spec example 649)" in {
      assert(breaks("foo \n baz\n") == 0)
      assert(textOf(inlines("foo \n baz\n")) == "foo\nbaz")
    }
    "survives inside emphasis (spec examples 638 and 639)" in {
      inlines("*foo  \nbar*\n").head match
        case Inline.Emphasis(inner, _) => assert(inner.exists(_.isInstanceOf[Inline.LineBreak]))
        case other                     => assert(false, s"expected emphasis, got $other")
      inlines("*foo\\\nbar*\n").head match
        case Inline.Emphasis(inner, _) => assert(inner.exists(_.isInstanceOf[Inline.LineBreak]))
        case other                     => assert(false, s"expected emphasis, got $other")
    }
    "takes the indentation off the line that follows it (spec examples 636 and 637)" in {
      assert(textOf(inlines("foo  \n     bar\n")) == "foo\nbar")
      assert(textOf(inlines("foo\\\n     bar\n")) == "foo\nbar")
    }
    // A run of spaces that reaches no line ending is ordinary text, and so is a backslash before anything else.
    "leaves spaces and backslashes that reach no line ending alone" in {
      assert(textOf(inlines("foo  bar\n")) == "foo  bar")
      assert(breaks("foo  bar\n") == 0)
      assert(textOf(inlines("foo\\bar\n")) == "foo\\bar")
    }
    "reports the span the break occupies in the source" in {
      val source = "foo  \nbaz\n"
      inlines(source).collectFirst { case Inline.LineBreak(span) => span } match
        case Some(span) => assert(source.substring(span.offset, span.end) == "  \n")
        case None       => assert(false, "expected a hard break")
    }
  }

  "raw HTML" - {

    /** The inline nodes of a document that is one paragraph. */
    def inlines(source: String): Chunk[Inline] =
      parse(source).blocks.head match
        case Block.Paragraph(content, _) => content
        case other                       => throw new AssertionError(s"expected a paragraph, got $other")

    def htmlIn(source: String): Chunk[String] =
      inlines(source).collect { case Inline.RawHtml(value, _) => value }

    "takes open and closing tags whole (spec examples 613 and 623)" in {
      assert(htmlIn("<a><bab><c2c>\n") == Chunk("<a>", "<bab>", "<c2c>"))
      assert(htmlIn("</a></foo >\n") == Chunk("</a>", "</foo >"))
    }
    "lets a tag run across a line break (spec example 615)" in
      assert(htmlIn("<a  /><b2\ndata=\"foo\" >\n") == Chunk("<a  />", "<b2\ndata=\"foo\" >"))

    // The whole reason this scans rather than looking for the next `>`. Each of these is a `<` the spec leaves as an
    // ordinary character, and the writer escapes it.
    "leaves a `<` that opens nothing as text (spec examples 618, 619, 622 and 624)" in {
      val notHtml = Chunk(
        "<33> <__>\n",                 // a name must start with a letter
        "<a h*#ref=\"hi\">\n",         // no legal attribute name
        "<a href='bar'title=title>\n", // attributes that do not separate
        "</a href=\"foo\">\n"          // a closing tag takes no attributes
      )
      notHtml.foreach(source => assert(htmlIn(source).isEmpty, s"expected no raw HTML in $source"))
    }
    "takes the four forms that are not tags (spec examples 625 to 629)" in {
      assert(htmlIn("foo <!-- this is a --\ncomment - with hyphens -->\n") ==
        Chunk("<!-- this is a --\ncomment - with hyphens -->"))
      // `<!-->` is a comment in its own right, so what follows it is ordinary text rather than more comment.
      assert(htmlIn("foo <!--> foo -->\n") == Chunk("<!-->"))
      assert(htmlIn("foo <!---> foo -->\n") == Chunk("<!--->"))
      assert(htmlIn("foo <?php echo $a; ?>\n") == Chunk("<?php echo $a; ?>"))
      assert(htmlIn("foo <!ELEMENT br EMPTY>\n") == Chunk("<!ELEMENT br EMPTY>"))
      assert(htmlIn("foo <![CDATA[>&<]]>\n") == Chunk("<![CDATA[>&<]]>"))
    }
    // The point of the node: its interior is never looked at again. An entity inside it is not decoded and a backslash
    // inside it does not escape, because neither is ever offered to the parser that would do so.
    "looks at nothing inside what it took (spec examples 630 and 631)" in {
      assert(htmlIn("foo <a href=\"&ouml;\">\n") == Chunk("<a href=\"&ouml;\">"))
      assert(htmlIn("foo <a href=\"\\*\">\n") == Chunk("<a href=\"\\*\">"))
    }
    "outranks emphasis, so a delimiter inside a tag is not one (spec example 476)" in {
      val content = inlines("**<a href=\"**\">\n")
      assert(!content.exists(_.isInstanceOf[Inline.StrongEmphasis]), s"emphasis reached inside the tag: $content")
      assert(htmlIn("**<a href=\"**\">\n") == Chunk("<a href=\"**\">"))
    }
    // A `]` inside an attribute is the attribute's. Before raw HTML was recognised, the label closed there and the
    // whole thing became a link.
    "outranks a link label, so a bracket inside a tag does not close one (spec example 524)" in {
      val content = inlines("[foo <bar attr=\"](baz)\">\n")
      assert(!content.exists(_.isInstanceOf[Inline.Link]), s"a link was formed through the tag: $content")
    }
    "gives an autolink the same standing inside a label (spec example 526)" in {
      val content = inlines("[foo<https://example.com/?search=](uri)>\n")
      val links   = content.collect { case Inline.Link(destination, _, _, _) => destination }
      assert(links.size == 1, s"expected only the autolink, got $links")
      assert(links.head.contains("search="), s"the autolink lost its URI: $links")
    }
    "reports the span the markup occupies in the source" in {
      val source = "text <br /> more\n"
      inlines(source).collectFirst { case Inline.RawHtml(value, span) => (value, span) } match
        case Some((value, span)) =>
          assert(value == "<br />")
          assert(source.substring(span.offset, span.end) == "<br />")
        case None => assert(false, "expected raw HTML")
    }
  }

  "a character reference" - {
    "decodes a decimal reference (spec example 26)" in
      assert(textOf(inlines("&#35;")) == "#")
    "decodes a hexadecimal reference in either case (spec example 27)" in {
      assert(textOf(inlines("&#X22;")) == "\"")
      assert(textOf(inlines("&#x22;")) == "\"")
    }
    "replaces an unrepresentable code point (spec example 26)" in
      assert(textOf(inlines("&#0;")) == "\uFFFD")
    "decodes a known name" in {
      assert(textOf(inlines("&amp;")) == "&")
      assert(textOf(inlines("&nbsp;")) == "\u00a0")
    }
    "leaves an unknown name literal (spec example 30)" in
      assert(textOf(inlines("&MadeUpEntity;")) == "&MadeUpEntity;")
    "leaves a name with no semicolon literal (spec example 29)" in
      assert(textOf(inlines("&copy")) == "&copy")
    "leaves a malformed numeric reference literal (spec example 28)" in {
      assert(textOf(inlines("&#;")) == "&#;")
      assert(textOf(inlines("&#x;")) == "&#x;")
    }
    "is not decoded inside a code span" in
      assert(codeSpans("`&amp;`") == Chunk("&amp;"))
  }

  "fenced code" - {
    "keeps its body as literal text, never inline content" in {
      parse("```\n*not emphasis*\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "*not emphasis*\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }
