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
  private def textOf(content: Chunk[Inline]): String =
    content.map {
      case Inline.Text(value, _)           => value
      case Inline.CodeSpan(value, _)       => value
      case Inline.Link(_, _, inner, _)     => textOf(inner)
      case Inline.Image(_, _, alt, _)      => alt
      case Inline.Emphasis(inner, _)       => textOf(inner)
      case Inline.StrongEmphasis(inner, _) => textOf(inner)
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
    "gives every item its own inline content and span" in {
      parse("- one\n- two").blocks.head match
        case Block.UnorderedList(items, _) =>
          assert(items.size == 2)
          assert(items(0).content == Chunk(Inline.Text("one", Span(2, 3))))
          assert(items(1).content == Chunk(Inline.Text("two", Span(8, 3))))
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
    "does not interrupt a paragraph (spec example 113)" in {
      parse("Foo\n    bar").blocks.head match
        case Block.Paragraph(content, _) => assert(textOf(content) == "Foo\n    bar")
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
        case Block.OrderedList(start, items, _) =>
          assert(start == 1)
          assert(items.map(item => textOf(item.content)) == Chunk("one", "two"))
        case other => assert(false, s"expected an ordered list, got $other")
    }
    "keeps the first marker's number as the start" in {
      parse("3. three\n4. four").blocks.head match
        case Block.OrderedList(start, items, _) =>
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

  "fenced code" - {
    "keeps its body as literal text, never inline content" in {
      parse("```\n*not emphasis*\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "*not emphasis*\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }
