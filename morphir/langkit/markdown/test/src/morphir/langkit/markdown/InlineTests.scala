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
      assert(inlines("<not a link>").forall { case _: Inline.Link => false; case _ => true })
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

  "fenced code" - {
    "keeps its body as literal text, never inline content" in {
      parse("```\n*not emphasis*\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "*not emphasis*\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }
