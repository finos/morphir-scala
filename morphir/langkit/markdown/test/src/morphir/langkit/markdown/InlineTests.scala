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

  "fenced code" - {
    "keeps its body as literal text, never inline content" in {
      parse("```\n*not emphasis*\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "*not emphasis*\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }
