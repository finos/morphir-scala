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

  "fenced code" - {
    "keeps its body as literal text, never inline content" in {
      parse("```\n*not emphasis*\n```").blocks.head match
        case Block.FencedCode(_, content, _) => assert(content == "*not emphasis*\n")
        case other                           => assert(false, s"expected fenced code, got $other")
    }
  }
