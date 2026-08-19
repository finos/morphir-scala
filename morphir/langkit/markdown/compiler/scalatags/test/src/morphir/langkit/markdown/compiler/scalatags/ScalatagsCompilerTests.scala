package morphir.langkit.markdown.compiler.scalatags

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.*

/**
 * Every expectation here is the `html` field of a real CommonMark 0.31.2 example, named by its number, copied
 * byte-for-byte. None is hand-written: if an expectation and the spec disagree, the expectation is the bug.
 *
 * The AST reaches only five block kinds today, so these cover the whole algebra. Inline nodes, and the examples that
 * exercise them, arrive with intent 0021.
 */
class ScalatagsCompilerTests extends Test[Any]:

  private val span = Span.zero

  private def render(blocks: Block*): String =
    ScalatagsCompiler.render(Document(Chunk.from(blocks), span))

  "ScalatagsCompiler" - {

    "renders an ATX heading (spec example 67)" in
      assert(render(Block.Heading(HeadingLevel.One, "foo", span)) == "<h1>foo</h1>\n")

    "renders every heading level" in {
      val levels = Chunk(
        HeadingLevel.One   -> "<h1>x</h1>\n",
        HeadingLevel.Two   -> "<h2>x</h2>\n",
        HeadingLevel.Three -> "<h3>x</h3>\n",
        HeadingLevel.Four  -> "<h4>x</h4>\n",
        HeadingLevel.Five  -> "<h5>x</h5>\n",
        HeadingLevel.Six   -> "<h6>x</h6>\n"
      )
      levels.foreach { case (level, expected) =>
        assert(render(Block.Heading(level, "x", span)) == expected)
      }
    }

    "renders a paragraph (spec example 645)" in
      assert(render(Block.Paragraph("foo", span)) == "<p>foo</p>\n")

    "spells a thematic break the way the fixtures do (spec example 11)" in
      assert(render(Block.ThematicBreak(span)) == "<hr />\n")

    "escapes text the way the spec does, leaving the apostrophe literal (spec example 12)" in {
      val text     = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
      val expected = """<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>""" + "\n"
      assert(render(Block.Paragraph(text, span)) == expected)
    }

    "puts the language in a class on the inner code element (spec example 142)" in
      assert(
        render(Block.FencedCode(FenceInfo.parse("ruby"), "def foo(x)\n  return 3\nend\n", span)) ==
          "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      )

    "omits the class when the fence names no language, and escapes the code (spec example 119)" in
      assert(
        render(Block.FencedCode(FenceInfo.empty, "<\n >\n", span)) ==
          "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      )

    "adds no newline inside an empty code block (spec example 130)" in
      assert(render(Block.FencedCode(FenceInfo.empty, "", span)) == "<pre><code></code></pre>\n")

    "renders a bullet list with one item per line (spec example 281)" in
      assert(
        render(Block.UnorderedList(Chunk("foo", "", "bar"), span)) ==
          "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
      )

    "separates sibling blocks with a newline apiece (spec example 57)" in
      assert(
        render(
          Block.UnorderedList(Chunk("foo"), span),
          Block.ThematicBreak(span),
          Block.UnorderedList(Chunk("bar"), span)
        ) == "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
      )

    "renders an empty document as the empty string" in
      assert(render() == "")
  }
