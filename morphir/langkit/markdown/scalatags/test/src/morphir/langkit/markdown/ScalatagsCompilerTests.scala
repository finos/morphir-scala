package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

/**
 * Every expectation here is the `html` field of a real CommonMark 0.31.2 example, named by its number, copied
 * byte-for-byte. None is hand-written: if an expectation and the spec disagree, the expectation is the bug.
 */
class ScalatagsCompilerTests extends Test[Any]:

  private val meta = MdcMeta.at(Span.zero)

  private def prose(value: String): Chunk[MdcNode.PhrasingContent] = Chunk(MdcNode.Text(value, meta))

  /** A one-paragraph item, which is what a tight list's items are. */
  private def item(value: String): MdcNode.ListItem =
    MdcNode.ListItem(Chunk(MdcNode.Paragraph(prose(value), meta)), meta)

  private def render(blocks: MdcNode.FlowContent*): String =
    ScalatagsCompiler.render(MdcNode.Root(Chunk.from(blocks), meta = meta))

  "ScalatagsCompiler" - {

    "renders an ATX heading (spec example 67)" in
      assert(render(MdcNode.Heading(HeadingLevel.One, prose("foo"), meta)) == "<h1>foo</h1>\n")

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
        assert(render(MdcNode.Heading(level, prose("x"), meta)) == expected)
      }
    }

    "renders a paragraph (spec example 645)" in
      assert(render(MdcNode.Paragraph(prose("foo"), meta)) == "<p>foo</p>\n")

    "spells a thematic break the way the fixtures do (spec example 11)" in
      assert(render(MdcNode.ThematicBreak(meta)) == "<hr />\n")

    "escapes text the way the spec does, leaving the apostrophe literal (spec example 12)" in {
      val text     = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
      val expected = """<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>""" + "\n"
      assert(render(MdcNode.Paragraph(prose(text), meta)) == expected)
    }

    "puts the language in a class on the inner code element (spec example 142)" in
      assert(
        render(MdcNode.Code(FenceInfo.parse("ruby"), "def foo(x)\n  return 3\nend\n", meta)) ==
          "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      )

    "omits the class when the fence names no language, and escapes the code (spec example 119)" in
      assert(
        render(MdcNode.Code(FenceInfo.empty, "<\n >\n", meta)) ==
          "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      )

    "adds no newline inside an empty code block (spec example 130)" in
      assert(render(MdcNode.Code(FenceInfo.empty, "", meta)) == "<pre><code></code></pre>\n")

    "renders a bullet list with one item per line (spec example 281)" in
      assert(
        render(MdcNode.List(
          ordered = false,
          Absent,
          spread = false,
          Chunk(item("foo"), item(""), item("bar")),
          meta
        )) ==
          "<ul>\n<li>foo</li>\n<li></li>\n<li>bar</li>\n</ul>\n"
      )

    "separates sibling blocks with a newline apiece (spec example 57)" in
      assert(
        render(
          MdcNode.List(ordered = false, Absent, spread = false, Chunk(item("foo")), meta),
          MdcNode.ThematicBreak(meta),
          MdcNode.List(ordered = false, Absent, spread = false, Chunk(item("bar")), meta)
        ) == "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
      )

    "renders an empty document as the empty string" in
      assert(render() == "")
  }
