package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.internal.Parser

/**
 * Every expectation here is the `html` field of a real CommonMark 0.31.2 example, named by its number, copied
 * byte-for-byte. None is hand-written: if an expectation and the spec disagree, the expectation is the bug.
 */
class ScalatagsCompilerTests extends Test[Any]:

  private val meta = MdMeta.at(Span.zero)

  private def prose(value: String): Chunk[MdNode.PhrasingContent] = Chunk(MdNode.Text(value, meta))

  /** A one-paragraph item, which is what a tight list's items are. */
  private def item(value: String): MdNode.ListItem =
    MdNode.ListItem(Chunk(MdNode.Paragraph(prose(value), meta)), meta = meta)

  private def render(blocks: MdNode.FlowContent*): String =
    ScalatagsCompiler.render(MdNode.Root(Chunk.from(blocks), meta = meta))

  "ScalatagsCompiler" - {

    "renders an ATX heading (spec example 67)" in
      assert(render(MdNode.Heading(HeadingLevel.One, prose("foo"), meta)) == "<h1>foo</h1>\n")

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
        assert(render(MdNode.Heading(level, prose("x"), meta)) == expected)
      }
    }

    "renders a paragraph (spec example 645)" in
      assert(render(MdNode.Paragraph(prose("foo"), meta)) == "<p>foo</p>\n")

    "spells a thematic break the way the fixtures do (spec example 11)" in
      assert(render(MdNode.ThematicBreak(meta)) == "<hr />\n")

    "escapes text the way the spec does, leaving the apostrophe literal (spec example 12)" in {
      val text     = """!"#$%&'()*+,-./:;<=>?@[\]^_`{|}~"""
      val expected = """<p>!&quot;#$%&amp;'()*+,-./:;&lt;=&gt;?@[\]^_`{|}~</p>""" + "\n"
      assert(render(MdNode.Paragraph(prose(text), meta)) == expected)
    }

    "puts the language in a class on the inner code element (spec example 142)" in
      assert(
        render(MdNode.Code(FenceInfo.parse("ruby"), "def foo(x)\n  return 3\nend\n", meta)) ==
          "<pre><code class=\"language-ruby\">def foo(x)\n  return 3\nend\n</code></pre>\n"
      )

    "omits the class when the fence names no language, and escapes the code (spec example 119)" in
      assert(
        render(MdNode.Code(FenceInfo.empty, "<\n >\n", meta)) ==
          "<pre><code>&lt;\n &gt;\n</code></pre>\n"
      )

    "adds no newline inside an empty code block (spec example 130)" in
      assert(render(MdNode.Code(FenceInfo.empty, "", meta)) == "<pre><code></code></pre>\n")

    "renders a bullet list with one item per line (spec example 281)" in
      assert(
        render(MdNode.List(
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
          MdNode.List(ordered = false, Absent, spread = false, Chunk(item("foo")), meta),
          MdNode.ThematicBreak(meta),
          MdNode.List(ordered = false, Absent, spread = false, Chunk(item("bar")), meta)
        ) == "<ul>\n<li>foo</li>\n</ul>\n<hr />\n<ul>\n<li>bar</li>\n</ul>\n"
      )

    "renders a table with a header and a body (spec example 198)" in {
      given MdProfile = MdProfile.gfm
      val rendered    =
        ScalatagsCompiler.render(Parser.parse("| foo | bar |\n| --- | --- |\n| baz | bim |\n").getOrThrow)
      assert(rendered ==
        "<table>\n<thead>\n<tr>\n<th>foo</th>\n<th>bar</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n" +
        "<td>baz</td>\n<td>bim</td>\n</tr>\n</tbody>\n</table>\n")
    }

    "puts alignment in an attribute, not a style (spec example 199)" in {
      given MdProfile = MdProfile.gfm
      val rendered    =
        ScalatagsCompiler.render(Parser.parse("| abc | defghi |\n:-: | -----------:\nbar | baz\n").getOrThrow)
      assert(rendered.contains("""<th align="center">abc</th>"""))
      assert(rendered.contains("""<td align="right">baz</td>"""))
      assert(!rendered.contains("text-align"))
    }

    "omits tbody entirely when there are no body rows (spec example 205)" in {
      given MdProfile = MdProfile.gfm
      val rendered    = ScalatagsCompiler.render(Parser.parse("| abc | def |\n| --- | --- |\n").getOrThrow)
      assert(rendered == "<table>\n<thead>\n<tr>\n<th>abc</th>\n<th>def</th>\n</tr>\n</thead>\n</table>\n")
      assert(!rendered.contains("tbody"))
    }

    "renders an empty document as the empty string" in
      assert(render() == "")
  }
