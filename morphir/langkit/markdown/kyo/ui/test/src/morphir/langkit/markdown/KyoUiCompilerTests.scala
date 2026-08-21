package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

/**
 * These assert on the shape of the `kyo.UI` tree and on the tags that reach the page — never on whole rendered
 * documents byte for byte.
 *
 * That is deliberate. kyo-ui stamps a `data-kyo-path` attribute onto every element it emits, so a byte-exact
 * expectation here would be pinning a reactive framework's bookkeeping rather than our node mapping. The ScalaTags
 * writer carries the byte-exact duty, and the shared algebra is what keeps the two mappings from drifting.
 */
class KyoUiCompilerTests extends Test[Any]:

  private val span = Present(Span.zero)

  private def prose(value: String): Chunk[MdcNode.PhrasingContent] = Chunk(MdcNode.Text(value, span))

  /** A one-paragraph item, which is what a tight list's items are. */
  private def item(value: String): MdcNode.ListItem =
    MdcNode.ListItem(Chunk(MdcNode.Paragraph(prose(value), span)), span)

  private def compile(blocks: MdcNode.FlowContent*): UI =
    KyoUiCompiler.compile(MdcNode.Root(Chunk.from(blocks), span))

  /** The first emission of a static render: kyo-ui only emits again when a signal changes. */
  private def render(blocks: MdcNode.FlowContent*): String < Async =
    UI.runRender(compile(blocks*)).take(1).run.map(_.head)

  private def children(ui: UI): Chunk[UI] = ui match
    case UI.Ast.Fragment(cs) => cs
    case other               => Chunk(other)

  "KyoUiCompiler node mapping" - {

    "compiles a heading to the kyo-ui element for its level" in {
      assert(children(compile(MdcNode.Heading(HeadingLevel.One, prose("Title"), span))).head.isInstanceOf[UI.Ast.H1])
      assert(children(compile(MdcNode.Heading(HeadingLevel.Two, prose("Title"), span))).head.isInstanceOf[UI.Ast.H2])
      assert(children(compile(MdcNode.Heading(HeadingLevel.Six, prose("Title"), span))).head.isInstanceOf[UI.Ast.H6])
    }

    "compiles a paragraph to a P" in
      assert(children(compile(MdcNode.Paragraph(prose("Body"), span))).head.isInstanceOf[UI.Ast.P])

    "compiles a thematic break to an Hr" in
      assert(children(compile(MdcNode.ThematicBreak(span))).head.isInstanceOf[UI.Ast.Hr])

    "compiles a fenced code block to a Pre wrapping a Code" in {
      val pre = children(compile(MdcNode.Code(FenceInfo.empty, "x", span))).head
      assert(pre.isInstanceOf[UI.Ast.Pre])
      assert(pre.asInstanceOf[UI.Ast.Pre].children.flatMap(children).head.isInstanceOf[UI.Ast.Code])
    }

    "compiles a bullet list to a Ul holding one Li per item" in {
      val list = children(
        compile(MdcNode.List(ordered = false, Absent, spread = false, Chunk(item("one"), item("two")), span))
      ).head
      assert(list.isInstanceOf[UI.Ast.Ul])
      // Children arrive wrapped in a Fragment, which renders with no element of its own; flatten it away.
      val items = list.asInstanceOf[UI.Ast.Ul].children.flatMap(children)
      assert(items.size == 2)
      assert(items.forall(_.isInstanceOf[UI.Ast.Li]))
    }

    "keeps every top-level block, in order" in {
      val compiled = children(
        compile(
          MdcNode.Heading(HeadingLevel.One, prose("T"), span),
          MdcNode.Paragraph(prose("B"), span),
          MdcNode.ThematicBreak(span)
        )
      )
      assert(compiled.size == 3)
      assert(compiled(0).isInstanceOf[UI.Ast.H1])
      assert(compiled(1).isInstanceOf[UI.Ast.P])
      assert(compiled(2).isInstanceOf[UI.Ast.Hr])
    }
  }

  "KyoUiCompiler rendering" - {

    "emits the heading tag and its text" in
      render(MdcNode.Heading(HeadingLevel.Two, prose("Title"), span)).map { html =>
        assert(html.startsWith("<h2"))
        assert(html.contains("Title"))
        assert(html.endsWith("</h2>"))
      }

    "escapes text rather than emitting it raw" in
      render(MdcNode.Paragraph(prose("a < b & c"), span)).map { html =>
        assert(html.contains("&lt;"))
        assert(html.contains("&amp;"))
        assert(!html.contains("a < b"))
      }

    "puts the fence language in a class on the code element" in
      render(MdcNode.Code(FenceInfo.parse("scala"), "x", span)).map { html =>
        assert(html.contains("language-scala"))
        assert(html.contains("<pre"))
        assert(html.contains("<code"))
      }

    "renders an empty document without raising" in
      render().map(html => assert(html.isEmpty))
  }
