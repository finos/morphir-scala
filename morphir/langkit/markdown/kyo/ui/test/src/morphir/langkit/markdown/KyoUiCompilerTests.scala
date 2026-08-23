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

  private val meta = MdMeta.at(Span.zero)

  private def prose(value: String): Chunk[MdNode.PhrasingContent] = Chunk(MdNode.Text(value, meta))

  private def cell(value: String): MdNode.TableCell = MdNode.TableCell(prose(value), meta)

  /** A one-paragraph item, which is what a tight list's items are. */
  private def item(value: String): MdNode.ListItem =
    MdNode.ListItem(Chunk(MdNode.Paragraph(prose(value), meta)), meta = meta)

  private def compile(blocks: MdNode.FlowContent*): UI =
    KyoUiCompiler.compile(MdNode.Root(Chunk.from(blocks), meta = meta))

  /** The first emission of a static render: kyo-ui only emits again when a signal changes. */
  private def render(blocks: MdNode.FlowContent*): String < Async =
    UI.runRender(compile(blocks*)).take(1).run.map(_.head)

  private def children(ui: UI): Chunk[UI] = ui match
    case UI.Ast.Fragment(cs) => cs
    case other               => Chunk(other)

  "KyoUiCompiler node mapping" - {

    "compiles a heading to the kyo-ui element for its level" in {
      assert(children(compile(MdNode.Heading(HeadingLevel.One, prose("Title"), meta))).head.isInstanceOf[UI.Ast.H1])
      assert(children(compile(MdNode.Heading(HeadingLevel.Two, prose("Title"), meta))).head.isInstanceOf[UI.Ast.H2])
      assert(children(compile(MdNode.Heading(HeadingLevel.Six, prose("Title"), meta))).head.isInstanceOf[UI.Ast.H6])
    }

    "compiles a paragraph to a P" in
      assert(children(compile(MdNode.Paragraph(prose("Body"), meta))).head.isInstanceOf[UI.Ast.P])

    "compiles a thematic break to an Hr" in
      assert(children(compile(MdNode.ThematicBreak(meta))).head.isInstanceOf[UI.Ast.Hr])

    "compiles a fenced code block to a Pre wrapping a Code" in {
      val pre = children(compile(MdNode.Code(FenceInfo.empty, "x", meta))).head
      assert(pre.isInstanceOf[UI.Ast.Pre])
      assert(pre.asInstanceOf[UI.Ast.Pre].children.flatMap(children).head.isInstanceOf[UI.Ast.Code])
    }

    "compiles a bullet list to a Ul holding one Li per item" in {
      val list = children(
        compile(MdNode.List(ordered = false, Absent, spread = false, Chunk(item("one"), item("two")), meta))
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
          MdNode.Heading(HeadingLevel.One, prose("T"), meta),
          MdNode.Paragraph(prose("B"), meta),
          MdNode.ThematicBreak(meta)
        )
      )
      assert(compiled.size == 3)
      assert(compiled(0).isInstanceOf[UI.Ast.H1])
      assert(compiled(1).isInstanceOf[UI.Ast.P])
      assert(compiled(2).isInstanceOf[UI.Ast.Hr])
    }

    "compiles a Delete node to a span carrying the md-del class" in {
      val paragraph = children(compile(MdNode.Paragraph(Chunk(MdNode.Delete(prose("gone"), meta)), meta))).head
      val span      = paragraph.asInstanceOf[UI.Ast.P].children.flatMap(children).head
      assert(span.isInstanceOf[UI.Ast.SpanElement])
      assert(span.asInstanceOf[UI.Ast.SpanElement].attrs.cssClasses.contains("md-del"))
    }

    "prepends the checked-box markup to a ticked task-list item" in {
      val taskItem = MdNode.ListItem(Chunk(MdNode.Paragraph(prose("done"), meta)), checked = Present(true), meta = meta)
      val li       = children(
        compile(MdNode.List(ordered = false, Absent, spread = false, Chunk(taskItem), meta))
      ).head.asInstanceOf[UI.Ast.Ul].children.flatMap(children).head
      val box = li.asInstanceOf[UI.Ast.Li].children.flatMap(children).head
      assert(box.asInstanceOf[UI.Ast.RawHtml].value == """<input checked="" disabled="" type="checkbox">""")
    }

    "prepends the unchecked-box markup, with no `checked` attribute, to an unticked task-list item" in {
      val taskItem =
        MdNode.ListItem(Chunk(MdNode.Paragraph(prose("todo"), meta)), checked = Present(false), meta = meta)
      val li = children(
        compile(MdNode.List(ordered = false, Absent, spread = false, Chunk(taskItem), meta))
      ).head.asInstanceOf[UI.Ast.Ul].children.flatMap(children).head
      val box   = li.asInstanceOf[UI.Ast.Li].children.flatMap(children).head
      val value = box.asInstanceOf[UI.Ast.RawHtml].value
      assert(value == """<input disabled="" type="checkbox">""")
      assert(!value.contains("checked"))
    }
  }

  "KyoUiCompiler rendering" - {

    "emits the heading tag and its text" in
      render(MdNode.Heading(HeadingLevel.Two, prose("Title"), meta)).map { html =>
        assert(html.startsWith("<h2"))
        assert(html.contains("Title"))
        assert(html.endsWith("</h2>"))
      }

    "escapes text rather than emitting it raw" in
      render(MdNode.Paragraph(prose("a < b & c"), meta)).map { html =>
        assert(html.contains("&lt;"))
        assert(html.contains("&amp;"))
        assert(!html.contains("a < b"))
      }

    "puts the fence language in a class on the code element" in
      render(MdNode.Code(FenceInfo.parse("scala"), "x", meta)).map { html =>
        assert(html.contains("language-scala"))
        assert(html.contains("<pre"))
        assert(html.contains("<code"))
      }

    /**
     * kyo-ui has no `thead`, so the header row goes in a `tbody` marked `md-thead`, and alignment is a class rather
     * than the `align` attribute the ScalaTags oracle writes. Both are recorded gaps, not accidents.
     */
    "renders a table as th and td cells, with alignment as a class" in
      render(MdNode.Table(
        Chunk(Present(ColumnAlignment.Center), Absent),
        MdNode.TableRow(Chunk(cell("h1"), cell("h2")), meta),
        Chunk(MdNode.TableRow(Chunk(cell("a"), cell("b")), meta)),
        meta
      )).map { html =>
        assert(html.contains("<table"))
        assert(html.contains("<th"))
        assert(html.contains("<td"))
        assert(html.contains("md-thead"))
        assert(html.contains("md-align-center"))
        // The header row group is a real `tbody`, not a bare `tr` under the table: kyo-ui's own guidance is that a
        // row patched into a live table outside a group breaks `tbody`-scoped selectors and semantics.
        assert(html.contains("<tbody"))
        assert(!html.contains("<thead"))
      }

    /** One row group, the header's; a table with no body rows gets no second one. */
    "gives a table with no body rows only its header row group" in
      render(MdNode.Table(
        Chunk(Absent),
        MdNode.TableRow(Chunk(cell("only")), meta),
        Chunk.empty,
        meta
      )).map { html =>
        assert(html.split("<tbody", -1).length - 1 == 1, html)
        assert(html.contains("md-thead"))
      }

    "renders an empty document without raising" in
      render().map(html => assert(html.isEmpty))
  }
