package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

class MdcNodeTests extends Test[Any]:

  private val span                          = Present(Span(3, 4))
  private def text(s: String): MdcNode.Text = MdcNode.Text(s, span)

  "MdcNode" - {

    "children is total: parents expose them, leaves are empty" in {
      val para = MdcNode.Paragraph(Chunk(text("a")), span)
      val root = MdcNode.Root(Chunk(para), span)
      assert(root.childNodes == Chunk(para))
      assert(para.childNodes == Chunk(text("a")))
      assert(text("a").childNodes.isEmpty)
      assert(MdcNode.ThematicBreak(span).childNodes.isEmpty)
      assert(MdcNode.Break(span).childNodes.isEmpty)
      assert(MdcNode.Image("/u", Absent, "alt", span).childNodes.isEmpty)
    }

    "value is Present exactly on the five literals" in {
      assert(text("a").literal == Present("a"))
      assert(MdcNode.InlineCode("c", span).literal == Present("c"))
      assert(MdcNode.Code(FenceInfo.empty, "b", span).literal == Present("b"))
      assert(MdcNode.Html("<hr>", span).literal == Present("<hr>"))
      assert(MdcNode.InlineHtml("<b>", span).literal == Present("<b>"))
      assert(MdcNode.Paragraph(Chunk.empty, span).literal == Absent)
      assert(MdcNode.Break(span).literal == Absent)
    }

    "tight is the negation of spread" in {
      val tightList =
        MdcNode.List(ordered = false, start = Absent, spread = false, Chunk.empty, span)
      val looseList =
        MdcNode.List(ordered = true, start = Present(3), spread = true, Chunk.empty, span)
      assert(tightList.tight)
      assert(!looseList.tight)
    }

    "construction without a span is first-class: generated nodes carry Absent" in {
      val generated = MdcNode.Paragraph(Chunk(MdcNode.Text("hi")))
      assert(generated.span == Absent)
      assert(generated.childNodes.head.span == Absent)
    }

    "unpositioned strips every span recursively" in {
      val parsedish = MdcNode.Root(Chunk(MdcNode.Paragraph(Chunk(text("a")), span)), span)
      def spans(node: MdcNode): Chunk[Maybe[Span]] =
        node.span +: node.childNodes.flatMap(spans)
      assert(spans(parsedish).forall(_.isDefined))
      assert(spans(parsedish.unpositioned).forall(_ == Absent))
    }

    "content categories admit what they claim" in {
      // Compile-time proof: these annotations fail to compile if the aliases drift.
      val flow: Chunk[MdcNode.FlowContent]         = Chunk(MdcNode.Paragraph(Chunk.empty, span))
      val phrasing: Chunk[MdcNode.PhrasingContent] = Chunk(text("x"), MdcNode.Break(span))
      assert(flow.size == 1 && phrasing.size == 2)
    }

    "a lowered tree is fully positioned" in {
      val root = Parser.parse("# T\n\n- a `c`\n\n> q\n") match
        case Result.Success(document) => document
        case other                    => throw new IllegalStateException(s"parse failed: $other")
      def spans(node: MdcNode): Chunk[Maybe[Span]] = node.span +: node.childNodes.flatMap(spans)
      assert(spans(root).forall(_.isDefined))
    }
  }
