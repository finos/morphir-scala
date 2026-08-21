package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

class MdcNodeTests extends Test[Any]:

  private val meta                          = MdcMeta.at(Span(3, 4))
  private def text(s: String): MdcNode.Text = MdcNode.Text(s, meta)

  "MdcNode" - {

    "children is total: parents expose them, leaves are empty" in {
      val para = MdcNode.Paragraph(Chunk(text("a")), meta)
      val root = MdcNode.Root(Chunk(para), meta = meta)
      assert(root.childNodes == Chunk(para))
      assert(para.childNodes == Chunk(text("a")))
      assert(text("a").childNodes.isEmpty)
      assert(MdcNode.ThematicBreak(meta).childNodes.isEmpty)
      assert(MdcNode.Break(meta).childNodes.isEmpty)
      assert(MdcNode.Image("/u", Absent, "alt", meta).childNodes.isEmpty)
    }

    "literal is Present exactly on the five literals" in {
      assert(text("a").literal == Present("a"))
      assert(MdcNode.InlineCode("c", meta).literal == Present("c"))
      assert(MdcNode.Code(FenceInfo.empty, "b", meta).literal == Present("b"))
      assert(MdcNode.Html("<hr>", meta).literal == Present("<hr>"))
      assert(MdcNode.InlineHtml("<b>", meta).literal == Present("<b>"))
      assert(MdcNode.Paragraph(Chunk.empty, meta).literal == Absent)
      assert(MdcNode.Break(meta).literal == Absent)
    }

    "tight is the negation of spread" in {
      val tightList =
        MdcNode.List(ordered = false, start = Absent, spread = false, Chunk.empty, meta)
      val looseList =
        MdcNode.List(ordered = true, start = Present(3), spread = true, Chunk.empty, meta)
      assert(tightList.tight)
      assert(!looseList.tight)
    }

    "span is derived from meta" in {
      assert(text("a").span == Present(Span(3, 4)))
      assert(MdcNode.Text("a").span == Absent)
    }

    "construction without a span is first-class: generated nodes carry Absent" in {
      val generated = MdcNode.Paragraph(Chunk(MdcNode.Text("hi")))
      assert(generated.span == Absent)
      assert(generated.childNodes.head.span == Absent)
    }

    "unpositioned strips every span recursively" in {
      val parsedish = MdcNode.Root(Chunk(MdcNode.Paragraph(Chunk(text("a")), meta)), meta = meta)
      def spans(node: MdcNode): Chunk[Maybe[Span]] =
        node.span +: node.childNodes.flatMap(spans)
      assert(spans(parsedish).forall(_.isDefined))
      assert(spans(parsedish.unpositioned).forall(_ == Absent))
    }

    "withMeta attaches typed data and unpositioned keeps it" in {
      val key  = MetaKey[String]("origin")
      val node = MdcNode.Paragraph(Chunk(MdcNode.Text("hi")), MdcMeta.at(Span(0, 2))).withMeta(key, "dsl")
      assert(node.meta.get(key) == Present("dsl"))
      assert(node.span == Present(Span(0, 2)))
      val stripped = node.unpositioned
      assert(stripped.span == Absent)
      assert(stripped.meta.get(key) == Present("dsl"))
    }

    "withMeta keeps the node at its own case, so it stays in its content category" in {
      // Compile-time proof: `withMeta` returning MdcNode rather than Text would fail this annotation.
      val annotated: MdcNode.Text                  = MdcNode.Text("hi").withMeta(MetaKey[Int]("weight"), 1)
      val phrasing: Chunk[MdcNode.PhrasingContent] = Chunk(annotated)
      assert(phrasing.size == 1 && annotated.value == "hi")
    }

    "content categories admit what they claim" in {
      // Compile-time proof: these annotations fail to compile if the aliases drift.
      val flow: Chunk[MdcNode.FlowContent]         = Chunk(MdcNode.Paragraph(Chunk.empty, meta))
      val phrasing: Chunk[MdcNode.PhrasingContent] = Chunk(text("x"), MdcNode.Break(meta))
      assert(flow.size == 1 && phrasing.size == 2)
    }

    "a lowered tree is fully positioned" in {
      val root = Parser.parse("# T\n\n- a `c`\n\n> q\n") match
        case Result.Success(document) => document
        case other                    => throw new IllegalStateException(s"parse failed: $other")
      def spans(node: MdcNode): Chunk[Maybe[Span]] = node.span +: node.childNodes.flatMap(spans)
      assert(spans(root).forall(_.isDefined))
    }

    "frontmatter is a Root field, traversed first, literal-bearing" in {
      val yaml = MdcNode.FrontMatter.Yaml(YamlDocText("title: x\n"))
      val root = MdcNode.Root(Chunk(MdcNode.Paragraph(Chunk(MdcNode.Text("hi")))), Present(yaml))
      assert(root.frontmatter == Present(yaml))
      assert(root.childNodes.head == yaml)
      assert(yaml.literal == Present("title: x\n"))
      assert(yaml.span == Absent)
      val stripped = root.unpositioned
      assert(stripped.asInstanceOf[MdcNode.Root].frontmatter.isDefined)
    }
  }
