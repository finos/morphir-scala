package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.MdNode as N
import morphir.langkit.markdown.dsl.*
import morphir.langkit.markdown.dsl.given

class DslTests extends Test[Any]:

  "dsl" - {

    "builds the spec's example tree structurally equal to a hand-built one" in {
      val built = doc(
        h1("Title"),
        p("hello ", strong("world"), " — see ", a("https://x.y")("here")),
        ul(li("one"), li(p("two"), codeBlock("scala", "val x = 1"))),
        quote(p(em("quoted"))),
        hr
      )

      val expected = N.Root(
        Chunk(
          N.Heading(HeadingLevel.One, Chunk(N.Text("Title"))),
          N.Paragraph(
            Chunk(
              N.Text("hello "),
              N.Strong(Chunk(N.Text("world"))),
              N.Text(" — see "),
              N.Link("https://x.y", Absent, Chunk(N.Text("here")))
            )
          ),
          N.List(
            ordered = false,
            start = Absent,
            spread = false,
            Chunk(
              N.ListItem(Chunk(N.Paragraph(Chunk(N.Text("one"))))),
              N.ListItem(Chunk(N.Paragraph(Chunk(N.Text("two"))), N.Code(FenceInfo.parse("scala"), "val x = 1")))
            )
          ),
          N.Blockquote(Chunk(N.Paragraph(Chunk(N.Emphasis(Chunk(N.Text("quoted"))))))),
          N.ThematicBreak()
        )
      )

      assert(built == expected)
    }

    "li wraps maximal phrasing runs in a Paragraph and flushes on flow content" in {
      val item = li("a", "b", p("c"))
      assert(item == N.ListItem(Chunk(N.Paragraph(Chunk(N.Text("a"), N.Text("b"))), N.Paragraph(Chunk(N.Text("c"))))))
    }

    "li with only phrasing content wraps once" in {
      val item = li("solo")
      assert(item == N.ListItem(Chunk(N.Paragraph(Chunk(N.Text("solo"))))))
    }

    "li with only flow content never emits an empty paragraph" in {
      val item = li(p("x"), hr, p("y"))
      assert(item ==
        N.ListItem(Chunk(N.Paragraph(Chunk(N.Text("x"))), N.ThematicBreak(), N.Paragraph(Chunk(N.Text("y"))))))
    }

    "li with no arguments builds an empty item" in
      assert(li().childNodes.isEmpty)

    "string conversion applies inside varargs" in {
      val paragraph: MdNode.Paragraph = p("plain")
      assert(paragraph == N.Paragraph(Chunk(N.Text("plain"))))
    }

    "ul defaults to tight, unordered, no start" in {
      val list = ul(li("a"))
      assert(!list.ordered)
      assert(list.start == Absent)
      assert(list.tight)
    }

    "ul(spread) sets spread" in {
      val list = ul(true)(li("a"))
      assert(!list.tight)
    }

    "ol defaults start to 1" in {
      val list = ol(li("a"))
      assert(list.ordered)
      assert(list.start == Present(1))
      assert(list.tight)
    }

    "ol(start) sets the start and defaults spread to tight" in {
      val list = ol(3)(li("x"))
      assert(list.start == Present(3))
      assert(list.tight)
    }

    "ol(start, spread) sets both" in {
      val list = ol(5, spread = true)(li("x"))
      assert(list.start == Present(5))
      assert(!list.tight)
    }

    "codeBlock without info parses to an empty FenceInfo" in
      assert(codeBlock("val x = 1") == N.Code(FenceInfo.empty, "val x = 1"))

    "codeBlock with info parses the fence info" in
      assert(codeBlock("scala", "val x = 1") == N.Code(FenceInfo.parse("scala"), "val x = 1"))

    "img with and without a title" in {
      assert(img("/u", "alt") == N.Image("/u", Absent, "alt"))
      assert(img("/u", "alt", "t") == N.Image("/u", Present("t"), "alt"))
    }

    "a with and without a title" in {
      assert(a("/u")("text") == N.Link("/u", Absent, Chunk(N.Text("text"))))
      assert(a("/u", "t")("text") == N.Link("/u", Present("t"), Chunk(N.Text("text"))))
    }

    "every DSL node is generated: span is Absent" in {
      val nodes: Chunk[MdNode] = Chunk(
        doc(),
        h1("t"),
        h2("t"),
        h3("t"),
        h4("t"),
        h5("t"),
        h6("t"),
        p("t"),
        codeBlock("v"),
        codeBlock("i", "v"),
        htmlBlock("<hr>"),
        quote(p("t")),
        ul(li("a")),
        ol(li("a")),
        ol(3)(li("a")),
        li("a"),
        hr,
        text("t"),
        code("c"),
        a("/u")("t"),
        a("/u", "title")("t"),
        img("/u", "alt"),
        img("/u", "alt", "title"),
        em("t"),
        strong("t"),
        inlineHtml("<b>"),
        br
      )
      assert(nodes.forall(_.span == Absent))
    }
  }

  "MdStyle" - {
    "default is available as a given" in {
      val style = summon[MdStyle]
      assert(style == MdStyle())
    }
  }

  "MdStyleKeys" - {
    "bullet round-trips through withMeta/meta.get" in {
      val node = em("x").withMeta(MdStyleKeys.bullet, '+')
      assert(node.meta.get(MdStyleKeys.bullet) == Present('+'))
    }
    "emphasisMarker round-trips" in {
      val node = em("x").withMeta(MdStyleKeys.emphasisMarker, '_')
      assert(node.meta.get(MdStyleKeys.emphasisMarker) == Present('_'))
    }
    "strongMarker round-trips" in {
      val node = strong("x").withMeta(MdStyleKeys.strongMarker, '_')
      assert(node.meta.get(MdStyleKeys.strongMarker) == Present('_'))
    }
    "fence round-trips" in {
      val node = codeBlock("v").withMeta(MdStyleKeys.fence, '~')
      assert(node.meta.get(MdStyleKeys.fence) == Present('~'))
    }
    "orderedDelimiter round-trips" in {
      val node = ol(li("a")).withMeta(MdStyleKeys.orderedDelimiter, ')')
      assert(node.meta.get(MdStyleKeys.orderedDelimiter) == Present(')'))
    }
    "headingStyle round-trips" in {
      val node = h1("t").withMeta(MdStyleKeys.headingStyle, HeadingStyle.Setext)
      assert(node.meta.get(MdStyleKeys.headingStyle) == Present(HeadingStyle.Setext))
    }
    "hardBreak round-trips" in {
      val node = br.withMeta(MdStyleKeys.hardBreak, HardBreakStyle.Spaces)
      assert(node.meta.get(MdStyleKeys.hardBreak) == Present(HardBreakStyle.Spaces))
    }
    "a key absent from a node's meta reads Absent" in
      assert(em("x").meta.get(MdStyleKeys.bullet) == Absent)

    "li alternating phrasing, flow, phrasing keeps each run in its own paragraph" in {
      val item = li("a", p("b"), "c")
      assert(item.children.size == 3)
      assert(item.children(0) == N.Paragraph(Chunk(N.Text("a"))))
      assert(item.children(1) == N.Paragraph(Chunk(N.Text("b"))))
      assert(item.children(2) == N.Paragraph(Chunk(N.Text("c"))))
    }
  }
