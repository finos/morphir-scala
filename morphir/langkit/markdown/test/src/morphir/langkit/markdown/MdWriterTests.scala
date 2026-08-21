package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.cst.Cst
import morphir.langkit.markdown.dsl.*
import morphir.langkit.markdown.dsl.given

/**
 * The writer's contract is structural fidelity, not byte fidelity: `Parser.parse(MdWriter.write(tree))` succeeds and
 * yields the tree it was given, compared after normalization.
 *
 * Normalization drops the two differences a parse legitimately introduces. Positions are provenance — a generated node
 * honestly has none, a parsed one does — so they come off. And a parse splits prose at every escape and every entity,
 * so `\*a` arrives as two [[MdcNode.Text]] nodes where the author wrote one; adjacent text merges back into one node
 * before the comparison. Nothing else is forgiven: a broken parse or a changed meaning is a failure, while
 * over-escaping, which changes only the bytes, is not.
 */
class MdWriterTests extends Test[Any]:

  /** Newlines shown as `\n` so a mismatch prints on one line and a missing trailing newline is visible. */
  private def oneLine(text: String): String = text.replace("\n", "\\n")

  private def mergedTexts(nodes: Chunk[MdcNode]): Chunk[MdcNode] =
    val out = scala.collection.mutable.ListBuffer.empty[MdcNode]
    nodes.foreach { node =>
      (out.lastOption, normalize(node)) match
        case (Some(MdcNode.Text(before, meta)), MdcNode.Text(after, _)) =>
          out.remove(out.size - 1)
          out += MdcNode.Text(before + after, meta)
        case (_, normalized) => out += normalized
    }
    Chunk.from(out.toList)

  private def flowOf(nodes: Chunk[MdcNode.FlowContent]): Chunk[MdcNode.FlowContent] =
    mergedTexts(nodes).map(_.asInstanceOf[MdcNode.FlowContent])

  private def phrasingOf(nodes: Chunk[MdcNode.PhrasingContent]): Chunk[MdcNode.PhrasingContent] =
    mergedTexts(nodes).map(_.asInstanceOf[MdcNode.PhrasingContent])

  private def normalize(node: MdcNode): MdcNode = node match
    case MdcNode.Root(children, meta)                         => MdcNode.Root(flowOf(children), meta)
    case MdcNode.Paragraph(children, meta)                    => MdcNode.Paragraph(phrasingOf(children), meta)
    case MdcNode.Heading(depth, children, meta)               => MdcNode.Heading(depth, phrasingOf(children), meta)
    case MdcNode.Blockquote(children, meta)                   => MdcNode.Blockquote(flowOf(children), meta)
    case MdcNode.List(ordered, start, spread, children, meta) =>
      MdcNode.List(ordered, start, spread, children.map(item => normalize(item).asInstanceOf[MdcNode.ListItem]), meta)
    case MdcNode.ListItem(children, meta)         => MdcNode.ListItem(flowOf(children), meta)
    case MdcNode.Link(url, title, children, meta) => MdcNode.Link(url, title, phrasingOf(children), meta)
    case MdcNode.Emphasis(children, meta)         => MdcNode.Emphasis(phrasingOf(children), meta)
    case MdcNode.Strong(children, meta)           => MdcNode.Strong(phrasingOf(children), meta)
    case leaf                                     => leaf

  private def normalized(root: MdcNode.Root): MdcNode = normalize(root.unpositioned)

  private def roundTrips(tree: MdcNode.Root, label: String)(using MdStyle)(using AssertScope): Unit =
    val written = MdWriter.write(tree)
    Parser.parse(written) match
      case Result.Success(reparsed) =>
        assert(
          normalized(reparsed) == normalized(tree),
          s"$label did not round-trip through the writer." +
            s"\n  written  ${oneLine(written)}" +
            s"\n  wrote    ${normalized(tree)}" +
            s"\n  reparsed ${normalized(reparsed)}"
        )
      case other =>
        throw new IllegalStateException(s"$label: write produced unparseable text ${oneLine(written)}: $other")

  /** Every document the writer produces is a document the CST parser tiles and reprints exactly. */
  private def raises(tree: MdcNode.Root, label: String)(using MdStyle)(using AssertScope): Unit =
    val written  = MdWriter.write(tree)
    val document = MdWriter.raise(tree)
    val errors   = Cst.tilingErrors(document, written.length)
    assert(errors.isEmpty, s"$label raised a CST that does not tile: ${errors.mkString("; ")}")
    assert(Cst.print(document) == written, s"$label raised a CST that does not reprint what was written")

  private val example: MdcNode.Root =
    doc(
      h1("Title"),
      p("hello ", strong("world"), " — see ", a("https://x.y")("here")),
      ul(li("one"), li(p("two"), codeBlock("scala", "val x = 1\n"))),
      quote(p(em("quoted"))),
      hr
    )

  "MdWriter.write" - {

    "an empty document writes nothing" in
      assert(MdWriter.write(doc()) == "")

    "a document ends with exactly one newline" in {
      val written = MdWriter.write(doc(p("one"), p("two")))
      assert(written == "one\n\ntwo\n", oneLine(written))
    }

    "blocks are separated by one blank line" in
      assert(MdWriter.write(doc(h1("a"), p("b"), hr)) == "# a\n\nb\n\n***\n")

    /**
     * A destination the bare form cannot hold goes in angle brackets. This is asserted on the text rather than round-
     * tripped because lowering percent-encodes what it reads — the AST's `url` is a normalized URI, so a space in one
     * is a URL no parse ever produces and no writer can hand back unchanged.
     */
    "a destination holding a space is written in angle brackets" in
      assert(MdWriter.write(doc(p(a("/a b")("x")))) == "[x](</a b>)\n")

    "an empty destination is written in angle brackets" in
      assert(MdWriter.write(doc(p(a("")("x")))) == "[x](<>)\n")

    /**
     * A code value with no trailing newline is written with one, because a fence has to close on its own line. The
     * reparse therefore reports the newline the fence needed — a code body's trailing newline is spelling the AST
     * cannot avoid carrying, so trees meant to round-trip write it themselves.
     */
    "a code value gains the newline its closing fence needs" in
      assert(MdWriter.write(doc(codeBlock("scala", "val x = 1"))) == "```scala\nval x = 1\n```\n")
  }

  "constructs round-trip" - {

    "the DSL example document" in roundTrips(example, "the DSL example")

    "an ATX heading at every depth" in
      roundTrips(doc(h1("one"), h2("two"), h3("three"), h4("four"), h5("five"), h6("six")), "headings")

    "an empty heading" in roundTrips(doc(h1()), "empty heading")

    "a paragraph of plain text" in roundTrips(doc(p("just words")), "paragraph")

    "a fenced code block with an info string" in
      roundTrips(doc(codeBlock("scala", "val x = 1\n")), "fenced code with info")

    "a code block with no info string" in roundTrips(doc(codeBlock("plain body\n")), "code without info")

    "a code block whose body holds a fence-length backtick run" in
      roundTrips(doc(codeBlock("md", "```\nnested\n```\n")), "code holding a fence")

    "an empty code block" in roundTrips(doc(codeBlock("")), "empty code")

    "an HTML block" in roundTrips(doc(htmlBlock("<div>\n<p>x</p>\n</div>")), "html block")

    "a blockquote" in roundTrips(doc(quote(p("quoted"), p("twice"))), "blockquote")

    "a tight bullet list" in roundTrips(doc(ul(li("one"), li("two"))), "tight bullet list")

    "a spread bullet list" in roundTrips(doc(ul(true)(li("one"), li("two"))), "spread bullet list")

    "an ordered list numbered from one" in roundTrips(doc(ol(li("one"), li("two"))), "ordered list")

    "an ordered list numbered from an arbitrary start" in
      roundTrips(doc(ol(7)(li("seven"), li("eight"), li("nine"), li("ten"))), "ordered list from seven")

    "a thematic break" in roundTrips(doc(hr), "thematic break")

    "inline code" in roundTrips(doc(p("call ", code("map"), " on it")), "inline code")

    "inline code holding backticks" in roundTrips(doc(p(code("a ` b"))), "inline code with a backtick")

    "inline code starting and ending with a backtick" in
      roundTrips(doc(p(code("`tick`"))), "inline code bounded by backticks")

    "inline code starting and ending with a space" in roundTrips(doc(p(code(" pad "))), "inline code padded")

    "a link" in roundTrips(doc(p(a("https://x.y")("here"))), "link")

    "a link with a title" in roundTrips(doc(p(a("/u", "the title")("here"))), "link with title")

    "a link whose destination holds parentheses" in
      roundTrips(doc(p(a("/a(b)")("here"))), "link with an awkward destination")

    "an image" in roundTrips(doc(p(img("/u", "alt text"))), "image")

    "an image with a title" in roundTrips(doc(p(img("/u", "alt text", "the title"))), "image with title")

    "emphasis" in roundTrips(doc(p(em("stressed"))), "emphasis")

    "strong emphasis" in roundTrips(doc(p(strong("loud"))), "strong")

    "emphasis inside strong" in roundTrips(doc(p(em(strong("both")))), "emphasis inside strong")

    "inline HTML" in roundTrips(doc(p("before ", inlineHtml("<b>"), "after")), "inline html")

    "a hard break" in roundTrips(doc(p("first", br, "second")), "hard break")

    "a soft break inside one text node" in roundTrips(doc(p(text("first\nsecond"))), "soft break")
  }

  "nesting round-trips" - {

    "a quote holding a list holding code" in
      roundTrips(
        doc(quote(p("intro"), ul(li(p("item"), codeBlock("scala", "val x = 1\n"))))),
        "quote of list of code"
      )

    "a quote inside a quote" in roundTrips(doc(quote(quote(p("deep")))), "nested quote")

    "a list inside a list item" in
      roundTrips(doc(ul(li(p("outer"), ul(li("inner"))))), "nested list")

    "a spread list whose items hold two paragraphs" in
      roundTrips(doc(ul(true)(li(p("one"), p("two")))), "spread list of two paragraphs")

    "a list item holding a quote" in roundTrips(doc(ul(li(quote(p("quoted item"))))), "list item holding a quote")
  }

  "adversarial text round-trips" - {

    "a bullet marker at the start of a line" in roundTrips(doc(p(text("* not a list"))), "* not a list")

    "an ordered marker at the start of a line" in roundTrips(doc(p(text("1. not ordered"))), "1. not ordered")

    "a dash at the start of a line" in roundTrips(doc(p(text("- not a list"))), "- not a list")

    "backticks" in roundTrips(doc(p(text("`ticks`"))), "backticks")

    "brackets" in roundTrips(doc(p(text("[brackets]"))), "brackets")

    "angle brackets and an ampersand" in roundTrips(doc(p(text("<angle> & amp"))), "angle and amp")

    "an entity lookalike" in roundTrips(doc(p(text("&amp; &#35; &copy;"))), "entity lookalikes")

    "a hash at the start of a line" in roundTrips(doc(p(text("# not a heading"))), "# not a heading")

    "trailing double space before a newline" in roundTrips(doc(p(text("line  \nend"))), "trailing double space")

    "underscores and asterisks" in roundTrips(doc(p(text("_a_ *b* **c**"))), "emphasis lookalikes")

    "a setext underline lookalike on its own line" in
      roundTrips(doc(p(text("Title\n===\n---"))), "setext lookalike")

    "a backslash" in roundTrips(doc(p(text("a \\ b \\* c"))), "backslashes")

    "an indented-code lookalike" in roundTrips(doc(p(text("first\n    indented"))), "indented lookalike")

    "a quote marker at the start of a line" in roundTrips(doc(p(text("> not a quote"))), "> not a quote")

    "pipes, tildes and bangs" in roundTrips(doc(p(text("a | b ~c~ !d"))), "pipes tildes bangs")

    "an adversarial text inside every container" in
      roundTrips(
        doc(quote(ul(li(p(text("* not a list\n1. not ordered")))))),
        "adversarial text nested"
      )

    "adversarial text as a heading and inside emphasis" in
      roundTrips(doc(h2(text("# not deeper")), p(em(text("[x]")))), "adversarial inline")
  }

  "style" - {

    "a non-default style writes setext headings, `_` emphasis and `+` bullets" in {
      given MdStyle = MdStyle(bullet = '+', emphasisMarker = '_', headingStyle = HeadingStyle.Setext)
      val tree      = doc(h1("Title"), h2("Sub"), h3("Deep"), p(em("x")), ul(li("a"), li("b")))
      assert(
        MdWriter.write(tree) == "Title\n===\n\nSub\n---\n\n### Deep\n\n_x_\n\n+ a\n+ b\n",
        oneLine(MdWriter.write(tree))
      )
      roundTrips(tree, "non-default style")
    }

    "a tilde fence and a `)` ordered delimiter" in {
      given MdStyle = MdStyle(fence = '~', orderedDelimiter = ')')
      val tree      = doc(codeBlock("scala", "val x = 1\n"), ol(li("one")))
      assert(MdWriter.write(tree) == "~~~scala\nval x = 1\n~~~\n\n1) one\n", oneLine(MdWriter.write(tree)))
      roundTrips(tree, "tilde fence and paren delimiter")
    }

    "the two-space hard break style" in {
      given MdStyle = MdStyle(hardBreak = HardBreakStyle.Spaces)
      val tree      = doc(p("first", br, "second"))
      assert(MdWriter.write(tree) == "first  \nsecond\n", oneLine(MdWriter.write(tree)))
      roundTrips(tree, "two-space hard break")
    }

    "a per-node emphasis marker overrides the style" in
      assert(MdWriter.write(doc(p(em("x").withMeta(MdStyleKeys.emphasisMarker, '_')))) == "_x_\n")

    "a per-node bullet overrides the style" in
      assert(MdWriter.write(doc(ul(li("a")).withMeta(MdStyleKeys.bullet, '*'))) == "* a\n")

    "a per-node heading style overrides the style" in
      assert(MdWriter.write(doc(h1("T").withMeta(MdStyleKeys.headingStyle, HeadingStyle.Setext))) == "T\n===\n")

    "a per-node fence overrides the style" in
      assert(MdWriter.write(doc(codeBlock("x\n").withMeta(MdStyleKeys.fence, '~'))) == "~~~\nx\n~~~\n")

    "a per-node ordered delimiter overrides the style" in
      assert(MdWriter.write(doc(ol(li("a")).withMeta(MdStyleKeys.orderedDelimiter, ')'))) == "1) a\n")

    "a per-node strong marker overrides the style" in
      assert(MdWriter.write(doc(p(strong("x").withMeta(MdStyleKeys.strongMarker, '_')))) == "__x__\n")

    "a thematic break avoids the bullet in scope" in {
      assert(MdWriter.write(doc(hr)) == "***\n")
      assert(MdWriter.write(doc(hr))(using MdStyle(bullet = '*')) == "---\n")
    }
  }

  "MdWriter.escapeText" - {

    "backslash-escapes punctuation wherever it appears" in
      assert(MdWriter.escapeText("a*b_c[d]e<f>g&h#i!j~k|l`m\\n", atLineStart = false) ==
        "a\\*b\\_c\\[d\\]e\\<f\\>g\\&h\\#i\\!j\\~k\\|l\\`m\\\\n")

    "escapes a list or underline marker only at the head of a line" in {
      assert(MdWriter.escapeText("- a", atLineStart = true) == "\\- a")
      assert(MdWriter.escapeText("a - b", atLineStart = false) == "a - b")
      assert(MdWriter.escapeText("+ a", atLineStart = true) == "\\+ a")
      assert(MdWriter.escapeText("=== ", atLineStart = true) == "\\===&#32;")
    }

    "escapes the delimiter of a line-leading digit run, not the digits" in {
      assert(MdWriter.escapeText("12. a", atLineStart = true) == "12\\. a")
      assert(MdWriter.escapeText("12) a", atLineStart = true) == "12\\) a")
      assert(MdWriter.escapeText("12 a", atLineStart = true) == "12 a")
      assert(MdWriter.escapeText("a 1. b", atLineStart = false) == "a 1. b")
    }

    "writes a space the parser would strip as a character reference" in {
      assert(MdWriter.escapeText("  a", atLineStart = true) == "&#32;&#32;a")
      assert(MdWriter.escapeText("a  ", atLineStart = false) == "a&#32;&#32;")
      assert(MdWriter.escapeText("a  \nb", atLineStart = false) == "a&#32;&#32;\nb")
      assert(MdWriter.escapeText("a b", atLineStart = false) == "a b")
      assert(MdWriter.escapeText("\ta", atLineStart = true) == "&#9;a")
    }

    "restarts line-head escaping after every newline" in
      assert(MdWriter.escapeText("ok\n- a", atLineStart = false) == "ok\n\\- a")

    "leaves an empty value alone" in
      assert(MdWriter.escapeText("", atLineStart = true) == "")
  }

  "MdWriter.raise" - {

    "tiles and reprints the DSL example" in raises(example, "the DSL example")

    "tiles and reprints a nested document" in
      raises(doc(quote(ul(li(p("item"), codeBlock("scala", "val x = 1\n"))))), "nested document")

    "tiles and reprints adversarial text" in
      raises(doc(p(text("* not a list")), p(text("1. not ordered"))), "adversarial text")

    "an empty document raises an empty CST" in
      assert(Cst.print(MdWriter.raise(doc())) == "")
  }
