package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.dsl.*
import morphir.langkit.markdown.dsl.given
import morphir.langkit.markdown.internal.{Cst, MdWriter, Parser}

/**
 * The writer's contract is structural fidelity, not byte fidelity: `Parser.parse(MdWriter.write(tree))` succeeds and
 * yields the tree it was given, compared after normalization.
 *
 * Normalization drops the three differences a parse legitimately introduces. Positions are provenance — a generated
 * node honestly has none, a parsed one does — so they come off. A parse splits prose at every escape and every entity,
 * so `\*a` arrives as two [[MdNode.Text]] nodes where the author wrote one; adjacent text merges back into one node
 * before the comparison. And a [[MdNode.FrontMatter.Yaml]] value gains a trailing newline when it lacks one: the
 * closing delimiter always starts a line of its own, so a parse of a non-empty value can never hand back one without a
 * final `\n`, the same fact [[MdWriter.write]] relies on to emit it. Nothing else is forgiven: a broken parse or a
 * changed meaning is a failure, while over-escaping, which changes only the bytes, is not.
 */
class MdWriterTests extends Test[Any]:

  /** Newlines shown as `\n` so a mismatch prints on one line and a missing trailing newline is visible. */
  private def oneLine(text: String): String = text.replace("\n", "\\n")

  private def mergedTexts(nodes: Chunk[MdNode]): Chunk[MdNode] =
    val out = scala.collection.mutable.ListBuffer.empty[MdNode]
    nodes.foreach { node =>
      (out.lastOption, normalize(node)) match
        case (Some(MdNode.Text(before, meta)), MdNode.Text(after, _)) =>
          out.remove(out.size - 1)
          out += MdNode.Text(before + after, meta)
        case (_, normalized) => out += normalized
    }
    Chunk.from(out.toList)

  private def flowOf(nodes: Chunk[MdNode.FlowContent]): Chunk[MdNode.FlowContent] =
    mergedTexts(nodes).map(_.asInstanceOf[MdNode.FlowContent])

  private def phrasingOf(nodes: Chunk[MdNode.PhrasingContent]): Chunk[MdNode.PhrasingContent] =
    mergedTexts(nodes).map(_.asInstanceOf[MdNode.PhrasingContent])

  /**
   * A frontmatter value, padded with the trailing newline a parse always hands one back with (see the class doc):
   * `yaml("title: x")`, authored with no trailing newline, and the same value reparsed from what the writer wrote,
   * compare equal after this padding even though only one of them carries the `\n` before it.
   */
  private def normalizeFrontMatter(front: MdNode.FrontMatter): MdNode.FrontMatter = front match
    case MdNode.FrontMatter.Yaml(value, meta) =>
      val raw    = value.unwrap
      val padded = if raw.isEmpty || raw.endsWith("\n") then raw else raw + "\n"
      MdNode.FrontMatter.Yaml(YamlDocText(padded), meta)

  private def normalize(node: MdNode): MdNode = node match
    case MdNode.Root(children, frontmatter, meta) =>
      MdNode.Root(flowOf(children), frontmatter.map(normalizeFrontMatter), meta)
    case MdNode.Paragraph(children, meta)                    => MdNode.Paragraph(phrasingOf(children), meta)
    case MdNode.Heading(depth, children, meta)               => MdNode.Heading(depth, phrasingOf(children), meta)
    case MdNode.Blockquote(children, meta)                   => MdNode.Blockquote(flowOf(children), meta)
    case MdNode.List(ordered, start, spread, children, meta) =>
      MdNode.List(ordered, start, spread, children.map(item => normalize(item).asInstanceOf[MdNode.ListItem]), meta)
    case MdNode.ListItem(children, checked, meta) => MdNode.ListItem(flowOf(children), checked, meta)
    case MdNode.Link(url, title, children, meta)  => MdNode.Link(url, title, phrasingOf(children), meta)
    case MdNode.Emphasis(children, meta)          => MdNode.Emphasis(phrasingOf(children), meta)
    case MdNode.Strong(children, meta)            => MdNode.Strong(phrasingOf(children), meta)
    case MdNode.Delete(children, meta)            => MdNode.Delete(phrasingOf(children), meta)
    case leaf                                     => leaf

  private def normalized(root: MdNode.Root): MdNode = normalize(root.unpositioned)

  private def roundTrips(tree: MdNode.Root, label: String)(using MdStyle)(using MdProfile)(using AssertScope): Unit =
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
  private def raises(tree: MdNode.Root, label: String)(using MdStyle)(using AssertScope): Unit =
    val written  = MdWriter.write(tree)
    val document = MdWriter.raise(tree)
    val errors   = Cst.tilingErrors(document, written.length)
    assert(errors.isEmpty, s"$label raised a CST that does not tile: ${errors.mkString("; ")}")
    assert(Cst.print(document) == written, s"$label raised a CST that does not reprint what was written")

  private val example: MdNode.Root =
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

    /**
     * The one node with no faithful spelling. CommonMark strips a space from each end of a code span only when its
     * interior is not all spaces, so no backtick run says "holding nothing"; and `` `` `` says nothing at all, being an
     * unbalanced run that reparses as prose. The written form is pinned here, and so is what it reparses to, so the
     * limit is a fact of the suite rather than a claim in a comment.
     */
    "an empty inline code value writes as a single-space code span" in {
      assert(MdWriter.write(doc(p(code("")))) == "` `\n")
      Parser.parse(MdWriter.write(doc(p(code(""))))) match
        case Result.Success(reparsed) =>
          assert(normalized(reparsed) == normalized(doc(p(code(" ")))), s"${normalized(reparsed)}")
        case other => throw new IllegalStateException(s"an empty code span wrote unparseable text: $other")
    }

    "a code span of spaces keeps every one of them" in roundTrips(doc(p(code("   "))), "all-space code span")
  }

  "frontmatter" - {
    given MdProfile = MdProfile.commonmark.withYamlFrontmatter

    "a document with frontmatter writes the delimiter, value and delimiter first, then a blank line before the body" in {
      val tree    = doc(frontmatter = yaml("title: x"))(p("hi"))
      val written = MdWriter.write(tree)
      assert(written == "---\ntitle: x\n---\n\nhi\n", oneLine(written))
      roundTrips(tree, "frontmatter with a body")
    }

    "a frontmatter-only document ends right after the closing delimiter line" in {
      val tree    = doc(frontmatter = yaml("title: x"))()
      val written = MdWriter.write(tree)
      assert(written == "---\ntitle: x\n---\n", oneLine(written))
      roundTrips(tree, "frontmatter only")
    }

    "a frontmatter value that already ends in a newline is not given a second one" in
      assert(MdWriter.write(doc(frontmatter = yaml("title: x\n"))()) == "---\ntitle: x\n---\n")

    "an empty frontmatter value emits nothing between its two delimiter lines" in {
      val tree = doc(frontmatter = yaml(""))()
      assert(MdWriter.write(tree) == "---\n---\n")
      roundTrips(tree, "empty frontmatter value")
    }

    /**
     * The sixth shape with no faithful spelling (see the [[MdWriter]] class doc): a frontmatter value holding a line
     * that reads exactly as the closing delimiter. The writer spells the value as given — there is no escape a YAML
     * value line can take the way a fenced code block outgrows its own longest run of backticks — so a reparse closes
     * the block at that line instead of the one the author meant, and the remainder becomes body content that was never
     * there. Pinned by written text rather than round-tripped, since the round trip is exactly what breaks.
     */
    "a frontmatter value holding a `---` line has no faithful spelling; the writer spells it as given" in {
      val written = MdWriter.write(doc(frontmatter = yaml("a\n---\nb"))())
      assert(written == "---\na\n---\nb\n---\n", oneLine(written))
    }
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
      roundTrips(doc(ol(ListStart(7))(li("seven"), li("eight"), li("nine"), li("ten"))), "ordered list from seven")

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

    /**
     * Not [[roundTrips]]: the override rides [[MdMeta.data]], which the parser has no way to reconstruct, so the
     * reparsed tree's `Break` carries no such annotation and cannot compare equal to the tree that wrote it. What does
     * survive is the meaning — a two-space hard break — so the reparse is checked against the plain, unannotated tree
     * instead, the same way the empty-code-span case above checks its own no-faithful-spelling reparse.
     */
    "a per-node hard-break style overrides the style" in {
      val tree    = doc(p("first", br.withMeta(MdStyleKeys.hardBreak, HardBreakStyle.Spaces), "second"))
      val written = MdWriter.write(tree)
      assert(written == "first  \nsecond\n", oneLine(written))
      Parser.parse(written) match
        case Result.Success(reparsed) =>
          val expected = doc(p("first", br, "second"))
          assert(normalized(reparsed) == normalized(expected), s"${normalized(reparsed)}")
        case other => throw new IllegalStateException(s"per-node hard break wrote unparseable text: $other")
    }

    "a thematic break avoids the bullet in scope" in {
      assert(MdWriter.write(doc(hr)) == "***\n")
      assert(MdWriter.write(doc(hr))(using MdStyle(bullet = '*')) == "___\n")
    }

    /**
     * A tight list item writes its blocks with no blank line between them, which is where the third spelling of a
     * thematic break earns its keep: `---` under a paragraph is that paragraph's setext underline, so a break that
     * follows one inside a tight item would silently promote it to a heading.
     */
    "a thematic break after a paragraph in a tight item stays a break under every bullet" in {
      roundTrips(doc(ul(li(p("x"), hr))), "break after a paragraph, `-` bullet")
      roundTrips(doc(ul(li(p("x"), hr))), "break after a paragraph, `*` bullet")(using MdStyle(bullet = '*'))
      roundTrips(doc(ul(li(p("x"), hr))), "break after a paragraph, `+` bullet")(using MdStyle(bullet = '+'))
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

  /**
   * One ordered list holding one item, written and read back, reported by what came back rather than by equality: the
   * question here is whether the marker survived as a marker at all.
   */
  private def startsAsList(start: ListStart)(using MdStyle)(using MdProfile)(using AssertScope): Unit =
    val written = MdWriter.write(doc(ol(start)(li("x"))))
    Parser.parse(written) match
      case Result.Success(reparsed) =>
        assert(
          reparsed.children.headOption.exists(_.isInstanceOf[MdNode.List]),
          s"a list starting at ${start.toInt} wrote ${oneLine(written)}, which reparsed as " +
            s"${reparsed.children.headOption.fold("nothing")(_.getClass.getSimpleName)}"
        )
      case other =>
        throw new IllegalStateException(s"a list starting at ${start.toInt} wrote unparseable text: $other")

  /**
   * The writer's fidelity contract, held by the type rather than by care.
   *
   * `MdNode.List.start` was an `Int`, and two of the values it accepted wrote text no reader takes as a list: `-1. x`
   * has no digits before its delimiter, and `1000000000. x` has one digit more than CommonMark's marker holds. Both
   * reparsed as a paragraph. [[ListStart]] admits exactly the range the marker spells, so the pair below is the whole
   * property — nothing outside the range can be built, and everything inside it comes back as a list.
   */
  "ListStart holds the writer to its contract" - {

    "the type admits exactly what a one-to-nine-digit marker spells" in {
      assert(ListStart.fromInt(-1) == Absent)
      assert(ListStart.fromInt(Int.MinValue) == Absent)
      assert(ListStart.fromInt(0) == Present(ListStart.Zero))
      assert(ListStart.fromInt(1) == Present(ListStart.One))
      assert(ListStart.fromInt(999999999) == Present(ListStart.Max))
      assert(ListStart.fromInt(1000000000) == Absent)
      assert(ListStart.fromInt(Int.MaxValue) == Absent)
    }

    "every start the type admits writes a list that reads back as a list" in {
      val candidates = Chunk(0, 1, 2, 9, 10, 11, 99, 100, 12345, 99999999, 999999998, 999999999)
      candidates.foreach { value =>
        ListStart.fromInt(value) match
          case Present(start) => startsAsList(start)
          case Absent         => assert(false, s"$value is inside CommonMark's range and should have been a ListStart")
      }
    }

    "the two starts that used to break fidelity are no longer constructible" in {
      assert(ListStart.fromInt(-1) == Absent)
      assert(ListStart.fromInt(1000000000) == Absent)
      // ol(ListStart(-1))(li("x")) and ol(ListStart(1000000000))(li("x")) do not compile.
    }
  }
end MdWriterTests
