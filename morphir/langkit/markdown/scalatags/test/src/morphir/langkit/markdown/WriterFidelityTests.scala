package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.dsl.*
import morphir.langkit.markdown.dsl.given
import morphir.langkit.markdown.internal.{CstParser, Lower, MdWriter}

/**
 * Fidelity between [[MdWriter]] and [[ScalatagsCompiler]], the byte-exact conformance oracle: writing a tree and
 * reading it back must render exactly what the tree itself renders.
 *
 * This is a different measurement than [[MdWriterTests]]' round-trip suite, which compares the reparsed *tree*
 * structurally, forgiving over-escaping and inline segmentation. Here the comparison is HTML output, which forgives
 * nothing structural — a segmentation difference that happens to render the same text passes, and a byte the writer
 * spent wrong shows up as a different render. The two suites are complementary: one proves the writer's *meaning*
 * survives, this one proves its *presentation* does.
 *
 * Two suites. Hand cases mirror [[MdWriterTests]]' roster, one DSL tree per construct, so a construct that renders
 * correctly but writes wrong is caught close to its cause. The corpus sweep runs the same measurement over every
 * CommonMark 0.31.2 example, parsed and lowered first — the writer's real exam, because a hand-picked roster cannot
 * anticipate every combination the spec's own examples exercise.
 */
class WriterFidelityTests extends Test[Any]:

  private val SpecFixture = "commonmark-0.31.2-spec.json"

  private final case class Example(markdown: String, html: String, example: Int, section: String) derives Schema

  private def readFixture(name: String): String =
    import AllowUnsafe.embrace.danger
    val dir = Flag[String]("morphir.conformance.fixtures", "")
    if dir.isEmpty then
      throw new IllegalStateException(
        "morphir.conformance.fixtures is unset; Mill sets it through the MorphirMarkdownConformance* traits."
      )
    val file = (Path(dir) / name).unsafe
    file.read() match
      case Result.Success(text) => text
      case other => throw new IllegalStateException(s"missing or unreadable fixture ${file.show}: $other")

  private lazy val examples: Chunk[Example] =
    Json.decode[Chunk[Example]](readFixture(SpecFixture)) match
      case Result.Success(parsed) => parsed
      case other                  => throw new IllegalStateException(s"could not read $SpecFixture: $other")

  private def oneLine(text: String): String = text.replace("\n", "\\n")

  /** `tree` renders the same directly as it does after a round trip through [[MdWriter]] and back. */
  private def rendersSame(tree: MdNode.Root, label: String)(using MdStyle)(using AssertScope): Unit =
    val direct  = ScalatagsCompiler.render(tree)
    val written = ScalatagsCompiler.render(Lower.lower(MdWriter.raise(tree)))
    assert(
      direct == written,
      s"$label rendered differently after the writer." +
        s"\n  direct  ${oneLine(direct)}" +
        s"\n  written ${oneLine(written)}"
    )

  "hand cases" - {

    "per construct" - {

      "an ATX heading at every depth" in
        rendersSame(doc(h1("one"), h2("two"), h3("three"), h4("four"), h5("five"), h6("six")), "headings")

      "an empty heading" in rendersSame(doc(h1()), "empty heading")

      "a paragraph of plain text" in rendersSame(doc(p("just words")), "paragraph")

      "a fenced code block with an info string" in
        rendersSame(doc(codeBlock("scala", "val x = 1\n")), "fenced code with info")

      "a code block with no info string" in rendersSame(doc(codeBlock("plain body\n")), "code without info")

      "a code block whose body holds a fence-length backtick run" in
        rendersSame(doc(codeBlock("md", "```\nnested\n```\n")), "code holding a fence")

      "an empty code block" in rendersSame(doc(codeBlock("")), "empty code")

      "an HTML block" in rendersSame(doc(htmlBlock("<div>\n<p>x</p>\n</div>")), "html block")

      "a blockquote" in rendersSame(doc(quote(p("quoted"), p("twice"))), "blockquote")

      "a tight bullet list" in rendersSame(doc(ul(li("one"), li("two"))), "tight bullet list")

      "a spread bullet list" in rendersSame(doc(ul(true)(li("one"), li("two"))), "spread bullet list")

      "an ordered list numbered from one" in rendersSame(doc(ol(li("one"), li("two"))), "ordered list")

      "an ordered list numbered from an arbitrary start" in
        rendersSame(doc(ol(ListStart(7))(li("seven"), li("eight"), li("nine"), li("ten"))), "ordered list from seven")

      "a thematic break" in rendersSame(doc(hr), "thematic break")

      "inline code" in rendersSame(doc(p("call ", code("map"), " on it")), "inline code")

      "inline code holding backticks" in rendersSame(doc(p(code("a ` b"))), "inline code with a backtick")

      "inline code starting and ending with a backtick" in
        rendersSame(doc(p(code("`tick`"))), "inline code bounded by backticks")

      "inline code starting and ending with a space" in rendersSame(doc(p(code(" pad "))), "inline code padded")

      "a link" in rendersSame(doc(p(a("https://x.y")("here"))), "link")

      "a link with a title" in rendersSame(doc(p(a("/u", "the title")("here"))), "link with title")

      "a link whose destination holds parentheses" in
        rendersSame(doc(p(a("/a(b)")("here"))), "link with an awkward destination")

      "an image" in rendersSame(doc(p(img("/u", "alt text"))), "image")

      "an image with a title" in rendersSame(doc(p(img("/u", "alt text", "the title"))), "image with title")

      "emphasis" in rendersSame(doc(p(em("stressed"))), "emphasis")

      "strong emphasis" in rendersSame(doc(p(strong("loud"))), "strong")

      "emphasis inside strong" in rendersSame(doc(p(em(strong("both")))), "emphasis inside strong")

      "inline HTML" in rendersSame(doc(p("before ", inlineHtml("<b>"), "after")), "inline html")

      "a hard break" in rendersSame(doc(p("first", br, "second")), "hard break")

      "a soft break inside one text node" in rendersSame(doc(p(text("first\nsecond"))), "soft break")
    }

    "frontmatter" - {

      /**
       * Frontmatter carries no rendering of its own — [[MarkdownFold.compile]] folds only `Root.children`, so a
       * frontmatter tree's body renders exactly as it would with no frontmatter at all. What the writer owes here is
       * the frontmatter value itself: that it comes back structurally unchanged once written and reparsed.
       */
      "a frontmatter doc's body renders unaffected, and the frontmatter value round-trips structurally" in {
        val tree = doc(frontmatter = yaml("title: x\n"))(p("hi"))
        rendersSame(tree, "frontmatter doc body")
        val writtenRoot = Lower.lower(MdWriter.raise(tree))
        assert(
          writtenRoot.frontmatter.map(_.unpositioned) == tree.frontmatter.map(_.unpositioned),
          s"frontmatter did not round-trip: ${writtenRoot.frontmatter}"
        )
      }
    }

    "nested quote, list and code" - {

      "a quote holding a list holding code" in
        rendersSame(
          doc(quote(p("intro"), ul(li(p("item"), codeBlock("scala", "val x = 1\n"))))),
          "quote of list of code"
        )

      "a quote inside a quote" in rendersSame(doc(quote(quote(p("deep")))), "nested quote")

      "a list inside a list item" in
        rendersSame(doc(ul(li(p("outer"), ul(li("inner"))))), "nested list")

      "a spread list whose items hold two paragraphs" in
        rendersSame(doc(ul(true)(li(p("one"), p("two")))), "spread list of two paragraphs")

      "a list item holding a quote" in rendersSame(doc(ul(li(quote(p("quoted item"))))), "list item holding a quote")
    }

    "adversarial text" - {

      "a bullet marker at the start of a line" in rendersSame(doc(p(text("* not a list"))), "* not a list")

      "an ordered marker at the start of a line" in
        rendersSame(doc(p(text("1. not ordered"))), "1. not ordered")

      "a dash at the start of a line" in rendersSame(doc(p(text("- not a list"))), "- not a list")

      "backticks" in rendersSame(doc(p(text("`ticks`"))), "backticks")

      "brackets" in rendersSame(doc(p(text("[brackets]"))), "brackets")

      "angle brackets and an ampersand" in rendersSame(doc(p(text("<angle> & amp"))), "angle and amp")

      "an entity lookalike" in rendersSame(doc(p(text("&amp; &#35; &copy;"))), "entity lookalikes")

      "a hash at the start of a line" in rendersSame(doc(p(text("# not a heading"))), "# not a heading")

      "trailing double space before a newline" in
        rendersSame(doc(p(text("line  \nend"))), "trailing double space")

      "underscores and asterisks" in rendersSame(doc(p(text("_a_ *b* **c**"))), "emphasis lookalikes")

      "a setext underline lookalike on its own line" in
        rendersSame(doc(p(text("Title\n===\n---"))), "setext lookalike")

      "a backslash" in rendersSame(doc(p(text("a \\ b \\* c"))), "backslashes")

      "an indented-code lookalike" in rendersSame(doc(p(text("first\n    indented"))), "indented lookalike")

      "a quote marker at the start of a line" in rendersSame(doc(p(text("> not a quote"))), "> not a quote")

      "pipes, tildes and bangs" in rendersSame(doc(p(text("a | b ~c~ !d"))), "pipes tildes bangs")

      "an adversarial text inside every container" in
        rendersSame(
          doc(quote(ul(li(p(text("* not a list\n1. not ordered")))))),
          "adversarial text nested"
        )

      "adversarial text as a heading and inside emphasis" in
        rendersSame(doc(h2(text("# not deeper")), p(em(text("[x]")))), "adversarial inline")
    }

    "marker clashes" - {

      /**
       * The inner [[MdNode.Emphasis]]'s marker is pinned by an explicit [[MdStyleKeys.emphasisMarker]] override — `*`,
       * matching what the outer level would also pick from the default style — so the writer cannot shadow the inner
       * level to the alternate marker the way it does when neither level is pinned (see the "emphasis" case in
       * `writeInline`). The outer level takes the alternate marker instead, since an explicit inner choice wins.
       */
      "a sole nested emphasis whose explicit marker matches the outer's alternates the outer instead" in
        rendersSame(
          doc(p(em(em("x").withMeta(MdStyleKeys.emphasisMarker, '*')))),
          "explicit inner marker clash"
        )

      /**
       * Strong's default marker and Emphasis's default marker are both `*`: written naively, `strong(em("x"))` would
       * touch a two-character run against a one-character run of the same character, giving `***x***`, which a parse
       * always reads back as the longer match — Emphasis nested in Strong rather than Strong nested in Emphasis. The
       * Strong arm of `writeInline` mirrors the Emphasis arm's own same-child clash handling to avoid it.
       */
      "a sole nested emphasis inside strong with the same default marker shadows the inner marker" in
        rendersSame(doc(p(strong(em("x")))), "strong of emphasis, default markers")

      /**
       * The mirror direction never clashes: `***x***` is what CommonMark itself resolves to Emphasis nested in Strong,
       * which is exactly what `em(strong("x"))` means, so no adjustment is needed and a faithful spelling already
       * exists.
       */
      "a sole nested strong inside emphasis with the same default marker already has a faithful spelling" in
        rendersSame(doc(p(em(strong("x")))), "emphasis of strong, default markers")
    }

    /**
     * Five of the six shapes the writer's scaladoc documents as unspellable have an HTML rendering to compare, and none
     * of those five is skipped here. The sixth — a frontmatter value holding a line that reads as its own closing
     * delimiter — has no HTML rendering at all, since frontmatter never reaches the compiler fold that turns a tree
     * into HTML; it is pinned by written text alone, in `MdWriterTests`.
     *
     * The first two below are checked against the nearest-meaning behaviour the writer commits to, using the writer's
     * own fixed point rather than a hardcoded HTML string. A tree the writer already wrote once should write the same
     * way again — if `rendersSame` held here, the documented limit would already be gone, so the assertions are that it
     * does not, and that what the writer settles on is stable.
     *
     * The remaining three all share one context — content that must fit inside a single-physical-line ATX heading — so
     * they are pinned directly: both the exact text `MdWriter.write` produces (the `&#10;` or space substitution is
     * itself part of what the class scaladoc promises) and the exact HTML that text renders as once reparsed, set
     * against the HTML the original tree renders as directly. The two differ in every case, which is the limit; a
     * `rendersSame` assertion on any of them would fail today and should keep failing unless the writer somehow found a
     * way to spell a real line break inside one physical line.
     */
    "documented writer limits" - {

      "an empty inline code span has no faithful spelling; the writer's nearest meaning is a single space" in {
        val tree         = doc(p(code("")))
        val direct       = ScalatagsCompiler.render(tree)
        val onceWritten  = Lower.lower(MdWriter.raise(tree))
        val onceRendered = ScalatagsCompiler.render(onceWritten)
        val twiceWritten = Lower.lower(MdWriter.raise(onceWritten))
        val twice        = ScalatagsCompiler.render(twiceWritten)
        assert(
          direct != onceRendered,
          s"expected an empty code span to render differently than a single-space one; both rendered ${oneLine(direct)}"
        )
        assert(
          onceRendered == twice,
          "the writer's spelling of an empty code span should be a fixed point: writing what it already wrote " +
            "should render the same again"
        )
      }

      "a raw-space link destination has no faithful spelling as written; the writer settles on a stable " +
        "percent-encoded form" in {
          val tree         = doc(p(a("/a b")("x")))
          val direct       = ScalatagsCompiler.render(tree)
          val onceWritten  = Lower.lower(MdWriter.raise(tree))
          val onceRendered = ScalatagsCompiler.render(onceWritten)
          val twiceWritten = Lower.lower(MdWriter.raise(onceWritten))
          val twice        = ScalatagsCompiler.render(twiceWritten)
          assert(
            direct != onceRendered,
            s"expected a raw-space destination to render differently once normalised; both rendered ${oneLine(direct)}"
          )
          assert(
            onceRendered == twice,
            "the writer's spelling of a raw-space destination should be a fixed point: writing what it already " +
              "wrote should render the same again"
          )
        }

      "a hard break cannot be spelled inside an ATX heading; the writer's nearest meaning is a soft line " +
        "difference" in {
          val tree = doc(h1("a", br, "b"))
          assert(MdWriter.write(tree) == "# a&#10;b\n", oneLine(MdWriter.write(tree)))
          val direct   = ScalatagsCompiler.render(tree)
          val reparsed = ScalatagsCompiler.render(Lower.lower(MdWriter.raise(tree)))
          assert(direct == "<h1>a<br />\nb</h1>\n", oneLine(direct))
          assert(reparsed == "<h1>a\nb</h1>\n", oneLine(reparsed))
          assert(direct != reparsed, "expected the hard break to be lost, which is the limit under test")
        }

      "an inline code span's own line ending cannot be spelled inside an ATX heading; the writer's nearest " +
        "meaning is a space" in {
          val tree = doc(h1(code("a\nb")))
          assert(MdWriter.write(tree) == "# `a b`\n", oneLine(MdWriter.write(tree)))
          val direct   = ScalatagsCompiler.render(tree)
          val reparsed = ScalatagsCompiler.render(Lower.lower(MdWriter.raise(tree)))
          assert(direct == "<h1><code>a\nb</code></h1>\n", oneLine(direct))
          assert(reparsed == "<h1><code>a b</code></h1>\n", oneLine(reparsed))
          assert(
            direct != reparsed,
            "expected the code span's own line ending to be lost, which is the limit under test"
          )
        }

      "raw inline HTML's own line ending cannot be spelled inside an ATX heading; the writer's nearest meaning " +
        "is a space" in {
          val tree = doc(h1(inlineHtml("<b\nx>")))
          assert(MdWriter.write(tree) == "# <b x>\n", oneLine(MdWriter.write(tree)))
          val direct   = ScalatagsCompiler.render(tree)
          val reparsed = ScalatagsCompiler.render(Lower.lower(MdWriter.raise(tree)))
          assert(direct == "<h1><b\nx></h1>\n", oneLine(direct))
          assert(reparsed == "<h1><b x></h1>\n", oneLine(reparsed))
          assert(
            direct != reparsed,
            "expected the raw HTML's own line ending to be lost, which is the limit under test"
          )
        }
    }
  }

  "corpus sweep" - {

    s"CommonMark 0.31.2 renders 652/652 identically after a write/read round trip" in {
      assert(examples.size == 652)

      def rendered(root: MdNode.Root): String = ScalatagsCompiler.render(root)

      val failures = examples.flatMap { example =>
        val original       = Lower.lower(CstParser.parse(example.markdown))
        val originalRender = rendered(original)
        val writtenRender  = rendered(Lower.lower(MdWriter.raise(original)))
        if originalRender == writtenRender then Chunk.empty
        else Chunk((example, originalRender, writtenRender))
      }

      for
        _ <- Kyo.foreachDiscard(failures.take(10)) { case (example, originalRender, writtenRender) =>
          Console.printLine(
            s"  [${example.section}] example ${example.example}\n" +
              s"    source   ${oneLine(example.markdown)}\n" +
              s"    written  ${oneLine(writtenRender)}\n" +
              s"    source-rendered ${oneLine(originalRender)}"
          )
        }
        _ <-
          if failures.size > 10 then Console.printLine(s"  ... and ${failures.size - 10} more failures")
          else Kyo.unit
      yield assert(
        failures.isEmpty,
        s"${failures.size} of ${examples.size} examples render differently after a write/read round trip"
      )
    }
  }
