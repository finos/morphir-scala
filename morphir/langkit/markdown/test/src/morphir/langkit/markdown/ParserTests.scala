package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.MorphirException
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*

class ParserTests extends Test[Any]:

  /** The literal text of inline content, for assertions that do not care how it is split into nodes. */
  /**
   * The prose of a one-paragraph list item.
   *
   * A list item holds blocks, so even the shortest one is a paragraph. Tests that only care what an item says go
   * through this rather than repeating the unwrap.
   */
  private def paragraphOf(item: ListItem): Chunk[Inline] =
    item.content.headOption match
      case Some(Block.Paragraph(content, _)) => content
      case _                                 => Chunk.empty

  private def textOf(content: Chunk[Inline]): String =
    content.map {
      case Inline.Text(value, _)           => value
      case Inline.CodeSpan(value, _)       => value
      case Inline.Link(_, _, inner, _)     => textOf(inner)
      case Inline.Image(_, _, alt, _)      => alt
      case Inline.Emphasis(inner, _)       => textOf(inner)
      case Inline.StrongEmphasis(inner, _) => textOf(inner)
    }.mkString

  private def parseMetrics(source: String): ScanMetrics =
    Parser.parseWithMetrics(source, ScanBudget.UnsafeUnbounded) match
      case Result.Success((_, metrics)) => metrics
      case _                            => throw new AssertionError("unbounded parse unexpectedly failed")

  private def assertLinearWork(name: String, input: Int => String)(using AssertScope): Unit =
    val smallerSource = input(1024)
    assert(smallerSource.length == 1024, s"$name fixture must be exactly 1,024 UTF-16 code units")
    val smaller = parseMetrics(smallerSource).work.toLong

    val largerSource = input(2048)
    assert(largerSource.length == 2048, s"$name fixture must be exactly 2,048 UTF-16 code units")
    val larger = parseMetrics(largerSource).work.toLong

    val bound =
      if smaller > (Long.MaxValue - 64L) / 3L then Long.MaxValue
      else smaller * 3L + 64L
    assert(larger <= bound, s"$name work grew from $smaller to $larger (bound $bound)")

  private def limitedBudget(
      maxInputLength: InputSize,
      maxWork: WorkUnits,
      maxNestingDepth: NestingDepth,
      maxOutputNodes: NodeCount
  ): ScanBudget.Limited =
    ScanBudget.limited(
      maxInputLength = maxInputLength,
      maxWork = maxWork,
      maxNestingDepth = maxNestingDepth,
      maxOutputNodes = maxOutputNodes
    ).getOrThrow

  private def tightWorkBudget(inputLength: Int): ScanBudget.Limited =
    limitedBudget(
      maxInputLength = InputSize.fromCodeUnits(inputLength.toLong).getOrThrow,
      maxWork = WorkUnits(32L),
      maxNestingDepth = NestingDepth(16),
      maxOutputNodes = NodeCount(16L)
    )

  private def assertWorkExhausted(source: String)(using AssertScope): Unit =
    val result    = Parser.parse(source, tightWorkBudget(source.length))
    val exhausted = result match
      case Result.Failure(ParseError.Scan(ScanFailure(ScanLimitExceeded.Work(_, _), _, _))) => true
      case _                                                                                => false
    assert(exhausted, s"expected typed work exhaustion, got $result")

  "Parser.parse" - {
    "has deterministic near-linear work growth for representative block inputs" in {
      val inputs = Chunk[(String, Int => String)](
        "paragraph"        -> (size => "a" * size),
        "fence"            -> (size => "```\n" + ("a" * (size - 8)) + "\n```"),
        "list"             -> (size => "- a\n" * (size / 4)),
        "ambiguous prefix" -> (size => "#######\n" * (size / 8))
      )

      inputs.foreach { case (name, input) => assertLinearWork(name, input) }
    }
    "terminates hostile inputs through typed work exhaustion" in {
      val hostile = Chunk(
        "`" * 100000,
        "~" * 100000,
        "   - " * 20000,
        "a\r\n" * 30000,
        "\uD83D\uDE00" * 50000
      )

      hostile.foreach { source =>
        assert(source.length.toLong < ScanBudget.default.maxInputLength.toLong)
        assertWorkExhausted(source)
      }
    }
    "preserves exact documents across the existing block subset" in {
      val cases = Chunk(
        ""        -> Document(Chunk.empty, Span.zero),
        "# Title" -> Document(
          Chunk(Block.Heading(HeadingLevel.One, Chunk(Inline.Text("Title", Span(2, 5))), Span(0, 7))),
          Span(0, 7)
        ),
        "alpha\nbeta" ->
          Document(Chunk(Block.Paragraph(Chunk(Inline.Text("alpha\nbeta", Span(0, 10))), Span(0, 10))), Span(0, 10)),
        "```scala\none\n\ntwo\n```" -> Document(
          Chunk(Block.FencedCode(FenceInfo.parse("scala"), "one\n\ntwo\n", Span(0, 21))),
          Span(0, 21)
        ),
        "```\ncode" -> Document(
          Chunk(Block.FencedCode(FenceInfo.empty, "code", Span(0, 8))),
          Span(0, 8)
        ),
        "```\ncode\n" -> Document(
          Chunk(Block.FencedCode(FenceInfo.empty, "code\n", Span(0, 9))),
          Span(0, 9)
        ),
        // An item spans its whole line, marker included, because that is what the item occupies in the source. Its
        // paragraph spans only the content, which is what an inline node needs to point through.
        "- alpha\n- beta" -> Document(
          Chunk(Block.UnorderedList(
            Chunk(
              ListItem(Chunk(Block.Paragraph(Chunk(Inline.Text("alpha", Span(2, 5))), Span(2, 5))), Span(0, 7)),
              ListItem(Chunk(Block.Paragraph(Chunk(Inline.Text("beta", Span(10, 4))), Span(10, 4))), Span(8, 6))
            ),
            tight = true,
            Span(0, 14)
          )),
          Span(0, 14)
        ),
        "---"      -> Document(Chunk(Block.ThematicBreak(Span(0, 3))), Span(0, 3)),
        "# A\n\nB" -> Document(
          Chunk(
            Block.Heading(HeadingLevel.One, Chunk(Inline.Text("A", Span(2, 1))), Span(0, 3)),
            Block.Paragraph(Chunk(Inline.Text("B", Span(5, 1))), Span(5, 1))
          ),
          Span(0, 6)
        ),
        "# A\r\n\r\nB" -> Document(
          Chunk(
            Block.Heading(HeadingLevel.One, Chunk(Inline.Text("A", Span(2, 1))), Span(0, 3)),
            Block.Paragraph(Chunk(Inline.Text("B", Span(7, 1))), Span(7, 1))
          ),
          Span(0, 8)
        )
      )

      cases.foreach { case (source, expected) =>
        assert(Parser.parse(source) == Result.succeed(expected))
      }
    }
    "maps an input-size ceiling to an exact typed scanner failure" in {
      val budget = limitedBudget(
        maxInputLength = InputSize.codeUnits(4L),
        maxWork = WorkUnits(100L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(10L)
      )

      Parser.parse("hello", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            error == ScanFailure(
              exceeded = ScanLimitExceeded.InputLength(
                limit = InputSize.codeUnits(4L),
                actual = InputSize.codeUnits(5L)
              ),
              offset = SourceOffset.start,
              phase = Present(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "reports incremental output exhaustion at the consumed heading end" in {
      val budget = limitedBudget(
        maxInputLength = InputSize.codeUnits(100L),
        maxWork = WorkUnits(100L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount.one
      )

      Parser.parse("# Title", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            error == ScanFailure(
              exceeded = ScanLimitExceeded.OutputNodes(limit = NodeCount.one, attempted = NodeCount(2L)),
              offset = SourceOffset(7),
              phase = Present(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "charges deterministic work for scanner movement and line-local inspection" in {
      val budget = limitedBudget(
        maxInputLength = InputSize.codeUnits(100L),
        maxWork = WorkUnits(1L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(10L)
      )

      Parser.parse("x", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            error == ScanFailure(
              // Classifying a line once rather than asking it six to ten separate questions cut this from 9 work
              // units to 2. The charge is still made and still not refunded; there is simply far less of it.
              exceeded = ScanLimitExceeded.Work(limit = WorkUnits(1L), attempted = WorkUnits(2L)),
              offset = SourceOffset(0),
              phase = Present(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "does not refund speculative paragraph lookahead work" in {
      val budget = limitedBudget(
        maxInputLength = InputSize.codeUnits(100L),
        maxWork = WorkUnits(30L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(10L)
      )

      assert(Parser.parse("# h", budget) == Parser.parse("# h"))
      Parser.parse("x\n# h", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            // Paragraph lookahead asks more of each line than it once did -- whether it is a setext underline, and
            // whether it is a numbered item that may interrupt -- so a line inspection is charged for each and the
            // budget is exceeded a little later. The property under test is unchanged: the speculative work is not
            // refunded when the lookahead is rolled back.
            error == ScanFailure(
              exceeded =
                ScanLimitExceeded.Work(limit = WorkUnits(30L), attempted = WorkUnits(31L)),
              offset = SourceOffset(5),
              phase = Present(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "accepts the exact incremental output ceiling and preserves the default result" in {
      val budget = limitedBudget(
        maxInputLength = InputSize.codeUnits(100L),
        maxWork = WorkUnits(100L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(2L)
      )

      assert(Parser.parse("# Title", budget) == Parser.parse("# Title"))
    }
    "charges exactly one output node for an empty document" in {
      val exact = limitedBudget(
        maxInputLength = InputSize.codeUnits(1L),
        maxWork = WorkUnits(1L),
        maxNestingDepth = NestingDepth(1),
        maxOutputNodes = NodeCount.one
      )

      assert(Parser.parse("", exact) == Result.succeed(Document(Chunk.empty, Span.zero)))
    }
    "accepts an explicitly unsafe unbounded budget" in {
      Parser.parse("# Title", ScanBudget.UnsafeUnbounded) match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks == Chunk(Block.Heading(HeadingLevel.One, Chunk(Inline.Text("Title", Span(2, 5))), Span(0, 7))))
        case _ => assert(false)
    }
    "budgets fence metadata tokens before whitespace-heavy allocation amplification" in {
      val infoStrings = Chunk(
        "scala " + ("flag " * 20000),
        "{" + (".class " * 20000) + "}",
        "scala " + ("key=value " * 10000)
      )

      infoStrings.foreach { info =>
        val source = s"~~~ $info\n~~~"
        val budget = limitedBudget(
          maxInputLength = InputSize.fromCodeUnits(source.length.toLong + 1L).getOrThrow,
          maxWork = WorkUnits(100000000L),
          maxNestingDepth = NestingDepth(16),
          maxOutputNodes = NodeCount(10L)
        )

        Parser.parse(source, budget) match
          case Result.Failure(ParseError.Scan(ScanFailure(ScanLimitExceeded.OutputNodes(limit, attempted), _, _))) =>
            assert(limit == NodeCount(10L))
            assert(attempted == NodeCount(17L))
          case other => throw new AssertionError(s"expected typed metadata output exhaustion, got $other")
      }
    }
    "accepts the exact fence metadata output boundary and preserves structured info" in {
      val source        = "~~~ scala flag key=value {.class}\n~~~"
      val metadataNodes = NodeCount.from(FenceInfo.TokenOutputReservation.toLong * 4L).getOrThrow
      val budget        = limitedBudget(
        maxInputLength = InputSize.fromCodeUnits(source.length.toLong).getOrThrow,
        maxWork = WorkUnits(10000L),
        maxNestingDepth = NestingDepth(16),
        maxOutputNodes = NodeCount.from(metadataNodes.toLong + 2L).getOrThrow
      )

      Parser.parse(source, budget) match
        case Result.Success(Document(Chunk(Block.FencedCode(info, "", _)), _)) =>
          assert(info == FenceInfo.parse("scala flag key=value {.class}"))
          assert(info.language == Present("scala"))
          assert(info.flag("flag"))
          assert(info.option("key") == Present("value"))
          assert(info.classes == Chunk("class"))
        case other => throw new AssertionError(s"expected exact-boundary fence success, got $other")
    }
    "reads an ATX heading and a paragraph" in {
      Parser.parse("# Title\n\nHello") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(level, content, _) =>
              assert(level.toInt == 1)
              assert(textOf(content) == "Title")
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph(content, _) => assert(textOf(content) == "Hello")
            case _                           => assert(false)
        case _ => assert(false)
    }
    "splits a heading from the next block at a single newline" in {
      Parser.parse("# Title\nBody") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(level, content, _) =>
              assert(level.toInt == 1)
              assert(textOf(content) == "Title")
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph(content, _) => assert(textOf(content) == "Body")
            case _                           => assert(false)
        case _ => assert(false)
    }
    "splits consecutive headings without a blank line" in {
      Parser.parse("# One\n## Two") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(HeadingLevel.One, content, _) => assert(textOf(content) == "One")
            case _                                           => assert(false)
          doc.blocks(1) match
            case Block.Heading(HeadingLevel.Two, content, _) => assert(textOf(content) == "Two")
            case _                                           => assert(false)
        case _ => assert(false)
    }
    "spans the whole source" in {
      val source = "# Title\n\nHello"
      Parser.parse(source) match
        case Result.Success(doc) => assert(doc.span == Span(0, source.length))
        case _                   => assert(false)
    }
    "keeps original offsets when the source uses CRLF line endings" in {
      val source = "# Title\r\n\r\nHello"
      Parser.parse(source) match
        case Result.Success(doc) =>
          assert(doc.span == Span(0, source.length))
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(HeadingLevel.One, content, span) =>
              assert(textOf(content) == "Title")
              assert(span == Span(0, "# Title".length))
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph(content, span) if textOf(content) == "Hello" =>
              assert(span.offset == source.indexOf("Hello"))
              assert(span.length == "Hello".length)
            case _ => assert(false)
        case _ => assert(false)
    }
    "keeps a lone carriage return as paragraph text in the original span" in {
      val source = "alpha\rbeta"

      assert(
        Parser.parse(source) == Result.succeed(
          Document(
            Chunk(Block.Paragraph(Chunk(Inline.Text(source, Span(0, source.length))), Span(0, source.length))),
            Span(0, source.length)
          )
        )
      )
    }
    "accepts an empty document" in {
      Parser.parse("") match
        case Result.Success(doc) => assert(doc.blocks.isEmpty)
        case _                   => assert(false)
    }
    "reads a fenced code block including blank lines inside the fence" in {
      val source = "```scala\nval x = 1\n\nval y = 2\n```"
      Parser.parse(source) match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.FencedCode(info, content, _) =>
              assert(info.raw == "scala")
              assert(info.language == Present("scala"))
              assert(content == "val x = 1\n\nval y = 2\n")
            case _ => assert(false)
        case _ => assert(false)
    }
    "keeps a shorter matching fence inside a longer fenced code block" in {
      val source = "````\nvalue\n```\nafter\n````"
      Parser.parse(source) match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.FencedCode(_, content, _) =>
              assert(content == "value\n```\nafter\n")
            case _ => assert(false)
        case _ => assert(false)
    }
    "does not recognize a fence indented by four spaces" in {
      Parser.parse("    ```\nvalue\n    ```") match
        case Result.Success(doc) =>
          assert(doc.blocks.forall {
            case Block.FencedCode(_, _, _) => false
            case _                         => true
          })
        case _ => assert(false)
    }
    "removes opening-fence indentation from fenced code content" in {
      val source = "   ```\n   value\n value\n```"
      Parser.parse(source) match
        case Result.Success(Document(blocks, _)) =>
          blocks(0) match
            case Block.FencedCode(_, content, _) => assert(content == "value\nvalue\n")
            case _                               => assert(false)
        case _ => assert(false)
    }
    "requires a closing fence with matching marker, valid indentation, and no trailing text" in {
      val source = "~~~\nfirst\n```\n    ~~~\n~~~ language\nlast\n~~~~\t"
      Parser.parse(source) match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case Block.FencedCode(_, content, _) =>
              assert(content == "first\n```\n    ~~~\n~~~ language\nlast\n")
            case _ => assert(false)
        case _ => assert(false)
    }
    "does not accept non-space trailing characters on a closing fence" in {
      val source = "~~~\nfirst\n~~~\u000c\nlast\n~~~"
      Parser.parse(source) match
        case Result.Success(Document(blocks, _)) =>
          blocks(0) match
            case Block.FencedCode(_, content, _) => assert(content == "first\n~~~\u000c\nlast\n")
            case _                               => assert(false)
        case _ => assert(false)
    }
    "trims only spaces and tabs around an info string" in {
      Parser.parse("~~~ \u000c example \u000c \nbody\n~~~") match
        case Result.Success(Document(blocks, _)) =>
          blocks(0) match
            case Block.FencedCode(info, _, _) => assert(info.raw == "\u000c example \u000c")
            case _                            => assert(false)
        case _ => assert(false)
    }
    "allows a three-space opening and closing fence to interrupt a paragraph" in {
      Parser.parse("before\n   ```\ncode\n  ```\nafter") match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks.size == 3)
          assert(blocks(0) == Block.Paragraph(Chunk(Inline.Text("before", Span(0, 6))), Span(0, 6)))
          blocks(1) match
            case Block.FencedCode(_, content, _) => assert(content == "code\n")
            case _                               => assert(false)
          assert(blocks(2) == Block.Paragraph(Chunk(Inline.Text("after", Span(25, 5))), Span(25, 5)))
        case _ => assert(false)
    }
    "uses end of document as the close of an unclosed fence" in {
      Parser.parse("```\ncode") match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case Block.FencedCode(_, content, _) => assert(content == "code")
            case _                               => assert(false)
        case _ => assert(false)
    }
    "allows an empty fenced code block" in {
      Parser.parse("```\n```") match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case Block.FencedCode(_, content, _) => assert(content.isEmpty)
            case _                               => assert(false)
        case _ => assert(false)
    }
    "accepts tildes in a tilde-fence info string but not backticks in a backtick-fence info string" in {
      Parser.parse("~~~ aa ~~~ `example`\nbody\n~~~\n\n``` `example`\nbody") match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks.size == 2)
          blocks(0) match
            case Block.FencedCode(info, content, _) =>
              assert(info.raw == "aa ~~~ `example`")
              assert(info.language == Present("aa"))
              assert(content == "body\n")
            case _ => assert(false)
          blocks(1) match
            // Not a fence: a backtick fence's info string may not contain a backtick, so the line is prose --
            // and its `example` is now an ordinary code span.
            case Block.Paragraph(content, _) =>
              assert(content.size == 3)
              assert(content(0) == Inline.Text("``` ", Span(31, 4)))
              assert(content(1) == Inline.CodeSpan("example", Span(35, 9)))
              assert(content(2) == Inline.Text("\nbody", Span(44, 5)))
            case _ => assert(false)
        case _ => assert(false)
    }
    "reads consecutive unordered list items as one list" in {
      Parser.parse("- alpha\n- beta") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.UnorderedList(items, _, _) =>
              assert(items.map(item => textOf(paragraphOf(item))) == Chunk("alpha", "beta"))
            case _ => assert(false)
        case _ => assert(false)
    }
    "reads a block quote as a container of blocks (spec example 228)" in {
      Parser.parse("> # Foo\n> bar\n> baz\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.BlockQuote(content, _) =>
              assert(content.size == 2)
              content(0) match
                case Block.Heading(level, inner, _) =>
                  assert(level == HeadingLevel.One)
                  assert(textOf(inner) == "Foo")
                case _ => assert(false)
              content(1) match
                case Block.Paragraph(inner, _) => assert(textOf(inner) == "bar\nbaz")
                case _                         => assert(false)
            case _ => assert(false)
        case _ => assert(false)
    }
    "keeps a quoted paragraph going when a line drops the marker (spec example 232)" in {
      Parser.parse("> # Foo\n> bar\nbaz\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.BlockQuote(content, _) =>
              assert(content.size == 2)
              content(1) match
                case Block.Paragraph(inner, _) => assert(textOf(inner) == "bar\nbaz")
                case _                         => assert(false)
            case _ => assert(false)
        case _ => assert(false)
    }
    // A lazy line is prose and nothing else. `---` under a quoted paragraph is a thematic break outside the quote,
    // not a setext underline inside it, and getting that wrong silently swallows the break.
    "will not let a lazy line close a setext heading (spec example 234)" in {
      Parser.parse("> foo\n---\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.BlockQuote(content, _) =>
              content(0) match
                case Block.Paragraph(inner, _) => assert(textOf(inner) == "foo")
                case _                         => assert(false)
            case _ => assert(false)
          doc.blocks(1) match
            case Block.ThematicBreak(_) => assert(true)
            case _                      => assert(false)
        case _ => assert(false)
    }
    "reads a quote with no content as an empty container (spec example 239)" in {
      Parser.parse(">\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.BlockQuote(content, _) => assert(content.isEmpty)
            case _                            => assert(false)
        case _ => assert(false)
    }
    "splits a quote at a blank line and joins it at a bare marker (spec examples 242 and 244)" in {
      Parser.parse("> foo\n\n> bar\n") match
        case Result.Success(doc) => assert(doc.blocks.size == 2)
        case _                   => assert(false)

      Parser.parse("> foo\n>\n> bar\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.BlockQuote(content, _) => assert(content.size == 2)
            case _                            => assert(false)
        case _ => assert(false)
    }
    "lets a quote interrupt the paragraph above it (spec example 245)" in {
      Parser.parse("foo\n> bar\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Paragraph(inner, _) => assert(textOf(inner) == "foo")
            case _                         => assert(false)
          doc.blocks(1) match
            case Block.BlockQuote(_, _) => assert(true)
            case _                      => assert(false)
        case _ => assert(false)
    }
    "nests quotes as deeply as the markers go, laziness included (spec example 250)" in {
      Parser.parse("> > > foo\nbar\n") match
        case Result.Success(doc) =>
          def onlyQuote(block: Block): Block =
            block match
              case Block.BlockQuote(content, _) =>
                assert(content.size == 1)
                content(0)
              case other => other

          val innermost = onlyQuote(onlyQuote(onlyQuote(doc.blocks(0))))
          innermost match
            case Block.Paragraph(inner, _) => assert(textOf(inner) == "foo\nbar")
            case _                         => assert(false)
        case _ => assert(false)
    }
    // Stripping `> ` shortens the text, so a span taken from the remainder would point four characters early unless
    // the offset moves with it.
    "keeps inline spans pointing at the source through a quote marker" in {
      val source = "> alpha\n"
      Parser.parse(source) match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.BlockQuote(content, _) =>
              content(0) match
                case Block.Paragraph(Chunk(Inline.Text(value, span)), _) =>
                  assert(value == "alpha")
                  assert(source.substring(span.offset, span.end) == "alpha")
                case _ => assert(false)
            case _ => assert(false)
        case _ => assert(false)
    }
    // Every line of a paragraph loses its leading whitespace, not only the first. An indented line under a paragraph is
    // therefore neither indented code nor a setext underline, and its inline spans must still point at the source.
    "strips the indentation from every line of a paragraph (spec examples 87 and 238)" in {
      Parser.parse("Foo\n    ---\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.Paragraph(content, _) => assert(textOf(content) == "Foo\n---")
            case other                       => assert(false, s"expected a paragraph, got $other")
        case _ => assert(false)

      Parser.parse("> foo\n    - bar\n") match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.BlockQuote(content, _) =>
              content(0) match
                case Block.Paragraph(inner, _) => assert(textOf(inner) == "foo\n- bar")
                case other                     => assert(false, s"expected a paragraph, got $other")
            case other => assert(false, s"expected a block quote, got $other")
        case _ => assert(false)
    }
    "keeps a stripped continuation line's spans pointing at the source" in {
      val source = "alpha\n    `beta`\n"
      Parser.parse(source) match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.Paragraph(content, _) =>
              content.collectFirst { case Inline.CodeSpan(value, span) => (value, span) } match
                // The span covers the backticks, as an inline span does everywhere: what it must not do is point four
                // characters early, which is what would happen if the text lost its indentation and the offset did not.
                case Some((value, span)) =>
                  assert(value == "beta")
                  assert(source.substring(span.offset, span.end) == "`beta`")
                case None => assert(false, "expected a code span in the continuation line")
            case other => assert(false, s"expected a paragraph, got $other")
        case _ => assert(false)
    }
    // The whole point of the container work: an item holds blocks, so it holds whatever a document holds.
    "reads a list item as a container of blocks (spec example 263)" in {
      Parser.parse("1.  foo\n\n    ```\n    bar\n    ```\n\n    baz\n\n    > bam\n") match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.OrderedList(_, items, tight, _) =>
              assert(items.size == 1)
              assert(!tight, "blank lines between an item's blocks make the list loose")
              val content = items(0).content
              assert(content.size == 4)
              assert(content(0).isInstanceOf[Block.Paragraph])
              assert(content(1).isInstanceOf[Block.FencedCode])
              assert(content(2).isInstanceOf[Block.Paragraph])
              assert(content(3).isInstanceOf[Block.BlockQuote])
            case other => assert(false, s"expected an ordered list, got $other")
        case _ => assert(false)
    }
    // Two blocks in one item are not enough to make a list loose; a blank line between them is. Getting this wrong is
    // invisible in the AST and shows up only as `p` elements appearing or vanishing in the output.
    "calls a list tight unless a blank line separates blocks or items" in {
      def tightnessOf(source: String): Boolean =
        Parser.parse(source) match
          case Result.Success(doc) =>
            doc.blocks(0) match
              case Block.UnorderedList(_, tight, _) => tight
              case other                            => throw new AssertionError(s"expected a list, got $other")
          case other => throw new AssertionError(s"parse failed: $other")

      assert(tightnessOf("- one\n- two\n"))
      assert(tightnessOf("- a\n  - b\n"), "a nested list is a second block, but no blank line separates them")
      assert(!tightnessOf("- one\n\n- two\n"), "a blank line between items")
      assert(!tightnessOf("* a\n*\n\n* c\n"), "a blank line after an empty item still separates it (spec example 315)")
      assert(!tightnessOf("- one\n\n  two\n"), "a blank line between an item's blocks")
    }
    "nests a list inside the item that indents it" in {
      Parser.parse("- a\n  - b\n") match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.UnorderedList(items, _, _) =>
              assert(items.size == 1, "the indented marker belongs to the item above it, not beside it")
              assert(items(0).content.size == 2)
              items(0).content(1) match
                case Block.UnorderedList(inner, _, _) => assert(inner.size == 1)
                case other                            => assert(false, s"expected a nested list, got $other")
            case other => assert(false, s"expected a list, got $other")
        case _ => assert(false)
    }
    // The lazy rule is stricter than the one for a line that kept its markers: `2.` may not interrupt a paragraph, but
    // the paragraph is not what this line fell out of. Before this was separated out, the second item was swallowed
    // into the first item's paragraph.
    "starts a new item on a marker that drops out of the item above (spec example 302)" in {
      Parser.parse("1. one\n2. two\n") match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.OrderedList(start, items, _, _) =>
              assert(start == 1)
              assert(items.size == 2)
            case other => assert(false, s"expected an ordered list, got $other")
        case _ => assert(false)
    }
    "reads an item with nothing after its marker, and stops at the second blank (spec example 280)" in {
      Parser.parse("-\n\n  foo\n") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2, "a list item may begin with at most one blank line")
          doc.blocks(0) match
            case Block.UnorderedList(items, _, _) =>
              assert(items.size == 1)
              assert(items(0).content.isEmpty)
            case other => assert(false, s"expected a list, got $other")
          assert(doc.blocks(1).isInstanceOf[Block.Paragraph])
        case _ => assert(false)
    }
    // Four spaces past the marker is code inside the item, not a very indented paragraph: the item spends one space
    // and the rest is content.
    "gives an item's over-indented content to a code block (spec example 270)" in {
      Parser.parse("- foo\n\n      bar\n") match
        case Result.Success(doc) =>
          doc.blocks(0) match
            case Block.UnorderedList(items, _, _) =>
              items(0).content(1) match
                case Block.IndentedCode(content, _) => assert(content == "bar\n")
                case other                          => assert(false, s"expected indented code, got $other")
            case other => assert(false, s"expected a list, got $other")
        case _ => assert(false)
    }
    "reads a thematic break between paragraphs" in {
      Parser.parse("Hello\n\n---\n\nWorld") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 3)
          doc.blocks(1) match
            case Block.ThematicBreak(_) => assert(true)
            case _                      => assert(false)
          doc.blocks(0) match
            case Block.Paragraph(content, _) => assert(textOf(content) == "Hello")
            case _                           => assert(false)
          doc.blocks(2) match
            case Block.Paragraph(content, _) => assert(textOf(content) == "World")
            case _                           => assert(false)
        case _ => assert(false)
    }
  }

  "ParseError" - {
    "exposes the root message and returns Syntax from its compatibility constructor" in {
      val syntax: ParseError.Syntax = ParseError("expected closing fence")
      val root: ParseError          = syntax

      assert(root.message == "expected closing fence")
      assert(root.getMessage == root.message)
      assert(ParseError.unapply(root).contains(root.message))
    }
    "keeps Syntax apply and unapply compatibility" in {
      val error = ParseError("expected closing fence")
      error match
        case ParseError(message) =>
          assert(error == ParseError.Syntax("expected closing fence"))
          assert(message == "expected closing fence")
    }
    "keeps typed scanner failures exception-compatible with a stable informative message" in {
      val error = ParseError.Scan(
        ScanFailure(
          exceeded = ScanLimitExceeded.InputLength(
            limit = InputSize.codeUnits(4L),
            actual = InputSize.codeUnits(5L)
          ),
          offset = SourceOffset.start,
          phase = Present(ScanPhase("markdown.blocks"))
        )
      )

      assert(error.isInstanceOf[Exception])
      assert(error.getMessage == "Markdown scan failed at offset 0 during markdown.blocks: InputLength(4,5)")
      assert(ParseError.unapply(error).contains(error.getMessage))
    }
    "unifies syntax and scanner failures as MorphirException values while retaining their messages" in {
      val syntax: MorphirException = ParseError.Syntax("expected closing fence")
      val scan: MorphirException   = ParseError.Scan(
        ScanFailure(
          exceeded = ScanLimitExceeded.Work(limit = WorkUnits(0L), attempted = WorkUnits(1L)),
          offset = SourceOffset.start,
          phase = Absent
        )
      )

      assert(syntax.getMessage == "expected closing fence")
      assert(scan.getMessage.startsWith("Markdown scan failed at offset 0"))
    }
  }
