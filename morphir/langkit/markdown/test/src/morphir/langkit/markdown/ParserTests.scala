package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.MorphirException
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*

class ParserTests extends Test[Any]:

  /** The literal text of inline content, for assertions that do not care how it is split into nodes. */
  private def textOf(content: Chunk[Inline]): String =
    content.map {
      case Inline.Text(value, _)       => value
      case Inline.CodeSpan(value, _)   => value
      case Inline.Link(_, _, inner, _) => textOf(inner)
      case Inline.Image(_, _, alt, _)  => alt
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
        "- alpha\n- beta" -> Document(
          Chunk(Block.UnorderedList(
            Chunk(
              ListItem(Chunk(Inline.Text("alpha", Span(2, 5))), Span(2, 5)),
              ListItem(Chunk(Inline.Text("beta", Span(10, 4))), Span(10, 4))
            ),
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
        maxWork = WorkUnits(8L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(10L)
      )

      Parser.parse("x", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            error == ScanFailure(
              exceeded = ScanLimitExceeded.Work(limit = WorkUnits(8L), attempted = WorkUnits(9L)),
              offset = SourceOffset(1),
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
            error == ScanFailure(
              exceeded =
                ScanLimitExceeded.Work(limit = WorkUnits(30L), attempted = WorkUnits(31L)),
              offset = SourceOffset(2),
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
            case Block.UnorderedList(items, _) =>
              assert(items.map(item => textOf(item.content)) == Chunk("alpha", "beta"))
            case _ => assert(false)
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
