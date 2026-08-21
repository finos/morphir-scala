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
  private def paragraphOf(item: MdcNode.ListItem): Chunk[MdcNode.PhrasingContent] =
    item.children.headOption match
      case Some(MdcNode.Paragraph(content, _)) => content
      case _                                   => Chunk.empty

  private def textOf(content: Chunk[MdcNode.PhrasingContent]): String =
    content.map {
      case MdcNode.Text(value, _)       => value
      case MdcNode.InlineCode(value, _) => value
      case MdcNode.Link(_, _, inner, _) => textOf(inner)
      case MdcNode.Image(_, _, alt, _)  => alt
      case MdcNode.Emphasis(inner, _)   => textOf(inner)
      case MdcNode.Strong(inner, _)     => textOf(inner)
      // Raw HTML is markup rather than text, and contributes none: a test asserting on it matches the node itself.
      case MdcNode.InlineHtml(_, _) => ""
      // A hard break reads as the line ending it stands for, so a test that only cares what the prose says need not
      // know which kind of break produced it.
      case MdcNode.Break(_) => "\n"
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
        "😀" * 50000
      )

      hostile.foreach { source =>
        assert(source.length.toLong < ScanBudget.default.maxInputLength.toLong)
        assertWorkExhausted(source)
      }
    }
    "preserves exact documents across the existing block subset" in {
      val cases = Chunk(
        ""        -> MdcNode.Root(Chunk.empty, meta = MdcMeta.at(Span.zero)),
        "# Title" -> MdcNode.Root(
          Chunk(MdcNode.Heading(
            HeadingLevel.One,
            Chunk(MdcNode.Text("Title", MdcMeta.at(Span(2, 5)))),
            MdcMeta.at(Span(0, 7))
          )),
          meta = MdcMeta.at(Span(0, 7))
        ),
        "alpha\nbeta" ->
          MdcNode.Root(
            Chunk(MdcNode.Paragraph(
              Chunk(MdcNode.Text("alpha\nbeta", MdcMeta.at(Span(0, 10)))),
              MdcMeta.at(Span(0, 10))
            )),
            meta = MdcMeta.at(Span(0, 10))
          ),
        "```scala\none\n\ntwo\n```" -> MdcNode.Root(
          Chunk(MdcNode.Code(FenceInfo.parse("scala"), "one\n\ntwo\n", MdcMeta.at(Span(0, 21)))),
          meta = MdcMeta.at(Span(0, 21))
        ),
        "```\ncode" -> MdcNode.Root(
          Chunk(MdcNode.Code(FenceInfo.empty, "code", MdcMeta.at(Span(0, 8)))),
          meta = MdcMeta.at(Span(0, 8))
        ),
        "```\ncode\n" -> MdcNode.Root(
          Chunk(MdcNode.Code(FenceInfo.empty, "code\n", MdcMeta.at(Span(0, 9)))),
          meta = MdcMeta.at(Span(0, 9))
        ),
        // An item spans its whole line, marker included, because that is what the item occupies in the source. Its
        // paragraph spans only the content, which is what an inline node needs to point through.
        "- alpha\n- beta" -> MdcNode.Root(
          Chunk(MdcNode.List(
            ordered = false,
            start = Absent,
            spread = false,
            Chunk(
              MdcNode.ListItem(
                Chunk(MdcNode.Paragraph(Chunk(MdcNode.Text("alpha", MdcMeta.at(Span(2, 5)))), MdcMeta.at(Span(2, 5)))),
                MdcMeta.at(Span(0, 7))
              ),
              MdcNode.ListItem(
                Chunk(MdcNode.Paragraph(Chunk(MdcNode.Text("beta", MdcMeta.at(Span(10, 4)))), MdcMeta.at(Span(10, 4)))),
                MdcMeta.at(Span(8, 6))
              )
            ),
            MdcMeta.at(Span(0, 14))
          )),
          meta = MdcMeta.at(Span(0, 14))
        ),
        "---"      -> MdcNode.Root(Chunk(MdcNode.ThematicBreak(MdcMeta.at(Span(0, 3)))), meta = MdcMeta.at(Span(0, 3))),
        "# A\n\nB" -> MdcNode.Root(
          Chunk(
            MdcNode.Heading(HeadingLevel.One, Chunk(MdcNode.Text("A", MdcMeta.at(Span(2, 1)))), MdcMeta.at(Span(0, 3))),
            MdcNode.Paragraph(Chunk(MdcNode.Text("B", MdcMeta.at(Span(5, 1)))), MdcMeta.at(Span(5, 1)))
          ),
          meta = MdcMeta.at(Span(0, 6))
        ),
        "# A\r\n\r\nB" -> MdcNode.Root(
          Chunk(
            MdcNode.Heading(HeadingLevel.One, Chunk(MdcNode.Text("A", MdcMeta.at(Span(2, 1)))), MdcMeta.at(Span(0, 3))),
            MdcNode.Paragraph(Chunk(MdcNode.Text("B", MdcMeta.at(Span(7, 1)))), MdcMeta.at(Span(7, 1)))
          ),
          meta = MdcMeta.at(Span(0, 8))
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
        maxWork = WorkUnits(20L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(10L)
      )

      assert(Parser.parse("# h", budget) == Parser.parse("# h"))

      // Said plainly: reading `x` then `# h` costs more than reading each on its own, and the difference is the
      // lookahead the paragraph did at `# h` before rejecting it. Rolling that lookahead back rewinds the cursor and
      // nothing else -- there is no refund.
      val paragraphAlone = parseMetrics("x").work.toLong
      val headingAlone   = parseMetrics("# h").work.toLong
      val together       = parseMetrics("x\n# h").work.toLong
      assert(
        together > paragraphAlone + headingAlone,
        s"speculative lookahead was refunded: $together <= $paragraphAlone + $headingAlone"
      )

      Parser.parse("x\n# h", budget) match
        case Result.Failure(ParseError.Scan(error)) =>
          assert(
            error == ScanFailure(
              exceeded = ScanLimitExceeded.Work(limit = WorkUnits(20L), attempted = WorkUnits(22L)),
              offset = SourceOffset(5),
              phase = Present(ScanPhase("markdown.blocks"))
            )
          )
        case other => assert(false, s"expected the work budget to be exceeded, got $other")
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

      assert(Parser.parse("", exact) == Result.succeed(MdcNode.Root(Chunk.empty, meta = MdcMeta.at(Span.zero))))
    }
    "accepts an explicitly unsafe unbounded budget" in {
      Parser.parse("# Title", ScanBudget.UnsafeUnbounded) match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(
            blocks == Chunk(
              MdcNode.Heading(
                HeadingLevel.One,
                Chunk(MdcNode.Text("Title", MdcMeta.at(Span(2, 5)))),
                MdcMeta.at(Span(0, 7))
              )
            )
          )
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
        case Result.Success(MdcNode.Root(Chunk(MdcNode.Code(info, "", _)), _, _)) =>
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
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Heading(level, content, _) =>
              assert(level.toInt == 1)
              assert(textOf(content) == "Title")
            case _ => assert(false)
          doc.children(1) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "Hello")
            case _                             => assert(false)
        case _ => assert(false)
    }
    "splits a heading from the next block at a single newline" in {
      Parser.parse("# Title\nBody") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Heading(level, content, _) =>
              assert(level.toInt == 1)
              assert(textOf(content) == "Title")
            case _ => assert(false)
          doc.children(1) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "Body")
            case _                             => assert(false)
        case _ => assert(false)
    }
    "splits consecutive headings without a blank line" in {
      Parser.parse("# One\n## Two") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Heading(HeadingLevel.One, content, _) => assert(textOf(content) == "One")
            case _                                             => assert(false)
          doc.children(1) match
            case MdcNode.Heading(HeadingLevel.Two, content, _) => assert(textOf(content) == "Two")
            case _                                             => assert(false)
        case _ => assert(false)
    }
    "spans the whole source" in {
      val source = "# Title\n\nHello"
      Parser.parse(source) match
        case Result.Success(doc) => assert(doc.span == Present(Span(0, source.length)))
        case _                   => assert(false)
    }
    "keeps original offsets when the source uses CRLF line endings" in {
      val source = "# Title\r\n\r\nHello"
      Parser.parse(source) match
        case Result.Success(doc) =>
          assert(doc.span == Present(Span(0, source.length)))
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Heading(HeadingLevel.One, content, meta) =>
              assert(textOf(content) == "Title")
              assert(meta.span == Present(Span(0, "# Title".length)))
            case _ => assert(false)
          doc.children(1) match
            case MdcNode.Paragraph(content, MdcMeta(Present(span), _)) if textOf(content) == "Hello" =>
              assert(span.offset == source.indexOf("Hello"))
              assert(span.length == "Hello".length)
            case _ => assert(false)
        case _ => assert(false)
    }
    "keeps a lone carriage return as paragraph text in the original span" in {
      val source = "alpha\rbeta"

      assert(
        Parser.parse(source) == Result.succeed(
          MdcNode.Root(
            Chunk(
              MdcNode.Paragraph(
                Chunk(MdcNode.Text(source, MdcMeta.at(Span(0, source.length)))),
                MdcMeta.at(Span(0, source.length))
              )
            ),
            meta = MdcMeta.at(Span(0, source.length))
          )
        )
      )
    }
    "accepts an empty document" in {
      Parser.parse("") match
        case Result.Success(doc) => assert(doc.children.isEmpty)
        case _                   => assert(false)
    }
    "reads a fenced code block including blank lines inside the fence" in {
      val source = "```scala\nval x = 1\n\nval y = 2\n```"
      Parser.parse(source) match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Code(info, content, _) =>
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
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Code(_, content, _) =>
              assert(content == "value\n```\nafter\n")
            case _ => assert(false)
        case _ => assert(false)
    }
    // Four-space indentation puts the fence-like line beyond the ≤3-space limit a fence opener requires, so the
    // parser must not treat the backtick run as a fence at all. Under the merged AST that shows up as document shape
    // rather than as a node kind: a recognized fence here would be exactly one Code block holding "value\n"; instead
    // the backtick lines fall back to indented code and a lazily-continued paragraph, so there are two blocks and the
    // fence body itself never appears as a block's content.
    "does not recognize a fence indented by four spaces" in {
      Parser.parse("    ```\nvalue\n    ```") match
        case Result.Success(doc) =>
          assert(
            doc.children.size != 1 ||
              (doc.children.headOption match {
                case Some(MdcNode.Code(_, content, _)) => content != "value\n"
                case _                                 => true
              }),
            s"a four-space-indented backtick run was recognized as a fence: $doc"
          )
        case _ => assert(false)
    }
    "removes opening-fence indentation from fenced code content" in {
      val source = "   ```\n   value\n value\n```"
      Parser.parse(source) match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          blocks(0) match
            case MdcNode.Code(_, content, _) => assert(content == "value\nvalue\n")
            case _                           => assert(false)
        case _ => assert(false)
    }
    "requires a closing fence with matching marker, valid indentation, and no trailing text" in {
      val source = "~~~\nfirst\n```\n    ~~~\n~~~ language\nlast\n~~~~\t"
      Parser.parse(source) match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case MdcNode.Code(_, content, _) =>
              assert(content == "first\n```\n    ~~~\n~~~ language\nlast\n")
            case _ => assert(false)
        case _ => assert(false)
    }
    "does not accept non-space trailing characters on a closing fence" in {
      val source = "~~~\nfirst\n~~~\u000c\nlast\n~~~"
      Parser.parse(source) match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          blocks(0) match
            case MdcNode.Code(_, content, _) => assert(content == "first\n~~~\u000c\nlast\n")
            case _                           => assert(false)
        case _ => assert(false)
    }
    "trims only spaces and tabs around an info string" in {
      Parser.parse("~~~ \u000c example \u000c \nbody\n~~~") match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          blocks(0) match
            case MdcNode.Code(info, _, _) => assert(info.raw == "\u000c example \u000c")
            case _                        => assert(false)
        case _ => assert(false)
    }
    "allows a three-space opening and closing fence to interrupt a paragraph" in {
      Parser.parse("before\n   ```\ncode\n  ```\nafter") match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(blocks.size == 3)
          assert(blocks(0) ==
            MdcNode.Paragraph(Chunk(MdcNode.Text("before", MdcMeta.at(Span(0, 6)))), MdcMeta.at(Span(0, 6))))
          blocks(1) match
            case MdcNode.Code(_, content, _) => assert(content == "code\n")
            case _                           => assert(false)
          assert(blocks(2) ==
            MdcNode.Paragraph(Chunk(MdcNode.Text("after", MdcMeta.at(Span(25, 5)))), MdcMeta.at(Span(25, 5))))
        case _ => assert(false)
    }
    "uses end of document as the close of an unclosed fence" in {
      Parser.parse("```\ncode") match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case MdcNode.Code(_, content, _) => assert(content == "code")
            case _                           => assert(false)
        case _ => assert(false)
    }
    "allows an empty fenced code block" in {
      Parser.parse("```\n```") match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(blocks.size == 1)
          blocks(0) match
            case MdcNode.Code(_, content, _) => assert(content.isEmpty)
            case _                           => assert(false)
        case _ => assert(false)
    }
    "accepts tildes in a tilde-fence info string but not backticks in a backtick-fence info string" in {
      Parser.parse("~~~ aa ~~~ `example`\nbody\n~~~\n\n``` `example`\nbody") match
        case Result.Success(MdcNode.Root(blocks, _, _)) =>
          assert(blocks.size == 2)
          blocks(0) match
            case MdcNode.Code(info, content, _) =>
              assert(info.raw == "aa ~~~ `example`")
              assert(info.language == Present("aa"))
              assert(content == "body\n")
            case _ => assert(false)
          blocks(1) match
            // Not a fence: a backtick fence's info string may not contain a backtick, so the line is prose --
            // and its `example` is now an ordinary code span.
            case MdcNode.Paragraph(content, _) =>
              assert(content.size == 3)
              assert(content(0) == MdcNode.Text("``` ", MdcMeta.at(Span(31, 4))))
              assert(content(1) == MdcNode.InlineCode("example", MdcMeta.at(Span(35, 9))))
              assert(content(2) == MdcNode.Text("\nbody", MdcMeta.at(Span(44, 5))))
            case _ => assert(false)
        case _ => assert(false)
    }
    "reads consecutive unordered list items as one list" in {
      Parser.parse("- alpha\n- beta") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.List(false, Absent, _, items, _) =>
              assert(items.map(item => textOf(paragraphOf(item))) == Chunk("alpha", "beta"))
            case _ => assert(false)
        case _ => assert(false)
    }
    "reads a block quote as a container of blocks (spec example 228)" in {
      Parser.parse("> # Foo\n> bar\n> baz\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Blockquote(content, _) =>
              assert(content.size == 2)
              content(0) match
                case MdcNode.Heading(level, inner, _) =>
                  assert(level == HeadingLevel.One)
                  assert(textOf(inner) == "Foo")
                case _ => assert(false)
              content(1) match
                case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "bar\nbaz")
                case _                           => assert(false)
            case _ => assert(false)
        case _ => assert(false)
    }
    "keeps a quoted paragraph going when a line drops the marker (spec example 232)" in {
      Parser.parse("> # Foo\n> bar\nbaz\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Blockquote(content, _) =>
              assert(content.size == 2)
              content(1) match
                case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "bar\nbaz")
                case _                           => assert(false)
            case _ => assert(false)
        case _ => assert(false)
    }
    // A lazy line is prose and nothing else. `---` under a quoted paragraph is a thematic break outside the quote,
    // not a setext underline inside it, and getting that wrong silently swallows the break.
    "will not let a lazy line close a setext heading (spec example 234)" in {
      Parser.parse("> foo\n---\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Blockquote(content, _) =>
              content(0) match
                case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "foo")
                case _                           => assert(false)
            case _ => assert(false)
          doc.children(1) match
            case MdcNode.ThematicBreak(_) => assert(true)
            case _                        => assert(false)
        case _ => assert(false)
    }
    "reads a quote with no content as an empty container (spec example 239)" in {
      Parser.parse(">\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Blockquote(content, _) => assert(content.isEmpty)
            case _                              => assert(false)
        case _ => assert(false)
    }
    "splits a quote at a blank line and joins it at a bare marker (spec examples 242 and 244)" in {
      Parser.parse("> foo\n\n> bar\n") match
        case Result.Success(doc) => assert(doc.children.size == 2)
        case _                   => assert(false)

      Parser.parse("> foo\n>\n> bar\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Blockquote(content, _) => assert(content.size == 2)
            case _                              => assert(false)
        case _ => assert(false)
    }
    "lets a quote interrupt the paragraph above it (spec example 245)" in {
      Parser.parse("foo\n> bar\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2)
          doc.children(0) match
            case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "foo")
            case _                           => assert(false)
          doc.children(1) match
            case MdcNode.Blockquote(_, _) => assert(true)
            case _                        => assert(false)
        case _ => assert(false)
    }
    "nests quotes as deeply as the markers go, laziness included (spec example 250)" in {
      Parser.parse("> > > foo\nbar\n") match
        case Result.Success(doc) =>
          def onlyQuote(block: MdcNode.FlowContent): MdcNode.FlowContent =
            block match
              case MdcNode.Blockquote(content, _) =>
                assert(content.size == 1)
                content(0)
              case other => other

          val innermost = onlyQuote(onlyQuote(onlyQuote(doc.children(0))))
          innermost match
            case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "foo\nbar")
            case _                           => assert(false)
        case _ => assert(false)
    }
    // Stripping `> ` shortens the text, so a span taken from the remainder would point four characters early unless
    // the offset moves with it.
    "keeps inline spans pointing at the source through a quote marker" in {
      val source = "> alpha\n"
      Parser.parse(source) match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Blockquote(content, _) =>
              content(0) match
                case MdcNode.Paragraph(Chunk(MdcNode.Text(value, MdcMeta(Present(span), _))), _) =>
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
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "Foo\n---")
            case other                         => assert(false, s"expected a paragraph, got $other")
        case _ => assert(false)

      Parser.parse("> foo\n    - bar\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Blockquote(content, _) =>
              content(0) match
                case MdcNode.Paragraph(inner, _) => assert(textOf(inner) == "foo\n- bar")
                case other                       => assert(false, s"expected a paragraph, got $other")
            case other => assert(false, s"expected a block quote, got $other")
        case _ => assert(false)
    }
    "keeps a stripped continuation line's spans pointing at the source" in {
      val source = "alpha\n    `beta`\n"
      Parser.parse(source) match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Paragraph(content, _) =>
              content.collectFirst { case MdcNode.InlineCode(value, MdcMeta(Present(span), _)) => (value, span) } match
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
          doc.children(0) match
            case MdcNode.List(true, Present(_), spread, items, _) =>
              assert(items.size == 1)
              assert(spread, "blank lines between an item's blocks make the list loose")
              val content = items(0).children
              assert(content.size == 4)
              assert(content(0).isInstanceOf[MdcNode.Paragraph])
              assert(content(1).isInstanceOf[MdcNode.Code])
              assert(content(2).isInstanceOf[MdcNode.Paragraph])
              assert(content(3).isInstanceOf[MdcNode.Blockquote])
            case other => assert(false, s"expected an ordered list, got $other")
        case _ => assert(false)
    }
    // Two blocks in one item are not enough to make a list loose; a blank line between them is. Getting this wrong is
    // invisible in the AST and shows up only as `p` elements appearing or vanishing in the output.
    "calls a list tight unless a blank line separates blocks or items" in {
      def tightnessOf(source: String): Boolean =
        Parser.parse(source) match
          case Result.Success(doc) =>
            doc.children(0) match
              case MdcNode.List(_, _, spread, _, _) => !spread
              case other                            => throw new AssertionError(s"expected a list, got $other")
          case other => throw new AssertionError(s"parse failed: $other")

      assert(tightnessOf("- one\n- two\n"))
      assert(tightnessOf("- a\n  - b\n"), "a nested list is a second block, but no blank line separates them")
      assert(!tightnessOf("- one\n\n- two\n"), "a blank line between items")
      assert(!tightnessOf("* a\n*\n\n* c\n"), "a blank line after an empty item still separates it (spec example 315)")

      // A nested container eats the blank line at its end, because a blank matches its continuation prefix. The blank
      // still separated the outer item's blocks, so it has to come back out.
      assert(
        !tightnessOf("- a\n  - b\n  - c\n\n- d\n"),
        "the blank ended a nested list, and the item holding it has a next item (spec example 326)"
      )
      assert(
        !tightnessOf("* foo\n  * bar\n\n  baz\n"),
        "the blank ended a nested list, and another block follows it in the same item (spec example 325)"
      )
      assert(
        !tightnessOf("- a\n- b\n\n  [ref]: /url\n- d\n"),
        "a link reference definition is recorded, not rendered, so it does not clear the blank before it (example 317)"
      )

      // A quote is the container that cannot do this: its prefix needs a `>`, so what it swallows is `>` with nothing
      // after it -- blank content rather than a blank line.
      assert(
        tightnessOf("* a\n  > b\n  >\n* c\n"),
        "a bare `>` inside a quoted item does not loosen the list around it (spec example 320)"
      )
      assert(!tightnessOf("- one\n\n  two\n"), "a blank line between an item's blocks")
    }
    "nests a list inside the item that indents it" in {
      Parser.parse("- a\n  - b\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.List(false, Absent, _, items, _) =>
              assert(items.size == 1, "the indented marker belongs to the item above it, not beside it")
              assert(items(0).children.size == 2)
              items(0).children(1) match
                case MdcNode.List(false, Absent, _, inner, _) => assert(inner.size == 1)
                case other                                    => assert(false, s"expected a nested list, got $other")
            case other => assert(false, s"expected a list, got $other")
        case _ => assert(false)
    }
    // The lazy rule is stricter than the one for a line that kept its markers: `2.` may not interrupt a paragraph, but
    // the paragraph is not what this line fell out of. Before this was separated out, the second item was swallowed
    // into the first item's paragraph.
    "starts a new item on a marker that drops out of the item above (spec example 302)" in {
      Parser.parse("1. one\n2. two\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.List(true, start, _, items, _) =>
              assert(start == Present(1))
              assert(items.size == 2)
            case other => assert(false, s"expected an ordered list, got $other")
        case _ => assert(false)
    }
    "reads an item with nothing after its marker, and stops at the second blank (spec example 280)" in {
      Parser.parse("-\n\n  foo\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2, "a list item may begin with at most one blank line")
          doc.children(0) match
            case MdcNode.List(false, Absent, _, items, _) =>
              assert(items.size == 1)
              assert(items(0).children.isEmpty)
            case other => assert(false, s"expected a list, got $other")
          assert(doc.children(1).isInstanceOf[MdcNode.Paragraph])
        case _ => assert(false)
    }
    // Four spaces past the marker is code inside the item, not a very indented paragraph: the item spends one space
    // and the rest is content.
    "gives an item's over-indented content to a code block (spec example 270)" in {
      Parser.parse("- foo\n\n      bar\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.List(false, Absent, _, items, _) =>
              items(0).children(1) match
                case MdcNode.Code(info, content, _) =>
                  assert(info == FenceInfo.empty)
                  assert(content == "bar\n")
                case other => assert(false, s"expected indented code, got $other")
            case other => assert(false, s"expected a list, got $other")
        case _ => assert(false)
    }
    // Every cursor that could claim a line looks at it: the document's, the item's once the marker is recognised, and
    // the list's when it asks whether another item begins there. They differ only in which prefixes they strip, so the
    // scan behind them is shared. Before it was, a run of list items cost about four scans a line, which was most of
    // what parsing them cost at all -- and work units measure that without a clock, so this is worth asserting.
    "scans each line once however many containers look at it" in {
      val plain = "alpha beta gamma\n" * 200
      val items = "- alpha beta gamma\n" * 200
      assert(plain.length + 400 == items.length, "the two fixtures must differ only by their markers")

      val plainWork = parseMetrics(plain).work.toLong
      val itemWork  = parseMetrics(items).work.toLong
      assert(
        itemWork < plainWork * 2,
        s"a list costs $itemWork against $plainWork for the same prose: the line scan is not being shared"
      )
    }
    // A tab advances to the next four-column stop, so what it counts for depends on where it sits. These are the four
    // places that ask, and each one used to count the tab as a single character.
    "measures a tab in columns wherever indentation is counted" in {
      def blockOf(source: String): MdcNode.FlowContent =
        Parser.parse(source) match
          case Result.Success(document) => document.children(0)
          case other                    => throw new AssertionError(s"parse failed: $other")

      def codeIn(block: MdcNode.FlowContent): String =
        block match
          case MdcNode.Code(_, content, _) => content
          case other                       => throw new AssertionError(s"expected indented code, got $other")

      // One leading tab is four columns, so it opens a code block -- and the tabs inside the body are content, left
      // exactly as written (spec example 1).
      assert(codeIn(blockOf("\tfoo\tbaz\t\tbim\n")) == "foo\tbaz\t\tbim\n")
      // Two spaces then a tab is four as well: the tab finishes the stop rather than adding four of its own (2).
      assert(codeIn(blockOf("  \tfoo\n")) == "foo\n")
      // The quote marker takes one column of the tab that follows it, leaving six -- four for the code block, two
      // over (6).
      blockOf(">\t\tfoo\n") match
        case MdcNode.Blockquote(content, _) => assert(codeIn(content(0)) == "  foo\n")
        case other                          => assert(false, s"expected a block quote, got $other")
      // The same for a bullet: more than four columns after the marker means the item spends one and holds code (7).
      blockOf("-\t\tfoo\n") match
        case MdcNode.List(false, Absent, _, items, _) => assert(codeIn(items(0).children(0)) == "  foo\n")
        case other                                    => assert(false, s"expected a list, got $other")
      // A tab separates the hashes of a heading from its text as well as a space does (10).
      blockOf("#\tFoo\n") match
        case MdcNode.Heading(level, content, _) =>
          assert(level == HeadingLevel.One)
          assert(textOf(content) == "Foo")
        case other => assert(false, s"expected a heading, got $other")
    }
    // A tab-expanded line counts columns while the source counts characters. When a list item compared the two it
    // never recognised its own marker line, gathered nothing, and read that same line for ever -- and because nothing
    // was read there was no work for the scan budget to notice.
    "keeps a tab-indented marker in the same coordinates as the source (spec example 9)" in {
      Parser.parse(" - foo\n   - bar\n\t - baz\n") match
        case Result.Success(doc) =>
          def onlyItem(block: MdcNode.FlowContent): MdcNode.ListItem =
            block match
              case MdcNode.List(false, Absent, _, items, _) =>
                assert(items.size == 1)
                items(0)
              case other => throw new AssertionError(s"expected a list, got $other")

          val second = onlyItem(doc.children(0)).children(1)
          val third  = onlyItem(second).children(1)
          third match
            case MdcNode.List(false, Absent, _, items, _) =>
              assert(items.size == 1, "the tab-indented marker belongs to the item above it")
            case other => assert(false, s"expected a third-level list, got $other")
        case other => assert(false, s"parse failed: $other")
    }
    "keeps a span true through a tab-indented continuation line" in {
      val source = "alpha\n\t`beta`\n"
      Parser.parse(source) match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Paragraph(content, _) =>
              content.collectFirst { case MdcNode.InlineCode(_, MdcMeta(Present(span), _)) => span } match
                case Some(span) => assert(source.substring(span.offset, span.end) == "`beta`")
                case None       => assert(false, "expected a code span on the continuation line")
            case other => assert(false, s"expected a paragraph, got $other")
        case _ => assert(false)
    }
    "leaves a tab that is content alone" in {
      // `1.5` is not a list marker, so nothing on this line is structural and the tab stays a tab.
      Parser.parse("1.5\tfoo\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "1.5\tfoo")
            case other                         => assert(false, s"expected a paragraph, got $other")
        case _ => assert(false)
    }
    // A fence whose first line is blank writes nothing, which used to look the same as having written no line at all,
    // so the next line joined it and the blank vanished.
    "keeps a blank first line inside a fence (spec examples 127 and 129)" in {
      def codeOf(source: String): String =
        Parser.parse(source) match
          case Result.Success(doc) =>
            doc.children(0) match
              case MdcNode.Code(_, content, _) => content
              case other                       => throw new AssertionError(s"expected a fence, got $other")
          case other => throw new AssertionError(s"parse failed: $other")

      // Five backticks are not closed by three, so the rest of the input is content -- blank first line included.
      assert(codeOf("`````\n\n```\naaa\n") == "\n```\naaa\n")
      assert(codeOf("```\n\n  \n```\n") == "\n  \n")
    }
    // The info string is prose the author wrote rather than a token, so it resolves escapes and references the way a
    // destination or a title does (spec examples 24 and 34).
    "resolves escapes and references in a fence's info string" in {
      def languageOf(source: String): String =
        Parser.parse(source) match
          case Result.Success(doc) =>
            doc.children(0) match
              case MdcNode.Code(info, _, _) => info.language.getOrElse("")
              case other                    => throw new AssertionError(s"expected a fence, got $other")
          case other => throw new AssertionError(s"parse failed: $other")

      assert(languageOf("``` foo\\+bar\nfoo\n```\n") == "foo+bar")
      assert(languageOf("``` f&ouml;&ouml;\nfoo\n```\n") == "föö")
    }
    // A title has to be separated from the destination by whitespace. Without that rule `(baz)` reads as a
    // parenthesised title and the line defines a link, where the spec leaves the whole of it as prose (example 201).
    "will not take a title that runs into the destination" in {
      Parser.parse("[foo]: <bar>(baz)\n\n[foo]\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 2, "nothing was defined, so both lines stay paragraphs")
        case other => assert(false, s"parse failed: $other")
    }
    // A paragraph of nothing but definitions has no content for a setext underline to promote, so the underline goes
    // back and begins a paragraph of its own (example 216).
    "gives a definition-only paragraph its setext line back" in {
      Parser.parse("[foo]: /url\n===\n[foo]\n") match
        case Result.Success(doc) =>
          assert(doc.children.size == 1)
          doc.children(0) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content).startsWith("==="))
            case other                         => assert(false, s"expected a paragraph, got $other")
        case other => assert(false, s"parse failed: $other")
    }
    // A line of exactly four spaces is both blank and indented, and it is the blankness that decides: it belongs to
    // the block only if indented content follows it (example 117).
    "holds back a blank line at either end of indented code" in {
      Parser.parse("\n    \n    foo\n    \n\n") match
        case Result.Success(doc) =>
          doc.children(0) match
            case MdcNode.Code(info, content, _) =>
              assert(info == FenceInfo.empty)
              assert(content == "foo\n")
            case other => assert(false, s"expected indented code, got $other")
        case other => assert(false, s"parse failed: $other")
    }
    "reads a thematic break between paragraphs" in {
      Parser.parse("Hello\n\n---\n\nWorld") match
        case Result.Success(doc) =>
          assert(doc.children.size == 3)
          doc.children(1) match
            case MdcNode.ThematicBreak(_) => assert(true)
            case _                        => assert(false)
          doc.children(0) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "Hello")
            case _                             => assert(false)
          doc.children(2) match
            case MdcNode.Paragraph(content, _) => assert(textOf(content) == "World")
            case _                             => assert(false)
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
