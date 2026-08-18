package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.core.scanner.*

class ParserTests extends Test[Any]:

  "Parser.parse" - {
    "maps an input-size ceiling to an exact typed scanner failure" in {
      val budget = ScanBudget.limited(
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
              phase = Some(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "reports aggregate output exhaustion at the temporary scanner start offset" in {
      val budget = ScanBudget.limited(
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
              // Task 5 parses remaining.text without advancing; Task 6 moves this to the scanner-backed parser.
              offset = SourceOffset.start,
              phase = Some(ScanPhase("markdown.blocks"))
            )
          )
        case _ => assert(false)
    }
    "accepts the exact aggregate output ceiling and preserves the default result" in {
      val budget = ScanBudget.limited(
        maxInputLength = InputSize.codeUnits(100L),
        maxWork = WorkUnits(100L),
        maxNestingDepth = NestingDepth(10),
        maxOutputNodes = NodeCount(2L)
      )

      assert(Parser.parse("# Title", budget) == Parser.parse("# Title"))
    }
    "accepts an explicitly unsafe unbounded budget" in {
      Parser.parse("# Title", ScanBudget.UnsafeUnbounded) match
        case Result.Success(Document(blocks, _)) =>
          assert(blocks == Chunk(Block.Heading(1, "Title", Span(0, 7))))
        case _ => assert(false)
    }
    "reads an ATX heading and a paragraph" in {
      Parser.parse("# Title\n\nHello") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(level, text, _) =>
              assert(level == 1)
              assert(text == "Title")
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph(text, _) => assert(text == "Hello")
            case _                        => assert(false)
        case _ => assert(false)
    }
    "splits a heading from the next block at a single newline" in {
      Parser.parse("# Title\nBody") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(level, text, _) =>
              assert(level == 1)
              assert(text == "Title")
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph(text, _) => assert(text == "Body")
            case _                        => assert(false)
        case _ => assert(false)
    }
    "splits consecutive headings without a blank line" in {
      Parser.parse("# One\n## Two") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 2)
          doc.blocks(0) match
            case Block.Heading(1, "One", _) => ()
            case _                          => assert(false)
          doc.blocks(1) match
            case Block.Heading(2, "Two", _) => ()
            case _                          => assert(false)
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
            case Block.Heading(1, "Title", span) =>
              assert(span == Span(0, "# Title".length))
            case _ => assert(false)
          doc.blocks(1) match
            case Block.Paragraph("Hello", span) =>
              assert(span.offset == source.indexOf("Hello"))
              assert(span.length == "Hello".length)
            case _ => assert(false)
        case _ => assert(false)
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
          assert(blocks(0) == Block.Paragraph("before", Span(0, 6)))
          blocks(1) match
            case Block.FencedCode(_, content, _) => assert(content == "code\n")
            case _                               => assert(false)
          assert(blocks(2) == Block.Paragraph("after", Span(25, 5)))
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
            case Block.Paragraph(text, _) => assert(text == "``` `example`\nbody")
            case _                        => assert(false)
        case _ => assert(false)
    }
    "reads consecutive unordered list items as one list" in {
      Parser.parse("- alpha\n- beta") match
        case Result.Success(doc) =>
          assert(doc.blocks.size == 1)
          doc.blocks(0) match
            case Block.UnorderedList(items, _) =>
              assert(items == Chunk("alpha", "beta"))
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
            case Block.Paragraph(text, _) => assert(text == "Hello")
            case _                        => assert(false)
          doc.blocks(2) match
            case Block.Paragraph(text, _) => assert(text == "World")
            case _                        => assert(false)
        case _ => assert(false)
    }
  }

  "ParseError" - {
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
          phase = Some(ScanPhase("markdown.blocks"))
        )
      )

      assert(error.isInstanceOf[Exception])
      assert(error.getMessage == "Markdown scan failed at offset 0 during markdown.blocks: InputLength(4,5)")
      assert(ParseError.unapply(error).contains(error.getMessage))
    }
  }
