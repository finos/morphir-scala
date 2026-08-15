package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span

class ParserTests extends Test[Any]:

  "Parser.parse" - {
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
              assert(info == "scala")
              assert(content == "val x = 1\n\nval y = 2\n")
            case _ => assert(false)
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
