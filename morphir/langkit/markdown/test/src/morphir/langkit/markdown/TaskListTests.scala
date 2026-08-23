package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{Cst, CstParser, Lower, MdWriter, Parser}

/**
 * Task list items: a bracketed checkbox at the head of a list item's first paragraph.
 *
 * The marker counts only in that position — `[ ]` anywhere else is ordinary text, and CommonMark already reads it as a
 * shortcut reference that resolves to nothing. Recognition happens where the item is built, so the CST records the
 * marker as a token rather than as prose.
 */
class TaskListTests extends Test[Any]:

  private def items(source: String)(using MdProfile): Chunk[MdNode.ListItem] =
    Parser.parse(source).getOrThrow.children.head match
      case MdNode.List(_, _, _, children, _) => children
      case other                             => throw new AssertionError(s"expected a list, got $other")

  /** The literal text of an item's first block, which is where the marker is stripped from. */
  private def textOf(item: MdNode.ListItem): String =
    item.children.headOption.map(_.childNodes.map(_.literal.getOrElse("")).mkString).getOrElse("")

  /**
   * The three assertions that decide whether a marker's span is the right one.
   *
   * Tiling catches a span that misowns a byte, printing catches one that loses or duplicates source, and lowering
   * catches one that hands the concrete tree different prose than the direct parse built. A span can satisfy any two of
   * them and still be wrong, so all three run together.
   */
  private def agrees(source: String)(using MdProfile, AssertScope): Unit =
    given MdStyle = MdStyle()
    val document  = CstParser.parse(source)
    val errors    = Cst.tilingErrors(document, source.length)
    val printed   = Cst.print(document)
    assert(errors.isEmpty, s"the checkbox marker broke tiling: ${errors.mkString("; ")}")
    assert(printed == source, s"the concrete syntax tree did not reproduce the source: $printed")
    val direct = Parser.parse(source).getOrThrow
    assert(
      Lower.lower(document).unpositioned == direct.unpositioned,
      "lowering the concrete tree disagrees with the direct parse"
    )
    val written = MdWriter.write(direct)
    assert(Parser.parse(written).getOrThrow.unpositioned == direct.unpositioned, s"round trip changed: $written")

  "task list items" - {

    "read an unchecked and a checked marker (spec example 279)" in {
      given MdProfile = MdProfile.gfm
      val parsed      = items("- [ ] foo\n- [x] bar\n")
      assert(parsed.map(_.checked) == Chunk(Present(false), Present(true)))
    }

    "accept an upper-case X" in {
      given MdProfile = MdProfile.gfm
      assert(items("- [X] foo\n").head.checked == Present(true))
    }

    "leave the rest of the item's prose alone" in {
      given MdProfile = MdProfile.gfm
      val paragraph   = items("- [x] foo bar\n").head.children.head
      assert(paragraph == MdNode.Paragraph(Chunk(MdNode.Text("foo bar", MdMeta.empty))).unpositioned ||
        paragraph.childNodes.map(_.literal.getOrElse("")).mkString == "foo bar")
    }

    "are off under the CommonMark profile" in {
      given MdProfile = MdProfile.commonmark
      val parsed      = items("- [ ] foo\n")
      assert(parsed.head.checked == Absent)
      assert(parsed.head.children.head.childNodes.map(_.literal.getOrElse("")).mkString == "[ ] foo")
    }

    "need a marker at the very start of the first paragraph" in {
      given MdProfile = MdProfile.gfm
      assert(items("- foo [ ] bar\n").head.checked == Absent)
    }

    "round-trip through the CST and the Markdown writer" in {
      given MdProfile = MdProfile.gfm
      agrees("- [x] foo\n- [ ] bar\n")
    }

    /**
     * The checkbox's span sits inside the item's first line, ahead of the continuation line's own indentation marker —
     * see `Cst.graduate`'s `ListItem` case, which sorts the two back into offset order rather than appending the
     * checkbox after markers a plain append would already have collected. A multi-line item is what exercises that
     * sort: a one-line item collects no continuation-line marker to sort against, so this is the shape a broken sort
     * would first miss.
     */
    "keeps a checkbox marker sorted ahead of a continuation line's own indentation marker" in {
      given MdProfile = MdProfile.gfm
      val source      = "- [x] first line\n  continuation line\n"
      agrees(source)
      assert(items(source).head.checked == Present(true))
    }
  }

  /**
   * The specification's whitespace class is six characters — space, tab, newline, line tabulation, form feed and
   * carriage return — and it governs both the character between the brackets and the one that must follow the closing
   * bracket "before any other content". Reading only a space, or only a space and a tab, is what these pin.
   *
   * The newline is the interesting member. A paragraph's text is its lines joined with a single `\n`, so a marker at
   * the end of the item's first line reads as `[x]\n…` however the source ended that line. The marker's *span* cannot
   * be counted off that joined text: on a CRLF document the one `\n` stands for two source characters.
   */
  "the whitespace around the marker" - {

    "takes a line ending as the whitespace after the closing bracket" in {
      given MdProfile = MdProfile.gfm
      val source      = "- [x]\n  continued\n"
      val item        = items(source).head
      assert(item.checked == Present(true))
      assert(textOf(item) == "continued", textOf(item))
      agrees(source)
    }

    "measures a CRLF line ending against the source rather than the joined text" in {
      given MdProfile = MdProfile.gfm
      val source      = "- [x]\r\n  continued\r\n"
      val item        = items(source).head
      assert(item.checked == Present(true))
      assert(textOf(item) == "continued", textOf(item))
      agrees(source)
    }

    /**
     * An unindented continuation line is contiguous with the line ending above it, so the inline phase hands the marker
     * back inside a text node that runs well past it. The span has to be measured against the source there too — this
     * is the case a length counted off the joined text gets wrong without leaving a gap for tiling to notice.
     */
    "measures a CRLF line ending inside a longer text node" in {
      given MdProfile = MdProfile.gfm
      val source      = "- [x]\r\ncontinued\r\n"
      val item        = items(source).head
      assert(item.checked == Present(true))
      assert(textOf(item) == "continued", textOf(item))
      agrees(source)
    }

    "reads a tab between the brackets as unchecked" in {
      given MdProfile = MdProfile.gfm
      val source      = "- [\t] foo\n"
      val item        = items(source).head
      assert(item.checked == Present(false))
      assert(textOf(item) == "foo", textOf(item))
      agrees(source)
    }

    "leaves an item with nothing after the closing bracket an ordinary item" in {
      given MdProfile = MdProfile.gfm
      val parsed      = items("- [x]\n")
      assert(parsed.head.checked == Absent)
      assert(textOf(parsed.head) == "[x]", textOf(parsed.head))
    }
  }
