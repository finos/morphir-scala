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
      given MdStyle   = MdStyle()
      val source      = "- [x] foo\n- [ ] bar\n"
      val document    = CstParser.parse(source)
      val errors      = Cst.tilingErrors(document, source.length)
      assert(errors.isEmpty, s"the checkbox marker broke tiling: ${errors.mkString("; ")}")
      assert(Cst.print(document) == source)
      val direct = Parser.parse(source).getOrThrow
      assert(Lower.lower(document).unpositioned == direct.unpositioned)
      val written = MdWriter.write(direct)
      assert(Parser.parse(written).getOrThrow.unpositioned == direct.unpositioned, s"round trip changed: $written")
    }
  }
