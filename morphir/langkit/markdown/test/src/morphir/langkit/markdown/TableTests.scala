package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.{Cst, CstParser, Lower, MdWriter, Parser}

/**
 * GFM pipe tables.
 *
 * A header row, a delimiter row of dashes with optional colons fixing each column's alignment, and body rows. Rows
 * shorter than the header are padded with empty cells and longer ones are truncated, so the table stays rectangular
 * whatever the source did. Cell content is inline content, and a backslash-escaped pipe does not split a cell.
 *
 * Recognition happens while a paragraph is open, the way a setext underline is recognized: the delimiter row is what
 * says the line above it was a header. A table therefore cannot interrupt a paragraph that already has content.
 */
class TableTests extends Test[Any]:

  private def tableOf(source: String)(using MdProfile): MdNode.Table =
    Parser.parse(source).getOrThrow.children.head match
      case table: MdNode.Table => table
      case other               => throw new AssertionError(s"expected a table, got $other")

  private def cellText(cell: MdNode.TableCell): String =
    cell.children.map(node => node.literal.getOrElse(node.childNodes.map(_.literal.getOrElse("")).mkString)).mkString

  private def rowText(row: MdNode.TableRow): Chunk[String] = row.children.map(cellText)

  "pipe tables" - {

    "read a header and a body row (spec example 198)" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| foo | bar |\n| --- | --- |\n| baz | bim |\n")
      assert(rowText(table.header) == Chunk("foo", "bar"))
      assert(table.rows.map(rowText) == Chunk(Chunk("baz", "bim")))
      assert(table.align == Chunk(Absent, Absent))
    }

    "read alignment from the delimiter row, with no outer pipes (spec example 199)" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| abc | defghi |\n:-: | -----------:\nbar | baz\n")
      assert(table.align == Chunk(Present(ColumnAlignment.Center), Present(ColumnAlignment.Right)))
    }

    "do not split a cell on an escaped pipe (spec example 200)" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| f\\|oo  |\n| ------ |\n| b `\\|` az |\n")
      assert(rowText(table.header) == Chunk("f|oo"))
    }

    "treat a trailing escaped pipe as content rather than a closing delimiter" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| a \\| b |\n| --- |\n")
      assert(rowText(table.header) == Chunk("a | b"))
    }

    "do not open on a delimiter row that dropped its container's marker" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("> | abc | def |\n| --- | --- |\n").getOrThrow
      assert(root.children.size == 1)
      root.children.head match
        case MdNode.Blockquote(children, _) => assert(children.head.isInstanceOf[MdNode.Paragraph])
        case other                          => throw new AssertionError(s"expected a blockquote, got $other")
    }

    "pad a short row and truncate a long one (spec example 204)" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| abc | def |\n| --- | --- |\n| bar |\n| bar | baz | boo |\n")
      assert(table.rows.map(rowText) == Chunk(Chunk("bar", ""), Chunk("bar", "baz")))
    }

    "allow a header with no body rows (spec example 205)" in {
      given MdProfile = MdProfile.gfm
      val table       = tableOf("| abc | def |\n| --- | --- |\n")
      assert(table.rows.isEmpty)
    }

    "end at a line that opens another block (spec example 201)" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("| abc | def |\n| --- | --- |\n| bar | baz |\n> bar\n").getOrThrow
      assert(root.children.size == 2)
      assert(root.children.head.isInstanceOf[MdNode.Table])
      assert(root.children(1).isInstanceOf[MdNode.Blockquote])
    }

    "end at a blank line, leaving what follows its own block (spec example 202)" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("| abc | def |\n| --- | --- |\n| bar | baz |\nbar\n\nbar\n").getOrThrow
      assert(root.children.size == 2)
      root.children.head match
        case table: MdNode.Table => assert(table.rows.map(rowText) == Chunk(Chunk("bar", "baz"), Chunk("bar", "")))
        case other               => throw new AssertionError(s"expected a table, got $other")
      assert(root.children(1).isInstanceOf[MdNode.Paragraph])
    }

    "need the delimiter row's cell count to match the header's" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("| abc | def |\n| --- |\n").getOrThrow
      assert(root.children.head.isInstanceOf[MdNode.Paragraph])
    }

    "do not interrupt a paragraph that already has content" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("lead\n| abc | def |\n| --- | --- |\n").getOrThrow
      assert(root.children.size == 1)
      assert(root.children.head.isInstanceOf[MdNode.Paragraph])
    }

    /**
     * The negative case, under the GFM profile, which is the only place it can be measured.
     *
     * The conformance suite scopes each example to the extensions its own fence names, so every untagged example in
     * both fixture files parses with tables off — and `CstRoundTripTests` takes the CommonMark default. `652/652`
     * therefore says nothing about whether the delimiter-row hook stays quiet under a profile that has tables on. It
     * rests on the guards inside `gather` and on the order of the branches there, and that is what these cases pin.
     */
    "leave a paragraph alone" - {

      /**
       * Doubly guarded, and worth a test for exactly that reason: the setext branch matches first, and `---` classifies
       * as a thematic break rather than as text, which the hook's own guard rejects. Neither guard is visible at the
       * other's site, so a refactor that moved the table check above the setext branch would pass every other case in
       * this file. This is also the commonest two-line shape in a real GFM document.
       */
      "a setext underline stays a heading" in {
        given MdProfile = MdProfile.gfm
        val root        = Parser.parse("foo\n---\n").getOrThrow
        assert(root.children.size == 1)
        root.children.head match
          case MdNode.Heading(level, _, _) => assert(level == HeadingLevel.Two)
          case other                       => throw new AssertionError(s"expected a setext heading, got $other")
      }

      "a second line of ordinary text stays paragraph content" in {
        given MdProfile = MdProfile.gfm
        val root        = Parser.parse("foo\nbar\n").getOrThrow
        assert(root.children.size == 1)
        assert(root.children.head.isInstanceOf[MdNode.Paragraph])
      }

      "a second line that opens another block opens it" in {
        given MdProfile = MdProfile.gfm
        Chunk(
          "foo\n```\ncode\n```\n" -> classOf[MdNode.Code],
          "foo\n- item\n"         -> classOf[MdNode.List],
          "foo\n# heading\n"      -> classOf[MdNode.Heading],
          "foo\n> quoted\n"       -> classOf[MdNode.Blockquote]
        ).foreach { case (source, expected) =>
          val root  = Parser.parse(source).getOrThrow
          val shown = source.replace("\n", "\\n")
          assert(root.children.size == 2, s"$shown produced ${root.children.size} blocks")
          assert(root.children.head.isInstanceOf[MdNode.Paragraph], s"$shown lost its paragraph")
          assert(expected.isInstance(root.children(1)), s"$shown did not open a ${expected.getSimpleName}")
        }
      }
    }

    "are off under the CommonMark profile" in {
      given MdProfile = MdProfile.commonmark
      val root        = Parser.parse("| foo | bar |\n| --- | --- |\n| baz | bim |\n").getOrThrow
      assert(root.children.head.isInstanceOf[MdNode.Paragraph])
    }

    "agree between a direct parse and a lowered CST" in {
      given MdProfile = MdProfile.gfm
      val source      = "| foo | bar |\n| :-- | --: |\n| baz | bim |\n"
      assert(Lower.lower(CstParser.parse(source)).unpositioned == Parser.parse(source).getOrThrow.unpositioned)
    }

    "tile the source with no gaps" in {
      given MdProfile = MdProfile.gfm
      Chunk(
        "| foo | bar |\n| :-- | --: |\n| baz | bim |\n",
        "abc | defghi\n:-: | -----------:\nbar | baz\n",
        "> | foo | bar |\n> | --- | --- |\n> | baz | bim |\n",
        "- | foo | bar |\n  | --- | --- |\n  | baz | bim |\n",
        "|\tfoo\t|\tbar |\n| --- | --- |\n"
      ).foreach { source =>
        val tree   = CstParser.parse(source)
        val errors = Cst.tilingErrors(tree, source.length)
        assert(errors.isEmpty, s"${source.replace("\n", "\\n")} does not tile: ${errors.mkString("; ")}")
        assert(Cst.print(tree) == source, s"${source.replace("\n", "\\n")} does not reprint")
      }
    }

    "round-trip through the Markdown writer" in {
      given MdProfile = MdProfile.gfm
      given MdStyle   = MdStyle()
      Chunk(
        "| foo | bar |\n| --- | --- |\n| baz | bim |\n",
        "| abc | defghi |\n| :-: | ----: |\n| bar | baz |\n",
        "| abc | def |\n| --- | --- |\n"
      ).foreach { source =>
        val direct  = Parser.parse(source).getOrThrow
        val written = MdWriter.write(direct)
        assert(
          Parser.parse(written).getOrThrow.unpositioned == direct.unpositioned,
          s"round trip changed ${source.replace("\n", "\\n")} to ${written.replace("\n", "\\n")}"
        )
      }
    }
  }
