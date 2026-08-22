package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.internal.Parser

/**
 * Strikethrough, GFM's sixth emphasis type.
 *
 * The 0.29-gfm specification wraps struck text in exactly two tildes. One tilde and three-or-more are literal, and a
 * run cannot cross a paragraph boundary — the delimiter machinery emphasis already uses gives that last property for
 * free, which is why this rides it rather than getting a scanner of its own.
 */
class StrikethroughTests extends Test[Any]:

  private def inlines(source: String)(using MdProfile): Chunk[MdNode.PhrasingContent] =
    Parser.parse(source).getOrThrow.children.head match
      case MdNode.Paragraph(content, _) => content
      case other                        => throw new AssertionError(s"expected a paragraph, got $other")

  private def textOf(content: Chunk[MdNode.PhrasingContent]): String =
    content.map {
      case MdNode.Text(value, _)   => value
      case MdNode.Delete(inner, _) => textOf(inner)
      case other                   => other.literal.getOrElse("")
    }.mkString

  /**
   * Adjacent `Text` nodes merged back into one, the same normalization [[MdWriterTests]] applies before a structural
   * comparison: a parse splits prose at every escape and every entity, so a round trip through the writer's `&#32;`
   * escaping can hand back two `Text` nodes where the direct parse produced one, without the meaning having changed at
   * all. Scoped to the shapes this suite's trees actually take — `Root`, `Paragraph`, `Delete` — rather than
   * duplicating `MdWriterTests`' full per-case walk.
   */
  private def normalize(node: MdNode): MdNode = node match
    case MdNode.Root(children, frontmatter, meta) =>
      MdNode.Root(children.map(child => normalize(child).asInstanceOf[MdNode.FlowContent]), frontmatter, meta)
    case MdNode.Paragraph(children, meta) => MdNode.Paragraph(mergedTexts(children), meta)
    case leaf                             => leaf

  private def normalizePhrasing(node: MdNode.PhrasingContent): MdNode.PhrasingContent = node match
    case MdNode.Delete(children, meta) => MdNode.Delete(mergedTexts(children), meta)
    case leaf                          => leaf

  private def mergedTexts(nodes: Chunk[MdNode.PhrasingContent]): Chunk[MdNode.PhrasingContent] =
    val out = scala.collection.mutable.ListBuffer.empty[MdNode.PhrasingContent]
    nodes.foreach { node =>
      (out.lastOption, normalizePhrasing(node)) match
        case (Some(MdNode.Text(before, meta)), MdNode.Text(after, _)) =>
          out.remove(out.size - 1)
          out += MdNode.Text(before + after, meta)
        case (_, normalized) => out += normalized
    }
    Chunk.from(out.toList)

  "strikethrough" - {

    "wraps text in two tildes (spec example 491)" in {
      given MdProfile = MdProfile.gfm
      val content     = inlines("~~Hi~~ Hello, world!\n")
      assert(
        content.head.unpositioned == MdNode.Delete(Chunk(MdNode.Text("Hi"))),
        s"expected a Delete holding \"Hi\", got ${content.head}"
      )
      assert(textOf(content) == "Hi Hello, world!")
    }

    "is off under the CommonMark profile" in {
      given MdProfile = MdProfile.commonmark
      assert(inlines("~~Hi~~ Hello, world!\n").forall(node => !node.isInstanceOf[MdNode.Delete]))
      assert(textOf(inlines("~~Hi~~ Hello, world!\n")) == "~~Hi~~ Hello, world!")
    }

    "does not cross a paragraph boundary (spec example 492)" in {
      given MdProfile = MdProfile.gfm
      val root        = Parser.parse("This ~~has a\n\nnew paragraph~~.\n").getOrThrow
      assert(root.children.size == 2)
      assert(root.children.forall {
        case MdNode.Paragraph(content, _) => content.forall(node => !node.isInstanceOf[MdNode.Delete])
        case _                            => false
      })
    }

    "leaves a single tilde literal" in {
      given MdProfile = MdProfile.gfm
      assert(textOf(inlines("~Hi~ there\n")) == "~Hi~ there")
      assert(inlines("~Hi~ there\n").forall(node => !node.isInstanceOf[MdNode.Delete]))
    }

    "leaves a run of three tildes literal" in {
      given MdProfile = MdProfile.gfm
      // A leading "~~~" would open a fenced code block (CommonMark's fence marker is `` ` `` or `~`), which is a
      // block-level rule with nothing to do with inline strikethrough, so the run sits mid-line here instead.
      val content = inlines("a ~~~Hi~~~ there\n")
      assert(textOf(content) == "a ~~~Hi~~~ there")
      assert(content.forall(node => !node.isInstanceOf[MdNode.Delete]))
    }

    "round-trips through the CST and the Markdown writer" in {
      given MdProfile = MdProfile.gfm
      given MdStyle   = MdStyle()
      val source      = "a ~~b~~ c\n"
      val direct      = Parser.parse(source).getOrThrow
      assert(normalize(morphir.langkit.markdown.internal.Lower.lower(
        morphir.langkit.markdown.internal.CstParser.parse(source)
      ).unpositioned) == normalize(direct.unpositioned))
      val written = morphir.langkit.markdown.internal.MdWriter.write(direct)
      assert(
        normalize(Parser.parse(written).getOrThrow.unpositioned) == normalize(direct.unpositioned),
        s"round trip changed: $written"
      )
    }
  }
