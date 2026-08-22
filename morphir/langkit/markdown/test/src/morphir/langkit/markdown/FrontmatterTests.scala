package morphir.langkit.markdown

import kyo.*
import kyo.test.*
import morphir.langkit.core.Span
import morphir.langkit.markdown.internal.{Cst, CstParser, Parser}

/**
 * Frontmatter recognition, which is opt-in: what an [[MdProfile]] carrying [[FrontMatterKind.Yaml]] claims, and what
 * plain CommonMark keeps.
 *
 * The negative cases matter as much as the positive ones. `---` is CommonMark syntax — a thematic break at the top of a
 * block, a setext underline beneath a paragraph — so a profile that recognizes nothing must parse a document opening
 * with `---` exactly as it always did, and a recognized block that never closes must fall all the way back to that same
 * parse rather than to a half-claimed one.
 */
class FrontmatterTests extends Test[Any]:

  private given yamlProfile: MdProfile = MdProfile.commonmark.withYamlFrontmatter

  private def rootOf(source: String)(using MdProfile): MdNode.Root =
    Parser.parse(source) match
      case Result.Success(root) => root
      case other                => throw new AssertionError(s"parse of ${oneLine(source)} failed: $other")

  private def yamlOf(root: MdNode.Root): Maybe[MdNode.FrontMatter.Yaml] =
    root.frontmatter.map { case yaml: MdNode.FrontMatter.Yaml => yaml }

  /** Newlines shown as `\n` so a mismatch prints on one line. */
  private def oneLine(text: String): String = text.replace("\r", "\\r").replace("\n", "\\n")

  private def roundTrips(source: String)(using MdProfile)(using AssertScope): Unit =
    val document = CstParser.parse(source)
    assert(
      Cst.print(document) == source,
      s"${oneLine(source)} did not round-trip: ${oneLine(Cst.print(document))}"
    )
    val errors = Cst.tilingErrors(document, source.length)
    assert(errors.isEmpty, s"${oneLine(source)} violates tiling: ${errors.mkString("; ")}")

  // "---\n" + "title: x\n" + "---\n" is 4 + 9 + 4 = 17 characters, and the body follows at 17.
  private val Happy = "---\ntitle: x\n---\n\n# H\n"

  "recognition" - {

    "claims the block a profile enables" in {
      val root = rootOf(Happy)
      assert(yamlOf(root).map(_.value.unwrap) == Present("title: x\n"))
    }

    "spans the whole block, both delimiter lines and their terminators" in {
      val root = rootOf(Happy)
      assert(yamlOf(root).flatMap(_.meta.span) == Present(Span(0, 17)))
    }

    "leaves the body exactly the document it would be on its own" in {
      val root = rootOf(Happy)
      val body = rootOf("\n# H\n")(using MdProfile.commonmark)
      assert(root.children.map(_.unpositioned) == body.children.map(_.unpositioned))
    }

    "keeps body spans absolute in the original source" in {
      val root    = rootOf(Happy)
      val heading = root.children.collectFirst { case heading: MdNode.Heading => heading }
      assert(heading.isDefined, s"no heading in ${root.children}")
      heading.foreach { node =>
        node.meta.span match
          case Present(span) => assert(Happy.substring(span.offset, span.end) == "# H")
          case Absent        => assert(false, "the heading carries no span")
      }
    }

    "an empty value region is an empty document, not a missing one" in {
      val root = rootOf("---\n---\nbody\n")
      assert(yamlOf(root).map(_.value.unwrap) == Present(""))
      assert(yamlOf(root).flatMap(_.meta.span) == Present(Span(0, 8)))
    }

    "a closing delimiter at end of input needs no terminator" in {
      val root = rootOf("---\ntitle: x\n---")
      assert(yamlOf(root).map(_.value.unwrap) == Present("title: x\n"))
      assert(yamlOf(root).flatMap(_.meta.span) == Present(Span(0, 16)))
    }

    "CRLF line endings are kept in the raw value" in {
      val source = "---\r\ntitle: x\r\n---\r\nbody\r\n"
      val root   = rootOf(source)
      assert(yamlOf(root).map(_.value.unwrap) == Present("title: x\r\n"))
      assert(yamlOf(root).flatMap(_.meta.span) == Present(Span(0, 20)))
      roundTrips(source)
    }

    "the delimiter is matched exactly: a leading space is not one" in {
      val source = " ---\ntitle: x\n ---\n"
      assert(rootOf(source).frontmatter == Absent)
      assert(rootOf(source) == rootOf(source)(using MdProfile.commonmark))
    }

    "a longer run is not the delimiter" in {
      val source = "----\ntitle: x\n----\n"
      assert(rootOf(source).frontmatter == Absent)
      assert(rootOf(source) == rootOf(source)(using MdProfile.commonmark))
    }
  }

  "fallback" - {

    "a block that never closes parses as plain CommonMark" in {
      val source = "---\ntitle: x\n"
      assert(rootOf(source).frontmatter == Absent)
      assert(rootOf(source) == rootOf(source)(using MdProfile.commonmark))
    }

    "an unclosed block leaves the CST untouched too" in {
      val source = "---\ntitle: x\n"
      assert(CstParser.parse(source) == CstParser.parse(source)(using MdProfile.commonmark))
      roundTrips(source)
    }

    "a delimiter pair below the first line is CommonMark, not frontmatter" in {
      val source = "# H\n\n---\ntitle: x\n---\n"
      assert(rootOf(source).frontmatter == Absent)
      assert(rootOf(source) == rootOf(source)(using MdProfile.commonmark))
    }
  }

  "profile off" - {

    "a document opening with the delimiter is untouched" in {
      val source = "---\ntitle: x\n---\n"
      val root   = rootOf(source)(using MdProfile.commonmark)
      assert(root.frontmatter == Absent)
      assert(root.children.nonEmpty)
    }

    "and parses as the thematic break and setext heading CommonMark reads there" in {
      val root  = rootOf("---\ntitle: x\n---\n")(using MdProfile.commonmark)
      val kinds = root.children.map {
        case MdNode.ThematicBreak(_) => "thematicBreak"
        case MdNode.Heading(_, _, _) => "heading"
        case _                       => "other"
      }
      assert(kinds == Chunk("thematicBreak", "heading"), s"read as $kinds")
    }

    "the default given profile is off, so the conformance corpus cannot see frontmatter" in {
      val root = rootOf("---\ntitle: x\n---\n")(using MdProfile.default)
      assert(root.frontmatter == Absent)
      assert(root.children.nonEmpty)
      val document = CstParser.parse("---\ntitle: x\n---\n")(using MdProfile.default)
      assert(document.childNodes.forall {
        case _: MdCstNode.Frontmatter => false
        case _                        => true
      })
    }
  }

  "cst" - {

    "materializes as delimiter tokens around a raw text leaf" in {
      val document = CstParser.parse(Happy)
      document.children.headOption match
        case Some(front: MdCstNode.Frontmatter) =>
          assert(front.span == Span(0, 17))
          assert(front.childNodes == Chunk(
            MdCstNode.Token("---\n", Span(0, 4)),
            MdCstNode.Text("title: x\n", Span(4, 9)),
            MdCstNode.Token("---\n", Span(13, 4))
          ))
        case other => assert(false, s"expected a frontmatter node, got $other")
    }

    "an empty value region carries no text leaf" in {
      val document = CstParser.parse("---\n---\nbody\n")
      document.children.headOption match
        case Some(front: MdCstNode.Frontmatter) =>
          assert(front.childNodes == Chunk(
            MdCstNode.Token("---\n", Span(0, 4)),
            MdCstNode.Token("---\n", Span(4, 4))
          ))
        case other => assert(false, s"expected a frontmatter node, got $other")
    }

    "round-trips byte-exact and tiles" in {
      roundTrips(Happy)
      roundTrips("---\n---\nbody\n")
      roundTrips("---\ntitle: x\n---")
      roundTrips("---\n")
      roundTrips("---")
    }
  }
