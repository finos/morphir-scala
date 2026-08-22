// Deliberately outside `morphir.langkit.markdown`. A test that sat inside the module's own package would see every
// name it is meant to prove `MD` supplies, because members of an enclosing package are in scope without an import —
// and so would a subpackage. Only from a package that is not an ancestor of the module does `import MD.*` have to
// carry its own weight. This is what a downstream consumer's file looks like.
package morphir.langkit.mdfacade

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.MD
import MD.*
import MD.given

/**
 * The facade's contract: one import puts the whole Markdown langkit in scope.
 *
 * Two imports, strictly — `MD.*` and `MD.given` — because a Scala 3 wildcard import does not carry givens, and the
 * String-to-Text conversion is a given. Nothing here names `morphir.langkit.markdown` beyond the facade itself, so a
 * name this suite uses is a name `MD` exports.
 *
 * Sitting outside the package buys a second proof for free: the parse and write machinery is `private[markdown]`, and
 * only from out here does its unreachability show up as a compile error rather than as an ordinary resolution.
 */
class MDTests extends Test[Any]:

  private def oneLine(text: String): String = text.replace("\n", "\\n")

  /**
   * A document the writer spells without introducing an escape or an entity, so a reparse splits no text run and this
   * suite owes no normalization. That rules out prose sitting directly against an inline marker: the writer escapes the
   * space in `hello **world**` as `&#32;`, and a parse then reads the entity as a text node of its own. Fidelity across
   * the cases that do need normalizing is [[morphir.langkit.markdown.MdWriterTests]]'s subject, not this one's — what
   * is being measured here is that the facade reaches the same machinery, not that the machinery is correct.
   */
  private val page: Root =
    doc(
      h1("Title"),
      p("plain prose carrying no markers"),
      p(strong("bold")),
      p(em("italic")),
      p(a("https://example.com")("link")),
      ul(li("one"), li("two")),
      quote(p("quoted")),
      codeBlock("scala", "val x = 1\n"),
      hr
    )

  "MD" - {

    "authors, writes and parses a document back through one import" in {
      val written = writer.write(page)
      parser.parse(written) match
        case Result.Success(reparsed) =>
          assert(
            reparsed.unpositioned == page,
            s"round-trip through the facade lost structure." +
              s"\n  written  ${oneLine(written)}" +
              s"\n  authored $page" +
              s"\n  reparsed ${reparsed.unpositioned}"
          )
        case other =>
          throw new IllegalStateException(s"facade write produced unparseable text ${oneLine(written)}: $other")
    }

    "matches on the AST cases the same import supplies" in {
      val heads = page.children.collect { case Heading(depth, children, _) => (depth, children) }
      assert(heads.size == 1, s"expected one heading, got ${heads.size}")
      assert(heads.head._1 == HeadingLevel.One, "the heading is level one")
      assert(heads.head._2 == Chunk(Text("Title")), s"the heading holds one text node, got ${heads.head._2}")
    }

    "reaches the CST verbs through MD.cst and the CST type through MD" in {
      val written = writer.write(page)
      // `MdCstNode` is flat on MD; only the verbs are nested. Its cases stay qualified, which is what keeps
      // `Paragraph` unambiguous in a file that imported the AST cases flat.
      val document: MdCstNode.Document = MD.cst.parse(written)
      assert(MD.cst.print(document) == written, "the CST reprints what the facade wrote")
      assert(
        MD.cst.tilingErrors(document, written.length).isEmpty,
        s"the CST tiles: ${MD.cst.tilingErrors(document, written.length).mkString("; ")}"
      )
      assert(MD.cst.lower(document).unpositioned == page, "lowering the CST recovers the authored tree")

      val node: MdCstNode = document
      val headings        = node.childNodes.collect { case heading: MdCstNode.AtxHeading => heading }
      assert(headings.size == 1, s"expected one ATX heading in the CST, got ${headings.size}")

      // Every name both trees claim resolves to the AST's case and to nothing else. Were the CST's cases ever
      // flattened onto MD, each of these ascriptions would become ambiguous and this block would stop compiling.
      val paragraph: Paragraph = p("x")
      val heading: Heading     = h1("x")
      val leaf: Text           = text("x")
      val link: Link           = a("https://example.com")("x")
      val image: Image         = img("https://example.com/i.png", "alt")
      val emphasis: Emphasis   = em("x")
      assert(leaf.value == "x" && image.alt == "alt" && link.url == "https://example.com", "the AST cases build")
      assert(
        paragraph.children.nonEmpty && heading.children.nonEmpty && emphasis.children.nonEmpty,
        "and hold what they were given"
      )
    }

    "names both parses for what they produce, not as duplicates" in {
      val written = writer.write(page)
      val syntax  = MD.cst.parse(written)
      val meaning = parser.parse(written)
      assert(MD.cst.print(syntax) == written, "cst.parse keeps every byte")
      assert(meaning.isSuccess, s"parser.parse yields the lowered tree, got $meaning")
      assert(MD.cst.lower(syntax).unpositioned == meaning.getOrThrow.unpositioned, "one is the other, lowered")
    }

    "raises a written tree to the CST of what it wrote" in {
      val written = writer.write(page)
      assert(MD.cst.print(writer.raise(page)) == written, "raise reprints exactly what write spelled")
    }

    "carries the config vocabulary under its unprefixed names" in {
      val profile: Profile = Profile.commonmark.withYamlFrontmatter
      assert(profile.supportsFrontMatter, "a profile with YAML enabled recognizes frontmatter")
      assert(profile.frontmatter == Set(FrontMatterKind.Yaml), "the enabled kind is YAML")

      val style: Style = Style(bullet = '*', headingStyle = HeadingStyle.Setext, hardBreak = HardBreakStyle.Spaces)
      assert(style.bullet == '*', "the style carries its bullet")

      val weight: MetaKey[Int] = MetaKey[Int]("facade.weight")
      val meta: Meta           = Meta.empty.updated(weight, 3)
      assert(meta.get(weight) == Present(3), "a minted key reads back off the meta")
      assert(StyleKeys.bullet.name == "md.bullet", "the published style keys are reachable")

      assert(HeadingLevel.fromInt(7) == Absent, "a level CommonMark cannot express is unrepresentable")
      assert(FenceInfo.parse("scala").language == Present("scala"), "a fence info string parses")
      assert(YamlDocText("k: v").unwrap == "k: v", "yaml document text unwraps")
      assert(MdParseError("boom").message == "boom", "a parse error carries its message")
    }

    "is the only way in: the parser and the writer are not reachable from out here" in {
      // `private[markdown]` is a compile-time restriction, so a compile-time check is what measures it. Both of these
      // resolve fine from inside `morphir.langkit.markdown`; from this package they must not.
      assert(
        !scala.compiletime.testing.typeChecks("morphir.langkit.markdown.internal.Parser.parse(\"x\")"),
        "internal.Parser must not be reachable from outside the markdown package"
      )
      assert(
        !scala.compiletime.testing.typeChecks("morphir.langkit.markdown.internal.MdWriter.write(null)"),
        "internal.MdWriter must not be reachable from outside the markdown package"
      )
      assert(scala.compiletime.testing.typeChecks("MD.parser"), "MD.parser is the way in")
      assert(scala.compiletime.testing.typeChecks("MD.writer"), "MD.writer is the way in")
    }

    "MD.parser forwards only parse, not the internal machinery that bypasses lowering" in {
      // A wildcard export is not stopped by `private[markdown]` — it is satisfied once a member is accessible at
      // the export site, and every member of `Parser` is, from here. `parseWithMetrics` in particular builds a tree
      // straight from the block loop rather than through `Lower`, so reaching it here would accept a profile and
      // then silently ignore every extension it names. `MD.scala` names `parse` explicitly for exactly this reason;
      // this test is what would fail if a future edit widened it back to a wildcard.
      assert(
        !scala.compiletime.testing.typeChecks("MD.parser.parseWithMetrics"),
        "MD.parser.parseWithMetrics must not be reachable: it bypasses Lower and so ignores the profile it is given"
      )
      assert(
        !scala.compiletime.testing.typeChecks("MD.parser.parseFragments"),
        "MD.parser.parseFragments must not be reachable from outside the markdown package"
      )
    }

    "MD.writer forwards only write and raise, not its internal machinery" in
      assert(
        !scala.compiletime.testing.typeChecks("MD.writer.escapeText"),
        "MD.writer.escapeText must not be reachable from outside the markdown package"
      )

    "types a node at the tree's own name" in {
      val node: MdNode = p("plain")
      assert(node.literal == Absent, "a paragraph is not a literal-bearing leaf")
      assert(node.childNodes == Chunk(Text("plain")), s"the paragraph holds its text, got ${node.childNodes}")
    }

    "reports a frontmatter document only under a profile that enables it" in {
      given Profile = Profile.commonmark.withYamlFrontmatter
      val withFront = doc(frontmatter = yaml("title: x\n"))(p("body"))
      parser.parse(writer.write(withFront)) match
        case Result.Success(reparsed) =>
          assert(reparsed.unpositioned == withFront, s"frontmatter did not survive, got ${reparsed.unpositioned}")
        case other => throw new IllegalStateException(s"frontmatter document did not parse: $other")
    }
  }
end MDTests
