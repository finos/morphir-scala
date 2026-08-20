package morphir.knowledge.okf

import kyo.*
import kyo.test.*
import morphir.langkit.markdown.{Block, Inline}

class ConceptTests extends Test[Any]:

  "Concept.parse" - {
    "reads YAML frontmatter and a markdown body" in {
      val source =
        """---
          |type: Playbook
          |title: Publishing
          |description: End-to-end steps.
          |tags:
          |  - elm
          |  - ir
          |---
          |
          |# Title
          |
          |Hello
          |""".stripMargin
      Concept.parse("publishing.md", source) match
        case Result.Success(concept) =>
          assert(concept.path == "publishing.md")
          assert(concept.kind == DocKind.Concept)
          assert(concept.hasFrontmatterBlock)
          assert(concept.frontmatter.`type` == Present("Playbook"))
          assert(concept.frontmatter.title == Present("Publishing"))
          assert(concept.frontmatter.description == Present("End-to-end steps."))
          assert(concept.frontmatter.tags == Chunk("elm", "ir"))
          assert(concept.body.blocks.size == 2)
          concept.body.blocks(0) match
            case Block.Heading(level, content, _) =>
              assert(level.toInt == 1)
              assert(content.map {
                case Inline.Text(value, _)     => value
                case Inline.CodeSpan(value, _) => value
              }.mkString == "Title")
            case _ => assert(false)
        case _ => assert(false)
    }
    "treats a document with no fence as having empty frontmatter" in {
      Concept.parse("note.md", "# Title\n") match
        case Result.Success(concept) =>
          assert(!concept.hasFrontmatterBlock)
          assert(concept.frontmatter == Frontmatter.empty)
          assert(concept.body.blocks.size == 1)
        case _ => assert(false)
    }
    "reads a sources list of mappings" in {
      val source =
        """---
          |type: Design Note
          |sources:
          |  - id: spec
          |    resource: https://example.test/spec
          |    title: Spec
          |---
          |
          |Body
          |""".stripMargin
      Concept.parse("note.md", source) match
        case Result.Success(concept) =>
          assert(concept.frontmatter.sources.size == 1)
          val src = concept.frontmatter.sources(0)
          assert(src.id == Present("spec"))
          assert(src.resource == "https://example.test/spec")
          assert(src.title == Present("Spec"))
        case _ => assert(false)
    }
    "classifies index.md at the bundle root as RootIndex" in {
      Concept.parse("index.md", "---\nokf_version: \"0.2\"\n---\n\n# Bundle\n") match
        case Result.Success(concept) =>
          assert(concept.kind == DocKind.RootIndex)
          assert(concept.frontmatter.okfVersion == Present("0.2"))
        case _ => assert(false)
    }
    "maps snake_case frontmatter keys onto camelCase fields" in {
      val source =
        """---
          |type: Design Note
          |stale_after: 2026-12-01
          |okf_version: "0.2"
          |---
          |
          |Body
          |""".stripMargin
      Concept.parse("note.md", source) match
        case Result.Success(concept) =>
          assert(concept.frontmatter.staleAfter == Present("2026-12-01"))
          assert(concept.frontmatter.okfVersion == Present("0.2"))
        case _ => assert(false)
    }
    "reports malformed YAML frontmatter" in {
      Concept.parse("note.md", "---\ntitle: [unclosed\n---\n\nBody\n") match
        case Result.Failure(OkfError.InvalidFrontmatter(_)) => assert(true)
        case _                                              => assert(false)
    }
  }
