package morphir.knowledge.okf

import kyo.*
import kyo.test.*

class BundleTests extends Test[Any]:

  "Bundle.parse" - {
    "loads index, logs, a nested index, and a concept from in-memory files" in {
      val files = Map(
        "index.md" ->
          """---
            |okf_version: "0.2"
            |title: Example
            |---
            |
            |# Example
            |""".stripMargin,
        "log.md" ->
          """# Log
            |
            |## 2026-08-15
            |* **Creation**: Added a concept.
            |""".stripMargin,
        "design/log.md" ->
          """# Design log
            |
            |## 2026-08-15
            |* **Update**: Nested log entry.
            |""".stripMargin,
        "design/index.md" ->
          """---
            |title: Design
            |---
            |
            |# Design
            |""".stripMargin,
        "design/note.md" ->
          """---
            |type: Design Note
            |title: A note
            |description: One sentence.
            |---
            |
            |Body
            |""".stripMargin
      )
      Bundle.parse("example", files) match
        case Result.Success(bundle) =>
          assert(bundle.slug == "example")
          assert(bundle.okfVersion == "0.2")
          assert(bundle.index.kind == DocKind.RootIndex)
          assert(bundle.logs.size == 2)
          assert(bundle.logs.map(_.path).toSet == Set("log.md", "design/log.md"))
          assert(bundle.subIndexes.size == 1)
          assert(bundle.subIndexes(0).path == "design/index.md")
          assert(bundle.concepts.size == 1)
          assert(bundle.concepts(0).frontmatter.title == Present("A note"))
        case _ => assert(false)
    }
    "fails when index.md is missing" in {
      Bundle.parse("example", Map("note.md" -> "# Note\n")) match
        case Result.Failure(OkfError.MissingBundleIndex) => assert(true)
        case _                                           => assert(false)
    }
    "fails when the root index has no okf_version" in {
      Bundle.parse("example", Map("index.md" -> "---\ntitle: Example\n---\n\n# Example\n")) match
        case Result.Failure(OkfError.InvalidFrontmatter(message)) =>
          assert(message.contains("okf_version"))
        case _ => assert(false)
    }
  }
