# morphir-knowledge-okf

A published OKF model library. `Concept.parse` reads a whole document through `morphir-langkit-markdown` under a
profile that recognizes YAML frontmatter, then decodes the raw block the parser hands back; okf owns the decoding, not
the split. `Bundle.parse` loads an in-memory map of bundle-relative paths. The root `index.md` must carry
`okf_version`. GitHub ingest maps issues, pull requests, and discussions from `morphir-connector-github` onto
concepts.

The kb skill (`.claude/skills/kb`) does not use this library yet. Switching it on is later intent. This module takes
document kinds, frontmatter fields, and the bundle shape from that skill. It does not take SnakeYAML or
commonmark-java. Frontmatter is decoded with Kyo `kyo-schema` and `kyo-schema-yaml`, using `Maybe` for optional fields.
Snake-case OKF keys (`okf_version`, `stale_after`) map onto camelCase fields via `@rename`. `-Yretain-trees` is off by
default (opt in with `MorphirRetainTrees`) so `Tag[Maybe[A]]` works (https://github.com/getkyo/kyo/issues/1883).

`morphir/contrib/knowledge` is microkanren and is unrelated.

## Artifact

`org.finos.morphir::morphir-knowledge-okf` — JVM, Scala.js, and Scala Native.

```scala
import kyo.*
import morphir.knowledge.okf.*

val source =
  """---
    |type: Playbook
    |title: Publishing
    |---
    |
    |# Title
    |""".stripMargin

Concept.parse("publishing.md", source) match
  case Result.Success(concept) => concept.frontmatter.title
  case Result.Failure(err)     => throw err
```
