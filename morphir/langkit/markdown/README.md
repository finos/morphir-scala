# morphir-langkit-markdown

Markdown as a langkit: source text to a CST, on the JVM, Scala.js, and Scala Native.

The production parser is not chosen yet. `commonmark-java` is JVM-only and must not enter this module. Until a
cross-platform parser (or a shared AST with per-platform engines) is named in the published-library-families Design
Note, this module ships a stub that parses ATX headings and paragraphs so tests run on all three platforms with no
third-party parser.

## Artifact

`org.finos.morphir::morphir-langkit-markdown` — JVM, Scala.js, and Scala Native.

Depends on `morphir-langkit-core` for `Span`. A `QueryableTree` instance is later work against `morphir-langkit-trees`.

```scala
import morphir.langkit.markdown.*

Parser.parse("# Title\n\nHello") match
  case kyo.Result.Success(doc) => doc.blocks
  case kyo.Result.Failure(err) => throw err
```
