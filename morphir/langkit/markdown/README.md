# morphir-langkit-markdown

Markdown as a langkit: source text to a CST, on the JVM, Scala.js, and Scala Native.

The module owns a CommonMark subset parser: ATX headings, paragraphs, fenced code, unordered lists, and thematic
breaks. Inlines stay raw text. `commonmark-java` must not enter this module. A third-party engine remains allowed
later if one compiles on JVM, JS, and Native.

## Artifact

`org.finos.morphir::morphir-langkit-markdown` — JVM, Scala.js, and Scala Native.

Depends on `morphir-langkit-core` for `Span`. A `QueryableTree` instance is later work against `morphir-langkit-trees`.

```scala
import morphir.langkit.markdown.*

Parser.parse("# Title\n\nHello") match
  case kyo.Result.Success(doc) => doc.blocks
  case kyo.Result.Failure(err) => throw err
```
