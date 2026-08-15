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

## Fenced code info

CommonMark treats the fence info string as opaque. `FenceInfo` keeps that string as `raw` and derives conventions
on top:

- `language` — first bare token, or the first Pandoc class when the info is brace-led (`{.haskell}`)
- `args` / `flags` / `option` — CLI-style tokens after the language (Kyo doctest: `doctest:expect=runs`,
  `doctest:setup`, `noformat`)
- `id` / `classes` / `attributes` — Pandoc brace attributes (`{#id .class key=value}`), including the combo form
  `scala {.numberLines}`

Construct only through `FenceInfo.parse` (or `FenceInfo.empty`). The constructor is package-private so callers cannot
assemble fields that disagree with `raw`.

```scala
val info = FenceInfo.parse("scala doctest:expect=runs noformat")
info.language                 // Present("scala")
info.option("doctest:expect") // Present("runs")
info.flag("noformat")         // true

val pandoc = FenceInfo.parse("{#mycode .haskell .numberLines startFrom=\"100\"}")
pandoc.language // Present("haskell")
pandoc.id       // Present("mycode")
pandoc.classes  // Chunk("numberLines")
```
