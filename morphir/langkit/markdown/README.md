# morphir-langkit-markdown

Markdown as a langkit: source text to a CST and an AST, on the JVM, Scala.js, and Scala Native.

The module owns its own CommonMark parser rather than wrapping one. `commonmark-java` must not enter this module. A
third-party engine remains allowed later if one compiles on JVM, JS, and Native.

## Dialects and conformance

A parse recognizes plain CommonMark under `MdProfile.commonmark`, or CommonMark plus GitHub Flavored Markdown's five
extensions under `MdProfile.gfm`:

| Extension | Recognizes |
| --- | --- |
| `Tables` | Pipe tables: a header row, a delimiter row fixing per-column alignment, and body rows |
| `TaskListItems` | `[ ]` and `[x]` at the start of a list item's first paragraph, rendered as a disabled checkbox |
| `Strikethrough` | `~~struck~~`, rendered as `del` |
| `Autolinks` | Bare URLs, `www.` hostnames and email addresses linked without `<>` around them |
| `TagFilter` | Escaping of a fixed list of raw HTML tags (`script`, `style`, `iframe` and kin) rather than passing them through |

Each extension branches at the earliest pipeline stage whose output tree can record the decision — see
[CONTRIBUTING.md](./CONTRIBUTING.md#where-a-profile-dependent-construct-branches) and
[decision 0015](../../../kb/bundles/morphir/morphir-scala/decisions/0015-profile-branches-at-the-earliest-capable-stage.md).

Conformance is measured against each dialect's own published example set, byte-exact through the ScalaTags oracle:

- **CommonMark 0.31.2**: 652 of 652.
- **GitHub Flavored Markdown 0.29-gfm**: 662 of 663 measured examples, plus nine recorded divergences — nested
  strong-emphasis examples that match no published CommonMark release — and one open gap (`morphir-t3p.8`, an
  escaped pipe inside a code span inside a table cell).

The Markdown writer carries the same guarantee: every measured GFM example, scoped to the one extension its own
fence claims, survives a write/read round trip with an identical render, exactly as the CommonMark corpus already
did.

## Artifact

`org.finos.morphir::morphir-langkit-markdown` — JVM, Scala.js, and Scala Native.

Depends on `morphir-langkit-core`, Morphir Prelude, and Kyo Core. A `QueryableTree` instance is later work against
`morphir-langkit-trees`.

```scala
import morphir.langkit.markdown.*

Parser.parse("# Title\n\nHello") match
  case kyo.Result.Success(doc) => doc.blocks
  case kyo.Result.Failure(err) => throw err
  case kyo.Result.Panic(err)   => throw err
```

## Parser budgets

`Parser.parse` uses `ScanBudget.default`, which bounds input length, deterministic scanner work, nesting, and emitted
nodes. Resource exhaustion is returned as `Result.Failure(ParseError.Scan(...))`, with the exact typed limit and
UTF-16 source offset:

```scala
import kyo.Result
import morphir.langkit.core.scanner.*
import morphir.langkit.markdown.*

val safe = Parser.parse(source)

val compiledBudget = ScanBudget.limited(
  maxInputLength = InputSize.megabytes(1),
  maxWork = WorkUnits(8L * 1024L * 1024L),
  maxNestingDepth = NestingDepth(128),
  maxOutputNodes = NodeCount(100000L)
)

compiledBudget match
  case Result.Success(smallerBudget) =>
    val limited = Parser.parse(source, smallerBudget)
  case Result.Failure(error) =>
    // Report invalid caller-supplied configuration; no parse occurs.
  case Result.Panic(error) =>
    // Handle an unexpected panic; no parse occurs.
```

Literal measures are checked by an inline macro; an invalid constant fails compilation. Dynamic values go through
`fromCodeUnits`, `fromMegabytes` (and the other `from*` size constructors), and `from`, and return `Result`. Invalid
budget ceilings still return a typed
`ScanBudgetError` in `Result.Failure`; they do not throw. Keep caller-supplied configuration in this result-handling
path rather than unwrapping it. Zero is a valid measure and an invalid ceiling: `InputSize.codeUnits(0)` is a valid
size, and `ScanBudget.limited` then fails with `NonPositiveInputLength`.

Trusted callers can explicitly remove the ceilings:

```scala
val unsafe = Parser.parse(source, ScanBudget.UnsafeUnbounded)
```

`UnsafeUnbounded` is not the safe path. Use it only when the caller independently controls input size and execution
isolation and accepts responsibility for resource containment. It does not change parse results or relax scanner
cursor and progress invariants; it only removes the resource limits.

The output-node budget includes the document, emitted blocks, and a conservative eight-unit reservation for every
fenced-code metadata token. Metadata reservations happen before token materialization and cover retained token text,
token-list linkage, and derived arguments, flags, classes, and attributes. This prevents whitespace-heavy fence info
strings from amplifying into an unbounded metadata collection.

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
