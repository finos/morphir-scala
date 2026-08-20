# Contributing to `morphir-langkit-markdown`

The namespace guide [`morphir/langkit/CONTRIBUTING.md`](../CONTRIBUTING.md) governs here, along with the root
[CONTRIBUTING.md](../../../CONTRIBUTING.md) and [AGENTS.md](../../../AGENTS.md). This file carries only what is true of
`langkit-markdown`.

## One public package, and `internal` for everything else

**Every public type lives in `morphir.langkit.markdown`, in all three artifacts.** Do not add a public sub-package
to group them — no `.compile`, no `.ast`, no `.parser`, and not `.scalatags` or `.kyoui` for the writers either. A
consumer writes one import and has the whole surface, whichever artifacts are on the classpath:

```scala
import morphir.langkit.markdown.*
```

Grouping by role reads well in a directory listing and badly at a call site. It makes a caller who wants to parse and
then compile import two packages to use one library, and it fixes our current internal decomposition into the public
API, so moving a type between roles later becomes a breaking change.

Depth is still available — it just goes downward, not sideways:

| Where | What | Visibility |
| --- | --- | --- |
| `morphir.langkit.markdown` | the public surface: `Document`, `Block`, `HeadingLevel`, `FenceInfo`, `Parser`, `ParseError`, `Compiler`, and each writer's entry point | public |
| `morphir.langkit.markdown.internal` | machinery no caller should name | `private[markdown]` |

**This makes `morphir.langkit.markdown` a split package** — three published artifacts contribute types to it. On a
classpath that is fine, and it is what makes the single import work. It would not be fine on a Java module path:
JPMS forbids two modules exporting the same package, and OSGi takes the same view. Nothing here targets either
today. Reopen the decision if that changes, rather than discovering it at a consumer's integration.

This follows `morphir.buildkit.internal`, which the root [AGENTS.md](../../../AGENTS.md) names as the pattern for
Kyo-based modules.

### Re-export the entry point rather than exposing the machinery

When an internal type holds a method callers genuinely need, `export` it from the public type it belongs to instead of
promoting the whole type. The AST fold is the worked example. The traversal lives in
`internal.MarkdownFold`, which is `private[markdown]`, and the algebra re-exports its entry point:

```scala
object Compiler:
  export morphir.langkit.markdown.internal.MarkdownFold.compile
```

Callers write `Compiler.compile(document)`. They cannot name `MarkdownFold`, so the walk can gain node kinds — as
intent 0021 widens the AST — without any of that being a change to the published surface.

An `export` clause is public even when its target is `private[markdown]`, which is what makes this work. That is
deliberate on Scala's part, not a loophole: the forwarder is the API, and the thing it forwards to is not.

### Check the boundary rather than trusting it

`private[markdown]` is a compile-time rule, so nothing about the emitted class files will tell you whether it holds.
To prove it, put a file in a package outside `morphir.langkit.markdown`, reference the internal type, and confirm the
compile fails:

```scala
package morphir.langkit.probe
import morphir.langkit.markdown.internal.MarkdownFold   // must not compile
```

Delete the probe afterwards. It is worth running whenever `internal` gains a type that something outside is tempted to
reach for.

## The `Compiler` algebra

`Compiler[Out]` has one method per AST node kind, and each method receives children that are **already compiled**. Two
rules keep it useful:

- **Add a node kind to the algebra and every writer at once.** The algebra is what keeps two output formats agreeing;
  a method only one of them implements defeats the point. See
  [intent 0033](../../../kb/bundles/intent/0033-markdown-compilation.md).
- **Keep it pure.** An effectful target instantiates `Out` at `A < S` — Kyo's pending-effect type — rather than making
  every method effectful. An effectful signature spreads across every format and buys nothing.

A `Monoid[Out]` was considered and does not fit: a monoid concatenates siblings, and a heading wraps its children
rather than sitting beside them. A visitor was also rejected, because traversal would then live in every output format
instead of in one driver.

## Writers live in their own artifacts

Output targets are **not** in this module, though they share its package. Each writer is its own published
artifact, so a parse-only consumer such as `morphir-knowledge-okf` pulls in neither:

- `morphir-langkit-markdown-scalatags` — the CommonMark conformance oracle
- `morphir-langkit-markdown-kyo-ui` — the browser path

Their module directories (`scalatags/`, `kyo/ui/`) exist to shape the artifact coordinate, not the package: source
under them still declares `package morphir.langkit.markdown`. The ScalaTags writer imports its library as
`_root_.scalatags` so the artifact directory name cannot be mistaken for a package.

Why there are two, and why kyo-ui cannot serve as the oracle, is recorded in
[intent 0033](../../../kb/bundles/intent/0033-markdown-compilation.md).

## Tests

kyo-test, `class FooTests extends Test[Any]`, files under `test/src/`, `*Tests` suffix. JVM-only tests go in
`test/src-jvm/` — `ParserBinaryCompatibilityTests` is the existing example. Run the module on all three platforms
before pushing:

```bash
./mill morphir.langkit.markdown.{jvm,js,native}.test
./mill morphir.langkit.markdown.scalatags.{jvm,js,native}.test
./mill morphir.langkit.markdown.kyo.ui.{jvm,js,native}.test
```

Moving a source file between these modules confuses incremental compilation, because the package does not change
with it. Scala Native shows it worst: the symptom is a compiler crash (`Duplicate primitive method ==`, or a cyclic
reference) rather than a sensible error. Run `./mill clean <module>` and compile again before believing it.
