# Contributing to `morphir-langkit`

The root [CONTRIBUTING.md](../../CONTRIBUTING.md) covers contribution process and governance, and
[AGENTS.md](../../AGENTS.md) covers build layout, code style, and testing conventions for the whole repo. Both govern
here. This file carries only what is true of `langkit` and is not derivable from them.

## Where a change belongs

The namespace is layered, and the layer boundaries are the point:

- **`core`** — anything a second langkit would also need, with no knowledge of any particular language. Source
  positions and diagnostic rendering live here.
- **`trees`** — the query DSL and the `QueryableTree[T]` typeclass. Also language-agnostic, but separate from `core`
  because a langkit can want positions without wanting queries.
- **`elm`** — everything Elm-specific.

Before adding to `elm`, check whether the thing is actually Elm-specific. Two clusters have already had to move back
out: the Kyo `Log` ↔ scribe bridge (now [`morphir/kit`](../kit)) and the position/snippet machinery (now `core`).
Both landed under `elm` only because the original port renamed a whole upstream namespace at once.

The test that settled those cases: does it mention Elm, or would a second langkit want it unchanged? `DiagnosticCode`
stayed in `elm` because its newtype validates `ELM-P###`/`ELM-T###`; `SourceOffsets` moved to `core` because it is
arithmetic over a string.

## One `Span` type

There is a single `morphir.langkit.core.Span`, exposing both `offset`/`length` and `start`/`end` views of the same
half-open range. It replaced a pair of types — one for syntax nodes, one for diagnostics — that differed only in
encoding.

Do not reintroduce a second span type for a new encoding. If a caller wants boundaries rather than an extent, use
`Span.fromStartEnd`; if it wants both plus a resolved line and column, use `SourceSpan`, which wraps a `Span` rather
than restating its fields.

## Cross-platform dependencies

Every module here builds for the JVM, Scala.js, and Scala Native, so dependency coordinates need the double-colon
form for the JS and Native blocks:

```yaml
- io.getkyo::kyo-core:1.0.0-RC5     # jvm
- io.getkyo::kyo-core::1.0.0-RC5    # js and native
```

A single colon cross-builds only by Scala version and silently resolves the JVM jar. Nothing fails at resolution — it
surfaces later as a `ClassNotFoundException` when the class is actually loaded, which is why it is worth getting right
up front.

Native test blocks additionally need `org.scala-native::test-interface::<version>`; Mill does not pull it in, and
without it the link fails on an unreachable `sbt.testing.Framework`.

## Tests

These modules use **kyo-test**, not ZIO Test. Extend `kyo.test.Test[Any]` and mix the matching per-platform trait into
the test block — `millbuild.KyoTest`, `KyoTestJS`, `KyoTestNative`, or `KyoTestWasm`. Those traits only set the
framework class, so each test block declares its own kyo-core/kyo-prelude/kyo-test-api/kyo-test-runner dependencies.

Discovery is by fingerprint rather than filename, so a spec whose class name does not match its file still runs. Keep
them matching anyway.

## Adding a langkit

1. Create `morphir/langkit/<name>/` with its own `package.mill.yaml`, one file per module.
2. Depend on `build.morphir.langkit.core.<platform>` and, if it needs queries,
   `build.morphir.langkit.trees.<platform>`.
3. Provide a `QueryableTree[T]` instance for the new tree — see [`trees/CONTRIBUTING.md`](./trees/CONTRIBUTING.md) for
   the invariants an instance must hold to.
4. Add feature coverage under [`itest`](./itest), which is shared across langkits.
