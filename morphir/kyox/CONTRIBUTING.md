# Contributing to `morphir-kyox`

The root [CONTRIBUTING.md](../../CONTRIBUTING.md) covers contribution process and governance, and
[AGENTS.md](../../AGENTS.md) covers build layout, code style, and testing conventions. Both govern here. This file
carries only what is true of `kyox`.

## What earns a place here

`kyox` is for code that is about Kyo, not about Morphir. Bridges to other libraries, effect and `Log` wiring, testing
support for Kyo-based code.

Two rules keep the boundary honest:

1. **No Morphir concepts.** If a type mentions IR, distributions, langkits, or any other Morphir domain idea, it
   belongs in that module. A `kyox` module that has to be updated when the IR changes is misplaced.
2. **Cross-platform or not at all.** Everything here builds for the JVM, Scala.js, and Scala Native. A JVM-only
   integration does not fit the namespace as it currently stands, and adding one means deciding whether `kyox` should
   grow platform-specific modules rather than quietly making an existing module JVM-only.

The namespace was created because a Kyo `Log` ↔ scribe bridge had ended up inside the Elm langkit — carried there by
a port that renamed an upstream namespace wholesale, despite the code having nothing to do with Elm. The same
gravity applies to anything Kyo-shaped written while working in a feature module: it will land wherever it was
written unless someone moves it here.

## Adding a module

One directory per module, each with its own `package.mill.yaml` declaring `jvm`, `js`, and `native` blocks. Use the
double-colon dependency form for the JS and Native blocks (`group::artifact::version`) — a single colon cross-builds
only by Scala version and silently resolves the JVM jar. Native test blocks also need
`org.scala-native::test-interface::<version>`.

Modules here publish under `org.finos.morphir` as `morphir-kyox-<name>`, derived automatically from the directory
path; there is no need to set `artifactName`.

## Kyo version

The Kyo version is pinned in [`mill-build/src/millbuild/deps.scala`](../../mill-build/src/millbuild/deps.scala).
Because these modules bridge Kyo's own API surface — `Log.Unsafe` in particular — they are more exposed to Kyo
changes than ordinary dependents, and a Kyo upgrade should build and test `morphir.kyox.__` first.
