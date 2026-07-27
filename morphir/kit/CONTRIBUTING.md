# Contributing to `morphir-kit`

The root [CONTRIBUTING.md](../../CONTRIBUTING.md) covers contribution process and governance, and
[AGENTS.md](../../AGENTS.md) covers build layout, code style, and testing conventions. Both govern here. This file
carries only what is true of `kit`.

## What earns a place here

A kit is scoped to one upstream library and holds what Morphir needs from it that the library does not ship. Bridges
to other libraries, wiring, testing support.

Two rules keep the boundary honest:

1. **No Morphir concepts.** If a type mentions IR, distributions, langkits, or any other Morphir domain idea, it
   belongs in that module. A kit that has to be updated when the IR changes is misplaced.
2. **Cross-platform or not at all.** Everything here builds for the JVM, Scala.js, and Scala Native. A JVM-only
   integration does not fit as the namespace currently stands, and adding one means deciding whether `kit` should grow
   platform-specific modules rather than quietly making an existing kit JVM-only.

The namespace was created because a Kyo `Log` ↔ scribe bridge had ended up inside the Elm langkit — carried there by
a port that renamed an upstream namespace wholesale, despite the code having nothing to do with Elm. The same gravity
applies to anything library-shaped written while working in a feature module: it lands wherever it was written unless
someone moves it here.

## Adding a kit

One directory per kit, named for the library it wraps, each with its own `package.mill.yaml` declaring `jvm`, `js`,
and `native` blocks. Use the double-colon dependency form for the JS and Native blocks (`group::artifact::version`) —
a single colon cross-builds only by Scala version and silently resolves the JVM jar. Native test blocks also need
`org.scala-native::test-interface::<version>`.

Kits publish under `org.finos.morphir` as `morphir-kit-<name>`, derived automatically from the directory path; there
is no need to set `artifactName`.

## Naming a kit after its library

A kit directory named for its library means the Scala package is too — `morphir.kit.kyo` for the Kyo kit — which puts
a package named `kyo` in scope inside sources that also import the real `kyo` library.

This resolves correctly today: `import kyo.*` inside `morphir.kit.kyo.log` reaches the library, not the enclosing
package. It is worth knowing about anyway, because it is the kind of thing that starts failing once a type is added
directly in the kit's own package rather than in a subpackage. If a name ever resolves to the wrong `kyo`, the fix is
`_root_.kyo`, not a rename.

## Upstream versions

Library versions are pinned in [`mill-build/src/millbuild/deps.scala`](../../mill-build/src/millbuild/deps.scala).
Because kits bridge their library's own API surface — `Log.Unsafe` in the Kyo kit's case — they are more exposed to
upstream changes than ordinary dependents, and an upgrade should build and test `morphir.kit.__` first.
