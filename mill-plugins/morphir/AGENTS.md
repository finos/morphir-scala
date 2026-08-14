# Mill Morphir plugin invariants

- Keep one implementation under this directory. The metabuild and publishable modules compile these same source roots.
- Preserve the dependency direction: `javascript -> toolchain`, `elm-tooling -> javascript`, `core` independent, and `elm -> core + elm-tooling`.
- Keep JavaScript runtime and package-manager contracts tool-neutral; Node and npm are implementations in the same plugin jar.
- Keep Kyo out of every plugin dependency graph. Use the pinned `sourcecode` dependency for user-facing call-site data.
- Keep host-language `compile` authoritative. Morphir adapters expose typed tasks and append generated sources.
- Cross-build every published plugin for all supported Mill 1.x versions with the `_mill1` platform suffix and Scala 3.
- Sonatype publication covers the five plugins via `MorphirMillPublishModule` (`__.publishSonatypeCentral`); `integration` stays test-only and unpublished. The old `mill-scalalib`-not-on-Central exclusion is retired — plugins compile against `mill-libs-scalalib`, which is on Maven Central for Mill 1.x.

See [the approved plugin architecture](../../kb/bundles/morphir/morphir-scala/design/mill-morphir-plugin-architecture.md) for rationale.
