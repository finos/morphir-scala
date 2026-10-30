# Contributing to the `morphir` module

This file covers dev notes specific to the `morphir` module (the root Morphir library — `morphir.jvm`/`morphir.js`) that don't belong in the root [CONTRIBUTING.md](../CONTRIBUTING.md) (project governance/process) or [AGENTS.md](../AGENTS.md) (general AI agent guidelines). See those first; this file only covers what's local to this module.

## Known issues

None at present.

Scaladoc for `morphir.jvm` and `morphir.js` was disabled until the build moved to Scala `3.9.0`. Scala `3.8.4` crashed
in `dotty.tools.scaladoc.translators.SignatureBuilder` on JDK 25
([scala/scala3#24183](https://github.com/scala/scala3/issues/24183)), so both modules published an empty doc jar.
Scala `3.9.0` contains the fix ([scala/scala3#25779](https://github.com/scala/scala3/pull/25779)), and the
workaround is gone.
