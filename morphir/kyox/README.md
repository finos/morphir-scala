# morphir-kyox

Kyo extensions: bridges and utilities for the [Kyo](https://getkyo.io) effect system that Morphir needs and Kyo itself
does not ship.

The namespace exists so that general-purpose Kyo code has an obvious home. Its first inhabitant arrived by the wrong
route — a Kyo `Log` ↔ scribe bridge that landed inside the Elm langkit purely because a port renamed an upstream
namespace wholesale, despite containing nothing to do with Elm or with parsing. `kyox` is where that kind of thing
goes instead.

## Modules

| Module | Artifact | What it holds |
| --- | --- | --- |
| [`core`](./core) | `morphir-kyox-core` | The Kyo `Log` ↔ scribe bridge, and an in-memory recorder for testing |

## What belongs here

Code that is about Kyo rather than about Morphir: bridges to other libraries, `Log` and effect wiring, testing
support. If it mentions a Morphir concept — IR, langkits, distributions — it belongs in that module instead.

Everything cross-builds for the JVM, Scala.js, and Scala Native, which is a real constraint on what can live here:
a JVM-only integration is not a candidate for this namespace as it stands.

```bash
./mill morphir.kyox.__.test
```
