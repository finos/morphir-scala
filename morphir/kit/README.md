# morphir-kit

Kits: extensions, helpers, and bridges for the libraries Morphir builds on.

Each kit is scoped to one upstream library and holds what Morphir needs from it that the library itself does not ship
— bridges to other libraries, wiring, testing support. Grouping them under `kit` keeps that kind of code out of the
feature modules, where it otherwise accumulates by accident, and gives later kits an obvious home.

## Kits

| Kit | Artifact | For |
| --- | --- | --- |
| [`kyo`](./kyo) | `morphir-kit-kyo` | The [Kyo](https://getkyo.io) effect system — currently a Kyo `Log` ↔ scribe bridge |

## What belongs in a kit

Code that is about the upstream library rather than about Morphir. If a type mentions a Morphir concept — IR,
distributions, langkits — it belongs in that module instead. A kit that has to change when the IR changes is
misplaced.

The first kit arrived by the wrong route: a Kyo `Log` ↔ scribe bridge that landed inside the Elm langkit because a
port renamed an upstream namespace wholesale, despite containing nothing to do with Elm or with parsing. The same
gravity applies to anything library-shaped written while working in a feature module — it stays where it was written
unless someone moves it here.

Everything cross-builds for the JVM, Scala.js, and Scala Native, which is a real constraint on what can live here: a
JVM-only integration is not a candidate as the namespace currently stands.

```bash
./mill morphir.kit.__.test
```
