# morphir-knowledge

Knowledge libraries Morphir publishes. This directory contains the MicroKanren-style logic module and the OKF
model; it is separate from the `kb/` document tree.

## Modules

| Module | Artifact | For |
| --- | --- | --- |
| [`logic`](./logic) | `morphir-knowledge-logic` | Logic goals, unification, constraints, and result streams |
| [`okf`](./okf) | `morphir-knowledge-okf` | Open Knowledge Format bundles, concepts, and GitHub ingest |

```bash
./mill morphir.knowledge.__.test
```
