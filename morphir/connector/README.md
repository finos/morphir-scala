# morphir-connector

Connectors: clients for external systems Morphir tooling and library users talk to.

Each connector is scoped to one system (GitHub, later GitLab, and so on) and holds what Morphir needs from that
system's API or CLI. Grouping them under `connector` keeps that kind of code out of `kit` (which wraps Scala
libraries Morphir builds on) and out of feature modules, where it otherwise accumulates by accident.

## Connectors

| Connector | Artifact | For |
| --- | --- | --- |
| [`github`](./github) | `morphir-connector-github` | The GitHub GraphQL API (issues, pull requests, discussions, gists) |

## What belongs in a connector

Code that is about the external system rather than about Morphir. If a type mentions a Morphir concept (IR,
distributions, OKF, langkits) it belongs in that module instead. A connector that has to change when the IR or the
knowledge model changes is misplaced.

GitHub ingest into OKF lives in `morphir/knowledge/okf` and depends on this family. It is not a connector.

Everything here cross-builds for the JVM, Scala.js, and Scala Native. A later `github-cli` sibling may be JVM-only
because `gh` is a process; that exception is named on that module, not here.

```bash
./mill morphir.connector.__.test
```
