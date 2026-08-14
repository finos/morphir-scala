# morphir-appkit

Host-application integrations: Morphir running inside another product's surface.

This directory is a reserved mill container. It has no published children yet.

| Planned leaf | Artifact | Host |
| --- | --- | --- |
| `electron` | `morphir-appkit-electron` | Electron, using Scala |
| `codeium` | `morphir-appkit-codeium` | Codeium |

Those leaves are backlog intents. Do not add mill children here until the matching intent leaves the backlog.

Appkit is not `kit` (Scala library bridges) and not `connector` (external-system clients). See
`kb/bundles/morphir/morphir-scala/decisions/0013-published-library-families.md`.
