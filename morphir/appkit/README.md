# morphir-appkit

Host-application integrations: Morphir running inside another product's surface.

The family-root artifact is `org.finos.morphir::morphir-appkit`. Package `morphir.appkit`.
`SecretStore` reads an OS password store. A missing entry is `Absent`. `javaKeychain` uses
`com.github.javakeyring:java-keyring:1.0.4` on the JVM and is not a published kit. `macOsKeychain`
runs `security find-generic-password` on the JVM, Node, and Scala Native. Tests inject a fake store
and do not require a real Keychain.

| Planned leaf | Artifact | Host |
| --- | --- | --- |
| `electron` | `morphir-appkit-electron` | Electron, using Scala |
| `codeium` | `morphir-appkit-codeium` | Codeium |

Those leaves are backlog intents. Do not add mill children for them until the matching intent leaves the backlog.

Appkit is not `kit` (Scala library bridges) and not `connector` (external-system clients). See
`kb/bundles/morphir/morphir-scala/decisions/0013-published-library-families.md`.
