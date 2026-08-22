# morphir-appkit

Host-application integrations: Morphir running inside another product's surface.

The family-root artifact is `org.finos.morphir::morphir-appkit`. Package `morphir.appkit`.
`SecretStore.get` returns `Maybe[Secret]`; a missing or empty entry is `Absent`. `Secret` redacts
`toString`, has no public string accessor, and preserves stored whitespace. Failures are
`SecretException`, which extends `MorphirException`. `javaKeychain` uses
`com.github.javakeyring:java-keyring:1.0.4` on the JVM and is not a published kit. `macOsKeychain` runs
`security find-generic-password` on the JVM, Node, and Scala Native. Tests inject a fake store and do not
require a real Keychain.

The `electron` leaf exists: [`morphir/appkit/electron`](./electron/README.md) publishes
`morphir-appkit-electron` (kb intent 0025).

| Planned leaf | Artifact | Host |
| --- | --- | --- |
| `codeium` | `morphir-appkit-codeium` | Codeium |

Planned leaves are backlog intents. Do not add mill children for them until the matching intent leaves the backlog.

Appkit is not `kit` (Scala library bridges) and not `connector` (external-system clients). See
`kb/bundles/morphir/morphir-scala/decisions/0013-published-library-families.md`.

## GitHub connection persistence

`SecretVault` extends `SecretStore` with `put` and `remove`. The local web host uses an operating-system
credential store for remembered GitHub.com tokens. A session-only connection does not write a vault entry and
ends when its host process exits. Remembering is always an explicit UI choice.

If writing a remembered credential fails, the connection attempt fails and leaves the active connection unchanged.
It does not downgrade silently to a session connection. Disconnect removes both the in-memory connection and any
remembered credential for that host.
