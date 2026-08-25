# morphir-ui

Client surface for Morphir: kyo-ui views (IR explorer, knowledge/intent browser) and the
transport-blind service contract they consume. One UI value mounts in the browser and in the
Electron desktop renderer unchanged.

Artifact `org.finos.morphir::morphir-ui`. Package `morphir.ui`; contract in `morphir.ui.services`.
JS platform first; the JVM platform is deferred by decision (kb intent 0029). A Wasm link variant
compiles in CI to keep that axis open.

## GitHub connection settings

Settings includes a GitHub.com connection panel shared by the browser host and Electron renderer. Paste a personal
access token and select Connect. The Remember this device control starts unchecked, so a successful connection is
session-only unless the user explicitly selects persistence. The view holds only status and safe error text. It
clears the password input after every connection attempt and never returns the token in a status or protocol response.

When remembering a token fails, Connect reports the failure and preserves the prior connection. It does not fall
back to a session connection. Disconnect removes the active session token and asks the host to remove any remembered
credential. GitHub Enterprise Server is not available in this panel; that work is tracked in `morphir-sx3`.
