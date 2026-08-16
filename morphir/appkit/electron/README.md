# morphir-appkit-electron

Morphir inside an Electron host: a kyo-jsonrpc transport over Electron IPC, minimal typed facades
over the Electron main-process APIs, and a `SecretStore` backed by Electron `safeStorage`.

Artifact `org.finos.morphir::morphir-appkit-electron`. Package `morphir.appkit.electron`; Electron
API glue in `internal`. JS platform only — this library runs inside Electron's Node and Chromium
runtimes. The renderer stays sandboxed: it talks through an `IpcPort` exposed by a hand-written
preload, never to Electron directly. See kb intent 0025.
