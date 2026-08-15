# morphir-ui

Client surface for Morphir: kyo-ui views (IR explorer, knowledge/intent browser) and the
transport-blind service contract they consume. One UI value mounts in the browser and in the
Electron desktop renderer unchanged.

Artifact `org.finos.morphir::morphir-ui`. Package `morphir.ui`; contract in `morphir.ui.services`.
JS platform first; the JVM platform is deferred by decision (kb intent 0029). A Wasm link variant
compiles in CI to keep that axis open.
