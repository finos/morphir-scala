# Log

## 2026-07-28

* **Creation**: Seeded from `docs/spec/morphir-toml/` in `finos/morphir` at commit `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc` — the format specification and the merge rules document.
* **Creation**: Split into its own bundle rather than folded into `morphir-ir-v3` or `morphir-ir-v4-draft`, because `morphir.toml` configures tooling and is not tied to an IR format version. The `morphir.json` concept remains in the `morphir-ir-v3` bundle, where it belongs as the configuration that v3-era tooling reads.
