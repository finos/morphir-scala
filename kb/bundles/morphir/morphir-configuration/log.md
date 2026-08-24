# Log

## 2026-08-24

* **Update**: Reframed the bundle around one configuration model with TOML and YAML serializations. The new YAML, discovery, overview, and merge material is pinned to `finos/morphir` commit `4d2a6d836da1c3a114241e911f1af0f38b97b453`; unchanged section references retain their original pin.
* **Creation**: Added a Rust implementation reference pinned to `finos/morphir-rust` commit `cdfa6c6323ab0f08a285b77a8a857eb9915a83fb`, including discovery, platform paths, merge coverage, CLI and daemon use, and verified specification gaps.
* **Update**: Updated the `morphir.json` comparison and bundle registries to describe the shared TOML and YAML model.

## 2026-07-28

* **Creation**: Seeded from `docs/spec/morphir-toml/` in `finos/morphir` at commit `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc`, using the format specification and the merge rules document.
* **Creation**: Split into its own bundle rather than folded into `morphir-ir-v3` or `morphir-ir-v4-draft`, because `morphir.toml` configures tooling and is not tied to an IR format version. The `morphir.json` concept remains in the `morphir-ir-v3` bundle, where it belongs as the configuration that v3-era tooling reads.
