# Morphir Knowledge Bundles

Grouping directory for the Morphir-related OKF bundles. **This directory is not itself a bundle** — it holds no
`index.md`, deliberately, so that nothing mistakes it for a bundle root. Each subdirectory below is a separate,
self-contained OKF bundle.

| Bundle | Source of truth | Description |
| ------ | --------------- | ----------- |
| [`morphir-ir-v3`](./morphir-ir-v3/) | [finos/morphir](https://github.com/finos/morphir) `docs/` | The Morphir IR specification at format version 3 — the current, active version. |
| [`morphir-ir-v4-draft`](./morphir-ir-v4-draft/) | [finos/morphir](https://github.com/finos/morphir) `docs/spec/draft/` and `docs/design/draft/ir/` | The draft specification for Morphir IR format version 4, with the design rationale behind it. Not yet active; subject to change. |
| [`morphir-configuration`](./morphir-configuration/) | [finos/morphir](https://github.com/finos/morphir) `docs/spec/morphir-toml/` | The `morphir.toml` workspace and project configuration format, and how layered configuration sources merge. |
| [`morphir-elm`](./morphir-elm/) | [finos/morphir-elm](https://github.com/finos/morphir-elm) | The Elm implementation of Morphir, which produces and consumes IR format version 3. |

## Source discipline

These bundles were seeded from two upstream repositories at pinned commits:

- `finos/morphir` @ `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc` (2026-07-27)
- `finos/morphir-elm` @ `1956c36d3715851a2f215775a45395690746d801` (2026-05-28)

Two constraints apply to how those repositories may be used as sources:

1. **`finos/morphir` — documentation only.** Only `docs/` is authoritative for spec knowledge. Code outside `docs/`
   in that repository is experimental and must not be consulted for knowledge or used to verify claims.
2. **`finos/morphir-elm` — the v3 implementation.** Its source is authoritative for how format version 3 is actually
   implemented, and is the right place to verify v3 claims against working code.

Every concept document records the file it came from in its `sources` frontmatter, with a commit-pinned URL.

See [../../AGENTS.md](../../AGENTS.md) for the OKF conventions these bundles follow.
