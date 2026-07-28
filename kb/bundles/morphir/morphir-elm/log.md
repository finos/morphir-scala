# Log

## 2026-07-28

* **Creation**: Seeded the bundle from `finos/morphir-elm` at commit `1956c36d3715851a2f215775a45395690746d801`, at topic-level granularity. Sources were the repository's `README.md`, `morphir.json`, `package.json`, `elm.json`, and the `src/Morphir/` source tree.
* **Creation**: This bundle is deliberately structural — it maps the implementation onto the specification and records verifiable facts such as `currentFormatVersion = 3`. Behavioral detail (evaluation, type inference, backend semantics) was not mined in this pass and is a candidate for a later one.
