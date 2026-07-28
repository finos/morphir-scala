# Log

## 2026-07-28

* **Creation**: Seeded the bundle from `docs/spec/draft/` in `finos/morphir` at commit `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc`, at topic-level granularity. Source modules were `whats-new`, `names`, `attributes`, `types`, `values`, `modules`, `packages`, `distribution`, and `schemas`.
* **Creation**: Every concept in this bundle carries `status: draft` and a `stale_after` of 2026-12-31, reflecting that the upstream specification is explicitly a draft and expected to churn. Rationale material in `docs/design/draft/ir/` was not consumed in this pass and is a candidate for a later one.
* **Update**: Added the [`design/`](/design/index.md) subdirectory covering all of `docs/design/draft/ir/` at the same commit — twelve concepts. Five of them document features the spec draft does not mention at all: [annotations](/design/annotations.md), [layered decorations](/design/decorations.md), the [Document type](/design/document-type.md), [`$meta`](/design/file-metadata.md), and [`$ref`](/design/node-references.md).
* **Update**: Recorded the concrete disagreements between the design documents and the spec draft in [Specification and Design Divergences](/design/divergences.md), and pointed the open note in [Document Tree Layout](/document-tree-layout.md) at it. The design tree's adjacent themes — Document Tree Protocol, Morphir Daemon, Extensions — were deliberately left out of scope; they concern the toolchain rather than the IR format.
