# Log

## 2026-08-02

* **Update**: Corrected [Schema Architecture](/schema-architecture.md), [Document Tree Layout](/document-tree-layout.md) and [Specification and Design Divergences](/design/divergences.md) against the schemas themselves, now mirrored in the sibling `morphir/morphir-upstream` bundle. The bundle's seeding took upstream's `docs/spec/draft/schemas.md` at its word and documented a nine-file `schemas/v4/` hierarchy that does not exist; the real schemas are `website/static/schemas/morphir-ir-v4.yaml` and `morphir-ir-v4-document-tree-files.yaml`.
* **Update**: Most of the recorded encoding divergences turned out to be settled by those schemas — `formatVersion` is a `oneOf` over semver and legacy integer rather than a disagreement, annotations are in the schema, and `DerivedTypeSpecification` is flat. What replaced them is a set of contradictions *inside* the schemas, and a previously unrecorded `manifest.json` versus `format.json` split over the distribution root metadata file.

## 2026-07-28

* **Creation**: Seeded the bundle from `docs/spec/draft/` in `finos/morphir` at commit `4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc`, at topic-level granularity. Source modules were `whats-new`, `names`, `attributes`, `types`, `values`, `modules`, `packages`, `distribution`, and `schemas`.
* **Creation**: Every concept in this bundle carries `status: draft` and a `stale_after` of 2026-12-31, reflecting that the upstream specification is explicitly a draft and expected to churn. Rationale material in `docs/design/draft/ir/` was not consumed in this pass and is a candidate for a later one.
* **Update**: Added the [`design/`](/design/index.md) subdirectory covering all of `docs/design/draft/ir/` at the same commit — twelve concepts. Five of them document features the spec draft does not mention at all: [annotations](/design/annotations.md), [layered decorations](/design/decorations.md), the [Document type](/design/document-type.md), [`$meta`](/design/file-metadata.md), and [`$ref`](/design/node-references.md).
* **Update**: Recorded the concrete disagreements between the design documents and the spec draft in [Specification and Design Divergences](/design/divergences.md), and pointed the open note in [Document Tree Layout](/document-tree-layout.md) at it. The design tree's adjacent themes — Document Tree Protocol, Morphir Daemon, Extensions — were deliberately left out of scope; they concern the toolchain rather than the IR format.
