---
type: Design Note
title: Distribution Design
description: The full distribution records, entry point kinds, semantic versioning, and VFS manifests.
tags: [morphir, ir, v4, draft, distribution, semver, entry-points, rationale]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-distributions
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/distributions.md
    title: Distribution Structure (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Distribution Design

Design detail behind [Distribution](/distribution.md) and [Document Tree Layout](/document-tree-layout.md). The
design records are richer than the spec draft's JSON sketches, in ways that matter.

## The three records

Each distribution kind is its own record, all carrying a `PackageInfo` of *name plus version*.

| Kind | Definition or spec | Dependencies hold | Analogy |
| ---- | ------------------ | ----------------- | ------- |
| **Library** | `PackageDefinition` | `PackageSpecification` | Shared library (`.so` / `.dll`) |
| **Specs** | `PackageSpecification` | `PackageSpecification` | Header files (`.h`) |
| **Application** | `PackageDefinition` | **`PackageDefinition`** | Static binary |

The Application row is the one to notice. Its dependencies are full *definitions*, not specifications — statically
linked, so it runs with no external dependency resolution. The spec draft describes Application as "all dependencies
statically linked" without making that type difference explicit.

The design also gives Specs a rationale the spec draft omits: it exists for **native and FFI bindings**, external
SDKs whose implementations are platform-specific (Morphir SDK basics), and third-party packages where only the public
API is needed for type checking.

## Entry points

An entry point is a record, and the collection is `Dict(Name, EntryPoint)`:

```
EntryPoint(target: FQName, kind: EntryPointKind, doc: Option(Documentation))
```

| Kind | Description | Example |
| ---- | ----------- | ------- |
| `Main` | Default or primary entry point | Application startup |
| `Command` | CLI subcommand | `morphir build`, `morphir test` |
| `Handler` | Service endpoint or message handler | HTTP route, queue consumer |
| `Job` | Batch or scheduled job | Nightly report, data sync |
| `Policy` | Business policy or rule | Validation rule, pricing policy |

The design labors one point: **the dictionary key and the `kind` are independent.** The key is an arbitrary
developer-chosen identifier (`"startup"`, `"api-handler"`, `"nightly-report"`); the kind is a semantic category from
that fixed set. `"startup"` with `kind: "main"` is normal, not a mistake.

`Policy` as a first-class entry point kind is a Morphir-specific choice — it treats a business rule as something a
runtime can be pointed at directly, which is the whole premise of business logic as data.

## Semantic versioning

Full SemVer 2.0.0: `MAJOR.MINOR.PATCH[-PRERELEASE][+BUILD]`, with pre-release identifiers that are individually
numeric or alphanumeric, and build metadata **ignored in precedence comparison** per the SemVer spec.

```
1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-alpha.beta < 1.0.0-beta
< 1.0.0-beta.2 < 1.0.0-beta.11 < 1.0.0-rc.1 < 1.0.0
```

Versions serialize as canonical strings: `"2.1.0-alpha.1"`, `"3.0.0-rc.2+build.456"`.

## Format version is a semver string

Every VFS file carries `formatVersion` as a **semver string**, currently `"4.0.0"` — major for breaking structural or
semantic changes, minor for backwards-compatible additions, patch for fixes and clarifications.

This is a real break from v3, where the format version was a bare integer. It also conflicts with the spec draft,
which shows `"formatVersion": 4` as an integer in `module.json`. See
[Specification and Design Divergences](/design/divergences.md).

## Manifests

```
VfsManifest(format_version, layout, package, created)          // format.json
VfsModuleManifest(format_version, path, types, values)          // module.json
```

`format.json` additionally carries `distribution` (`"Library"` / `"Specs"` / `"Application"`), `version`, an ISO 8601
`created` timestamp, and — for an Application — the `entryPoints` map.

The design's `module.json` is a **manifest listing type and value names**:

```json
{
  "formatVersion": "4.0.0",
  "path": "my-org/domain",
  "types": ["user", "user-(id)", "order"],
  "values": ["get-user-by-email", "create-order"]
}
```

The spec draft's `module.json` instead uses a `module` key for the path, and in inline style holds full definitions
under `types` and `values`. Same filename, materially different content.

## Node file headers

Every `*.type.json` and `*.value.json` carries a header of `formatVersion` and `name` before its `def` or `spec` key:

```json
{
  "formatVersion": "4.0.0",
  "name": "user",
  "def": { "TypeAliasDefinition": { "body": { "Record": { "fields": { } } } } }
}
```

The `name` field is redundant with the filename by design — it survives the file being moved, copied, or streamed out
of the tree.
