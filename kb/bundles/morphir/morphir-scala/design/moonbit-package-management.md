---
type: Reference
title: MoonBit registry, resolution, and source materialization
description: "How MoonBit resolves and materializes source packages from its Git-backed JSONL registry."
tags: [buildkit, package-management, moonbit, registry, resolution, source-packages]
status: stable
stale_after: 2026-10-05
sources:
  - id: moon-registry
    title: Moon Registry capability
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/mooncake/src/registry.rs
  - id: moon-online-registry
    title: Moon online registry index and archive materializer
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/mooncake/src/registry/online.rs
  - id: moon-index-update
    title: Moon Git registry-index update
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/mooncake/src/update.rs
  - id: moon-dependency-model
    title: Moon dependency source model
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/moonutil/src/dependency.rs
  - id: moon-resolution
    title: Moon minimal-version resolver
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/mooncake/src/resolver/mvs.rs
  - id: moon-materialization
    title: Moon immutable dependency-source materialization
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/crates/mooncake/src/dependency_source/global.rs
  - id: moon-architecture
    title: Moon module dependency architecture
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/docs/dev/reference/arch.md
  - id: moon-license
    title: Moon AGPL-3.0 license
    resource: https://github.com/moonbitlang/moon/blob/53e21954391f334121b68175c206bfcd7e142042/LICENSE
  - id: mooncakes-index
    title: Mooncakes registry index snapshot 0dfbd598afd15b4592e3226b9f1ed3c56a22c270
    resource: "git+https://mooncakes.io/git/index?commit=0dfbd598afd15b4592e3226b9f1ed3c56a22c270"
  - id: moon-package-docs
    title: MoonBit package-management tour
    resource: https://github.com/moonbitlang/moonbit-docs/blob/c86a73f5f60a34f73c4865b837f777368ad52c7a/next/toolchain/moon/package-manage-tour.md
  - id: dashboard-sources
    title: Mooncakes dashboard source selection
    resource: https://github.com/moonbitlang/mooncakes-dashboard/blob/627729df928926d99ef6fac11c9ab978be816ca6/lib/source.ts
  - id: dashboard-jsonl
    title: Mooncakes dashboard line-delimited build output
    resource: https://github.com/moonbitlang/mooncakes-dashboard/blob/627729df928926d99ef6fac11c9ab978be816ca6/main.ts
  - id: dashboard-types
    title: Mooncakes dashboard source and result types
    resource: https://github.com/moonbitlang/mooncakes-dashboard/blob/627729df928926d99ef6fac11c9ab978be816ca6/lib/types.ts
---

# MoonBit registry, resolution, and source materialization

MoonBit's package registry is a Git repository whose package histories are small line-delimited JSON files. A current
record that can be materialized associates its published version with a source-archive checksum; `moon` verifies the
archive with SHA-256 before materializing and building it locally. The permissive reader can enumerate a partial
record whose checksum is absent, but that record cannot complete checksum lookup and source acquisition. The index
supplies discovery and resolution metadata; the archive service supplies content; the package manager owns
verification, caching, and extraction.

This is useful precedent for Morphir because the distribution unit is source, while a lightweight registry remains
capable of version discovery and reproducible acquisition. It is architectural evidence for the evolving
[Package URL-centered package-management design](/design/package-url-package-management.md), not a format or
implementation to copy.

## Scope and licensing boundary

The behavior described here was observed at `moonbitlang/moon` commit `53e2195`, the Mooncakes registry-index commit
`0dfbd59`, the dashboard commit `627729d`, and the documentation commit `c86a73f`.

The Mooncakes index source is written as a `git+https` provenance scope containing both the clone remote and exact
commit. It records the checkout that was inspected; it does not claim that the Git smart endpoint serves an HTTP page
that dereferences the commit query.

The `moon` implementation is AGPL-3.0. This article describes externally visible architecture and data flow; Morphir
must not copy or adapt its code unless the project deliberately accepts that license. The inspected registry index and
dashboard repository do not carry repository-level licenses, so their public availability is evidence, not permission
to reuse implementation or content. A `license` field in a package record describes that published package only.

## Two different line-delimited JSON systems

MoonBit uses line-delimited JSON in two independent places that should not be conflated.

| System | Shape | Purpose | Consumed by ordinary package resolution |
| --- | --- | --- | --- |
| Mooncakes registry index | One `user/<owner>/<module>.index` file per module; one version record per line | Version discovery, dependency metadata, archive integrity | Yes |
| Mooncakes dashboard | One `data/<os>/<channel>/data.jsonl`; metadata header followed by build results | CI health across registry packages and curated Git repositories | No |

There is no central registry `packages.jsonl`. The Git tree itself partitions the registry by package identity, and
each `.index` file is the version history for one module. The dashboard later combines registry packages and curated
Git repositories for testing, but those Git entries do not become package-manager dependencies.

## Registry index layout

The configured index defaults to `https://mooncakes.io/git/index`. `moon update` clones it into the user's registry
area and later pulls its primary branch. If the checkout is invalid, points at a different origin, or cannot be
updated, the updater creates a replacement checkout beside the old one and swaps it into place rather than mutating
an uncertain directory.

For a logical module such as `owner/project`, the client derives a path under `user/owner/project.index`. Additional
module-name path segments become additional directories. The selected snapshot contains examples such as
`user/tonyfettes/js/test.index`.

Each nonempty line is an independent JSON object for one published version. The observed producer payload is
extensible and commonly contains:

| Concern | Observed fields |
| --- | --- |
| Identity | `name`, `version` |
| Resolution | `deps` or older `dependencies` |
| Integrity | `checksum` |
| Source layout | `source`, `include`, `exclude` |
| Presentation and provenance | `readme`, `repository`, `license`, `keywords`, `description`, `created_at` |
| Build policy | target settings, warnings, rules, scripts, binary dependencies |

The package-manager reader deliberately has a smaller kernel: it deserializes `version`, `deps`, and `checksum`, all
as optional fields, and ignores other metadata. Version discovery skips malformed lines and lines without a version.
It accumulates valid versions in semantic-version order. Checksum lookup scans the file from the end for the selected
version, making the last matching record authoritative if duplicate versions exist.

This split lets registry records evolve without forcing the resolver to understand presentation or build metadata.
It also means the format needs independent validation rules: the reader's leniency is not a registry-publication
schema, and malformed historical data can be skipped during discovery but still disrupt later checksum lookup.

### Example `.index` file

The following synthetic file is schema-faithful to records in the pinned registry snapshot. It would live at
`user/example/rules.index`. Each physical line is a complete JSON object; the file is not a JSON array and has no
commas between records.

```jsonl
{"name":"example/rules","version":"0.1.0","deps":{},"readme":"README.md","repository":"https://github.com/example/rules.git","license":"Apache-2.0","description":"Example source package","source":"src","checksum":"1111111111111111111111111111111111111111111111111111111111111111","created_at":"2026-08-01T00:00:00Z"}
{"name":"example/rules","version":"0.2.0","deps":{"example/model":"0.1.0","moonbitlang/x":"0.4.31"},"readme":"README.md","repository":"https://github.com/example/rules.git","license":"Apache-2.0","description":"Example source package","source":"src","checksum":"2222222222222222222222222222222222222222222222222222222222222222","created_at":"2026-08-02T00:00:00Z"}
```

The filename partitions the registry by module identity, while `name` repeats that identity in producer metadata.
For resolution, `moon` reads only `version`, `deps`, and `checksum`; fields such as `readme`, `repository`, `license`,
`description`, `source`, and `created_at` remain useful to publication, presentation, or other tools but are ignored by
the resolver's compact record type. The second line means that version `0.2.0` directly requires the stated versions
of `example/model` and `moonbitlang/x`.

Appending a release adds one independently parseable line and changes only that module's history file. Readers order
the accumulated versions semantically rather than treating line order as version order. Duplicate versions are
possible in the permissive reader; reverse checksum lookup makes the last matching record authoritative. A malformed
line can be skipped during version enumeration, but the later checksum scan is less forgiving, so publication-time
validation remains essential.

### Filesystem portability

The inspected index contains package paths that differ only by case. Cloning it on a case-insensitive filesystem
reported collisions between `Permutation.index` and `permutation.index`, and between `MoonTetris.index` and
`moontetris.index`. A Git tree is therefore not automatically a portable package namespace. A Morphir registry needs
canonical case rules or a storage mapping that cannot collide on supported filesystems.

## Resolution flow

The registry capability exposes operations for listing all versions of a module, locating an exact version's source
checksum, and acquiring that source into a destination. The resolver depends on the capability rather than on HTTP or
the Git checkout directly, which also lets tests supply an in-memory registry.

Resolution proceeds as follows:

1. Read only the requested module's `.index` file.
2. Parse version and dependency requirements from its lines.
3. Order versions with semantic-version rules.
4. Apply MoonBit's minimal-version selection algorithm to the complete dependency graph.
5. Select concrete module versions before fetching their contents.

MoonBit's documented requirements use caret-like compatibility buckets: versions below `2.0.0` share one compatibility
set, while later versions are grouped by major version. This is an ecosystem-specific policy, not behavior that a
generic Morphir resolver should infer from the shape of a version string.

The current tool has no resolved dependency lockfile. Its filesystem `.moon-lock` coordinates concurrent operations;
it is not a package graph. An exact selected version and archive checksum strongly identify content after resolution,
but reproducing the graph also requires retaining the registry-index commit used for selection.

## Source archive acquisition and materialization

The index does not store each archive URL. The online registry derives a download path from registry base, owner,
module name, and selected version. For the selected version it rereads the corresponding `.index` file to obtain the
expected SHA-256 checksum.

The materializer then:

1. Checks whether the archive cache already contains the version.
2. Hashes cached content before reuse; a mismatch forces reacquisition.
3. Streams a newly downloaded ZIP through SHA-256 verification before publishing it to the cache.
4. Keeps the verified file handle open through extraction, reducing replacement races.
5. Extracts into a staging directory.
6. Validates that the contained module manifest has the selected name and version.
7. Records the source-archive checksum beside the prepared tree.
8. Renames the staged tree into an immutable versioned source location.

A later build validates the cached checksum metadata and manifest identity before reuse. If the registry changes the
checksum for an existing name and version, the immutable source cache refuses to silently accept the new content.
Package-controlled post-install hooks are not part of the registry materialization boundary.

This is the important sense in which Mooncakes packages are source packages: publication produces a versioned source
archive, materialization produces a verified source tree, and the consumer's toolchain compiles that tree locally.
The registry is an index over source distributions, not a repository of platform-specific compiled artifacts.

## `source`, local paths, and Git are different concepts

MoonBit's similarly named fields do not all identify an origin:

- A module-level `source` field selects the source-code directory inside a module or published archive.
- A dependency `path` selects another local module relative to the depending module.
- A dependency `git` value names an external repository; the legacy JSON model also permits branch and version fields.

Current support is narrower than the data model suggests. Local path dependencies resolve only for local, workspace,
standard-library, or other non-registry roots; registry-published modules cannot escape into a publisher's filesystem.
Modern local development uses workspace members, which can override a registry identity with a local source tree.

Git dependency fields are accepted by the legacy JSON model, but current dependency resolution contains no Git
implementation and source materialization rejects Git module sources. The newer `moon.mod` syntax permits versioned
registry imports and routes local composition through workspace configuration. It would therefore be incorrect to
describe ordinary `moon` builds as supporting arbitrary Git dependencies.

For Morphir, this distinction is valuable: package identity, source origin, directory within the source, and local
workspace override should be separate types rather than overloaded strings.

## Dashboard Git sources are not registry sources

The separate Mooncakes dashboard selects the latest registry modules, explicitly requested registry ranges, and a
curated list of Git repositories. It builds that corpus across operating systems and toolchain channels and writes a
line-delimited result file per environment.

The first dashboard line contains run metadata. Following lines contain a source descriptor—either a Mooncakes name
and version or a Git URL and revision string—plus per-backend build, check, and test results. The dashboard web client
and analysis tools stream these files directly.

### Example dashboard `data.jsonl`

This synthetic, schema-faithful excerpt illustrates the different line types. The result is conventionally stored at
a path such as `data/linux/nightly/data.jsonl`; each displayed line is one complete JSON value.

```jsonl
{"runId":"123456789","runNumber":"42","startTime":"2026-08-06T12:00:00Z","toolchainVersion":["moon 0.1.0","moonc 0.1.0"]}
{"source":{"type":"mooncakes","name":"example/rules","version":"0.2.0"},"cbt":{"check":{"wasm":{"status":"Success","start_time":"2026-08-06T12:01:00Z","elapsed":123,"stdout_path":"logs/example-rules-check-wasm.out","stderr_path":"logs/example-rules-check-wasm.err"},"wasm-gc":{"status":"Skipped"},"js":{"status":"Skipped"},"native":{"status":"Skipped"}},"build":{"wasm":{"status":"Skipped"},"wasm-gc":{"status":"Skipped"},"js":{"status":"Skipped"},"native":{"status":"Skipped"}},"test":{"wasm":{"status":"Skipped"},"wasm-gc":{"status":"Skipped"},"js":{"status":"Skipped"},"native":{"status":"Skipped"}}}}
{"source":{"type":"git","url":"https://github.com/example/rules.git","rev":"main"},"error":"clone failed"}
```

The line types differ:

- The header has no `source`; it describes the dashboard run.
- Each later line identifies either an exact Mooncakes package or a Git URL and requested revision.
- A successful result may report `check`, `build`, and `test` across `wasm`, `wasm-gc`, `js`, and `native`.
- Each matrix cell is `Success`, `Failure`, `WarningFailure`, or `Skipped`, with logs stored separately.
- A source-level failure may carry `error`, as the final example does.

This `data.jsonl` is generated CI evidence for the dashboard, not a registry index and not an input to ordinary
dependency resolution. In particular, the Git example records the selector `main`; it does not prove which immutable
commit was tested and must not be treated as a reproducible package lock.

Dashboard Git entries are generally moving branches, shallow-cloned for CI. Recording the branch string in a result
does not create an immutable source identity. The dashboard demonstrates that the same build workflow can accept
registry and Git source providers; it does not demonstrate Git dependency resolution or reproducible Git locking in
the package manager.

## Lessons for Morphir source packages

MoonBit provides evidence for several separations that fit Morphir:

- **Identity from location.** A package's logical identity selects registry metadata; archives, Git commits, local
  workspaces, and mirrors remain source descriptors.
- **Small resolution kernel.** A registry entry needs exact identity, dependency requirements, integrity, and source
  information. Description, license, build hints, and discovery metadata can evolve as extensions.
- **One package history per file.** Line-delimited per-package histories give small Git diffs, lazy reads, straightforward
  mirroring, and service-free offline resolution from a pinned index checkout.
- **Source as the distribution.** A publishable Morphir package can contain source modules, native manifest data, and
  metadata needed to produce Morphir IR, rather than requiring precompiled platform artifacts.
- **Verified materialization.** Resolution chooses identity; acquisition verifies bytes; materialization validates the
  package manifest and yields a source tree. Each phase has a separately testable failure boundary.
- **Workspace overlays.** Local source packages can override a stable logical package identity during development
  without making local paths publishable dependencies.
- **A real lock.** Morphir should improve on the observed design by recording the Package URL requirement, selected
  exact purl, registry-index revision, source descriptor, archive digest or immutable Git commit, transitive graph,
  and local-source policy.
- **Portable namespace rules.** Registry storage must not rely on case distinctions unsupported by common filesystems.

A possible Morphir source descriptor is a closed choice among a verified registry archive, an immutable Git commit
with optional subdirectory, a local workspace snapshot, and a vendored tree. The descriptor belongs beside the
resolved `pkg:morphir` identity; it does not replace or become part of that identity merely because the source package
is unpublished.

The launch git-file index may adopt the architectural shape—Git-distributed metadata, per-package histories,
extensible records, content digests—without copying MoonBit's field names, Rust interfaces, endpoint conventions, or
AGPL implementation. Its normative schema must come from Morphir's own requirements and acceptance fixtures.

## Related Morphir work

- [Package URL-centered package management](/design/package-url-package-management.md) applies these observations to
  provisional `pkg:morphir` identity, locks, and source backends.
- [Intent 0007](../../../intent/0007-multi-frontend-morphir-transformation-pipeline.md) owns the overall buildkit
  pipeline refinement.
- [Intent 0013](../../../intent/0013-pluggable-package-resolution-and-materialization.md) owns package resolution and
  source materialization.
