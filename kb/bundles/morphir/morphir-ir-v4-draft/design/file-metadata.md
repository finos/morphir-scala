---
type: Design Note
title: File Metadata ($meta)
description: Provenance, tooling, and extension metadata carried at the top level of VFS files.
tags: [morphir, ir, v4, draft, metadata, provenance, vfs]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-meta
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/meta.md
    title: File Metadata ($meta) (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# File Metadata (`$meta`)

**Not present in the v4 spec draft.** A standard place for file-level metadata in VFS JSON files, without polluting
the IR schema.

## Design principles

- **File-level only** — `$meta` sits at the top level of a file, never on individual nodes.
- **Open JSON** — plain JSON objects, no special processing.
- **Optional** — files without it are valid; tools must handle absence.
- **Extensible** — namespaced extensions carry tool-specific data.
- **Non-semantic** — metadata never affects IR semantics or type checking.

That last principle is the load-bearing one: a tool may discard `$meta` entirely and still be correct.

## How it differs from the neighbors

| Feature | Purpose | Scope |
| ------- | ------- | ----- |
| **`$meta`** | Operational metadata — provenance, tooling | File |
| [Decorations](/design/decorations.md) | Semantic annotations — docs, deprecated | IR nodes |
| `formatVersion` | Schema version for parsing | File |
| [Attributes](/attributes.md) | Type-level metadata on IR nodes | Node |

## Standard fields

All optional.

### Provenance

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `source` | String | Source file that generated this IR |
| `sourceRange` | SourceRange | `{ "start": [line, col], "end": [line, col] }`, 1-indexed, end inclusive |
| `compiler` | String | Tool and version that generated the file |
| `generated` | String | ISO 8601 generation timestamp |
| `checksum` | String | `algorithm:hexdigest`, e.g. `sha256:...` |

### Tooling

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `editedBy` | String | Last tool or user to modify the file |
| `editedAt` | String | ISO 8601 modification timestamp |
| `locked` | Boolean | Hint that the file should not be auto-modified |
| `generated` | Boolean | True if generated rather than hand-edited |

Note that `generated` appears in both tables with different types — a timestamp under provenance, a boolean under
tooling. The design does not reconcile this.

## Extensions

```json
{
  "$meta": {
    "extensions": {
      "morphir-vscode": { "foldingRanges": [[5, 10]], "diagnosticLevel": "warning" },
      "my-company/custom-tool": { "internalId": "proj-123-user", "reviewStatus": "approved" }
    }
  }
}
```

Simple names for well-known tools, scoped names for organization tools.

## Example

```json
{
  "formatVersion": "4.0.0",
  "name": "user",
  "$meta": {
    "source": "src/Domain/User.elm",
    "sourceRange": { "start": [15, 1], "end": [22, 1] },
    "compiler": "morphir-elm 3.2.0",
    "generated": "2026-01-16T14:30:00Z",
    "checksum": "sha256:e3b0c442..."
  },
  "def": { "TypeAliasDefinition": { } }
}
```

## Processing rules

- **Reading** — treat absence as empty; **preserve unknown fields**, both standard and in `extensions`, for forward
  compatibility; treat invalid structure as a warning, not an error.
- **Writing** — include when provenance is available; **preserve other tools' extensions** when updating; update
  `editedAt` and `editedBy`; recalculate `checksum` on content change.
- **Merging** — field-wise, override winning where present, with `extensions` dictionaries merged.

The preservation rules are what make a shared file safe for several tools to touch.

## Validation

Lenient throughout: unknown standard field is info-level and preserved; invalid field type or `sourceRange` or
timestamp is a warning with the field ignored or kept raw; unknown extension namespace is not flagged at all.

## Security

- `$meta` should not contain secrets.
- `source` paths reveal directory structure — consider stripping them in public distributions.
- Extensions from untrusted sources are untrusted data.
- `checksum` verifies integrity, **not authenticity** — there are no signatures.

## Future considerations

Digital signatures (`ed25519` with public key and value), compression hints, and cross-references to related
artifacts such as tests and docs.
