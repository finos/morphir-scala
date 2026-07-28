---
type: Specification Section
title: URI and Locator
description: Protocol-level addressing for the Document Tree virtual filesystem, and the hybrid Locator identifier.
tags: [morphir, ir, v4, draft, addressing, document-tree]
status: draft
stale_after: 2026-12-31
sources:
  - id: names
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/names.md
    title: Naming (IR v4 draft) — URI and Locator
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# URI and Locator

v4 introduces protocol-level addressing for the [Document Tree](/document-tree-layout.md) virtual filesystem. Where
an [FQName](/naming.md) says *what* an entity is, a URI says *where its file lives*.

## Scheme

Identifies the kind of resource being addressed:

| Scheme | Form | Addresses |
| ------ | ---- | --------- |
| **Pkg** | `morphir://pkg/...` | Local project resources |
| **Deps** | `morphir://deps/...` | External dependency resources |
| **Session** | `morphir://session/...` | Transaction state resources |

The `Session` scheme is the one with no v3 analogue at all — it exists because v4 contemplates transactional editing
of the IR, which is also where `DeletedDuringRefactor` hole reasons come from. See [Incompleteness](/incompleteness.md).

## Suffix

Indicates the content type of a Document Tree node:

| Suffix | File | Contains |
| ------ | ---- | -------- |
| **TypeSuffix** | `.type.json` | Type definition or specification |
| **ValueSuffix** | `.value.json` | Value definition or specification |
| **ModuleSuffix** | `module.json` | Module manifest |

## URI

A protocol-level address combining scheme, path, name, and suffix.

- **Structure**: `Uri(scheme, path, name, suffix)`
- **Example**: `morphir://pkg/my-org/project/main/domain/user.type.json`

## Locator

A **Locator** is a hybrid identifier that can reference an IR entity either way:

- **ByIdentity** — via `FQName`, e.g. `morphir/(sdk):list#map`
- **ByUri** — via `URI`, e.g. `morphir://pkg/.../list/map.value.json`

Its purpose is to bridge semantic IR identity with physical Document Tree addressing, so that tooling working at
either level can name the same thing.
