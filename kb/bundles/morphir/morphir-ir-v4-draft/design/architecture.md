---
type: Design Note
title: v4 Architecture
description: The hub-and-spoke daemon model behind v4, its design principles, and how the design documents track status.
tags: [morphir, ir, v4, draft, architecture, daemon, vfs]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-ir-readme
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir/README.md
    title: IR v4 Format (design)
  - id: design-readme
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/README.md
    title: Morphir v4 Design (design)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# v4 Architecture

The v4 format is not a standalone redesign — it is the data model for a larger toolchain architecture. Reading the
[specification concepts](/index.md) without this context makes several decisions look arbitrary.

The design tree records IR v4 as version `0.1.0-draft`, dated 2026-01-15, status **Partial Implementation**.

## Purpose

The design specifies a "Morphir-VFS" architecture plus a JSON-RPC 2.0 protocol for the next-generation toolchain,
enabling a polyglot ecosystem where a Core Daemon orchestrates compilation and refactoring across language-agnostic
backends.

## Design principles

| Principle | Meaning |
| --------- | ------- |
| **Immutability First** | All IR transformations are modeled as immutable state transitions |
| **VFS-Centric** | The distribution is a hierarchical file system, reachable by standard shell tools |
| **Graceful Degradation** | "Best effort" code generation during incremental refactoring |
| **Transactional Integrity** | Multi-module refactors use a Propose-Commit lifecycle |
| **Dual Mode** | Both classic single-blob and discrete VFS layouts are supported |

Two of these explain features that otherwise look odd: *Graceful Degradation* is why
[Incompleteness](/incompleteness.md) exists, and *Transactional Integrity* is why a `DeletedDuringRefactor` hole
carries a transaction ID and why the addressing scheme includes `morphir://session/`.

## Hub-and-spoke

```
                    ┌─────────────────────┐
                    │     Core Daemon     │
                    │  (Gleam/Go/Rust)    │
                    │   VFS Manager       │
                    │   IR Graph          │
                    └──────────┬──────────┘
                               │ JSON-RPC 2.0
           ┌───────────────────┼───────────────────┐
           ▼                   ▼                   ▼
    ┌─────────────┐     ┌─────────────┐     ┌─────────────┐
    │  TypeScript │     │ Spark/Scala │     │     Go      │
    │   Backend   │     │   Backend   │     │   Backend   │
    └─────────────┘     └─────────────┘     └─────────────┘
```

- **Hub** — a language-agnostic daemon acting as JSON-RPC 2.0 server and VFS orchestrator.
- **Spokes** — polyglot backends consuming IR through the VFS protocol.
- **Transport** — JSON-RPC 2.0 over HTTP (CLI to daemon) or stdin/stdout (LSP or one-shot).

The IR-as-filesystem choice follows from this: if backends are separate processes in other languages, a directory of
JSON files is a far cheaper contract than a shared in-memory data structure.

## Reference notation

All type definitions in the design documents use **Gleam** syntax as the canonical reference, chosen to make
functional contracts and sum/product type semantics explicit. Gleam is a notation here, not an implementation
commitment — the daemon is described as "Gleam/Go/Rust".

## VFS terminology

The design documents say **VFS mode**; the spec draft says **Document Tree mode**. They are the same thing. The
design also names two artifacts the spec draft does not mention:

- `session.jsonl` — an append-only transaction journal at the distribution root, for crash recovery.
- `deco/` — the layered decoration tree. See [Layered Decorations](/design/decorations.md).

## Status tracking

Design documents carry tracking frontmatter — `status`, plus `tracking` with `beads`, `github_issues`,
`github_discussions`, and `implementation` path. Status values run Draft → Review → Approved → POC → Partial →
Complete.

Per-document status at the seeded commit:

| Document | Status |
| -------- | ------ |
| Naming | Partial |
| Types | POC |
| Values | POC |
| Modules, Packages, Distributions, Document, Metadata, References | Draft |
| Decorations | Partial |

Nothing in the IR v4 design is marked Approved or Complete. That is the single most important fact to carry out of
this bundle.

## Scope not covered here

The design tree has three further themes this bundle does not cover: the **Document Tree Protocol** (JSON-RPC API),
the **Morphir Daemon** (workspace lifecycle, dependency resolution, incremental builds, file watching), and
**Extensions** (WASM Component Model, task system). They are adjacent to the IR format rather than part of it.
