---
type: Format
title: Configuration Overview
description: How morphir.toml is discovered, how TOML maps to the internal object model, and what the top-level keys are.
resource: https://morphir.finos.org/schemas/morphir-config-v1.yaml
tags: [morphir, configuration, morphir-toml, workspace]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Configuration Overview

`morphir.toml` configures Morphir tooling. Its scope is the workspace, projects, tasks, workflows, and toolchains —
not the IR format, which is specified separately.

## Discovery

Tooling treats a directory as a **workspace** when it contains a `morphir.toml`, or the hidden variant
`.morphir/morphir.toml`.

The file at hand is only one of six configuration sources that are merged into an effective configuration — see
[Merge Rules](/merge-rules.md).

## Data model

The document is [TOML](https://toml.io/); its semantics are defined by mapping to an equivalent JSON-like object
model.

| TOML | Object model |
| ---- | ------------ |
| `[workspace]` table | `{ "workspace": { ... } }` |
| `[toolchain.morphir-elm.tasks.make]` dotted table | `toolchain["morphir-elm"]["tasks"]["make"]` |
| Array | JSON array |
| Inline table | JSON object |

Specifying semantics against the object model rather than TOML syntax is what lets environment variables and other
non-TOML sources participate in the same merge.

## Top-level keys

All are optional; an absent section uses defaults.

| Key | Covers | Concept |
| --- | ------ | ------- |
| `morphir` | Core settings — IR version constraints | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `workspace` | Workspace discovery and output layout | [Workspace and Project](/workspace-and-project.md) |
| `project` | Project metadata | [Workspace and Project](/workspace-and-project.md) |
| `ir` | IR processing settings | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `codegen` | Code generation settings | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `cache` | Cache settings | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `logging` | Logging settings | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `ui` | UI and TUI settings | [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) |
| `tasks` | Project task definitions | [Tasks and Workflows](/tasks-and-workflows.md) |
| `workflows` | Named staged workflows | [Tasks and Workflows](/tasks-and-workflows.md) |
| `bindings` | External binding type mapping — WIT, Protobuf, JSON | — |
| `toolchain` | External tool adapters and task catalogs | [Toolchains](/toolchains.md) |

`bindings` is listed in the specification's top-level key map but has no section of its own; treat its shape as
unspecified.

## Naming convention

Keys use `snake_case` (`source_directory`, `output_dir`, `format_version`) — unlike `morphir.json`, which uses
camelCase. See [Relationship to morphir.json](/relationship-to-morphir-json.md).

## Machine-readable schema

- `https://morphir.finos.org/schemas/morphir-config-v1.yaml`
- `https://morphir.finos.org/schemas/morphir-config-v1.json`

The schema describes the equivalent JSON model, not TOML syntax.
