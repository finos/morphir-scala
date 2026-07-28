---
type: Format
title: Workspace and Project
description: Workspace discovery and member globs, project metadata, and decoration declarations.
tags: [morphir, configuration, morphir-toml, workspace, project, decorations]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification — workspace, project
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Workspace and Project

## `[workspace]`

| Field | Type | Default | Meaning |
| ----- | ---- | ------- | ------- |
| `root` | string | directory containing the config file | Workspace root directory |
| `output_dir` | string | `".morphir"` | Output directory for generated artifacts, relative to the root |
| `members` | string[] | — | Glob patterns discovering workspace member projects |
| `exclude` | string[] | — | Glob patterns excluded from member discovery |
| `default_member` | string | — | Default member path when none is specified |

`members` and `exclude` are what make Morphir a multi-project tool: one workspace holds many projects, discovered by
glob rather than enumerated.

## `[project]`

Single-project configuration, or the root project in a workspace.

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `name` | string | Project identifier — kebab-case, PascalCase, or dotted |
| `version` | string | Project version |
| `source_directory` | string | Directory containing project sources |
| `exposed_modules` | string[] | Modules in the project's public API |
| `module_prefix` | string | Optional prefix for qualified names |

Everything here is **optional**, which is the sharpest contrast with `morphir.json`, where `name`,
`sourceDirectory`, and `exposedModules` are all required.

`module_prefix` has no `morphir.json` counterpart. It separates the project's identity from the module namespace,
which `morphir.json` conflates by deriving the prefix from `name`.

## `[project.decorations.<decorationId>]`

Sidecar metadata schemas and value locations, keyed by decoration id.

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `display_name` | string | Human-readable UI label |
| `ir` | string | Path to the decoration schema IR file |
| `entry_point` | string | Fully-qualified type reference, `Package:Module:Type` |
| `storage_location` | string | Path to the decoration values file |

`storage_location` is new relative to `morphir.json`'s decoration config, which declares the schema but not where
values live.

For what decorations are, see the `morphir-elm` bundle's decorations concept; for where the v4 design takes them, see
the `morphir-ir-v4-draft` bundle's layered decorations design note.
