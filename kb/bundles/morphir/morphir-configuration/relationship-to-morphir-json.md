---
type: Concept
title: Relationship to morphir.json
description: How morphir.toml and the older morphir.json project file compare, and which tools read which.
tags: [morphir, configuration, morphir-toml, morphir-json, migration]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification — status and scope
  - id: json-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-json/morphir-json-specification.md
    title: Morphir JSON Project Configuration Specification
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Relationship to morphir.json

Two project configuration formats exist. They are not versions of each other.

| | `morphir.json` | `morphir.toml` |
| --- | --- | --- |
| **Read by** | `finos/morphir-elm`; supported for compatibility by Morphir Go | Morphir tooling parsing into `pkg/config.Config` |
| **Scope** | One project | Workspace, projects, tasks, workflows, toolchains |
| **Key style** | camelCase | snake_case |
| **Required fields** | `name`, `sourceDirectory`, `exposedModules` | none — everything is optional |
| **Layering** | Single file | Six merged sources — see [Merge Rules](/merge-rules.md) |
| **Status** | Draft; `morphir-elm` docs authoritative | Draft; intended to become authoritative |

Each specification explicitly puts the other out of scope.

## Overlapping fields

| Concept | `morphir.json` | `morphir.toml` |
| ------- | -------------- | -------------- |
| Project name | `name` (required) | `[project].name` |
| Source directory | `sourceDirectory` (required) | `[project].source_directory` |
| Public API | `exposedModules` (required) | `[project].exposed_modules` |
| Decorations | `decorations.<id>` with `displayName`, `ir`, `entryPoint` | `[project.decorations.<id>]` with the same three plus `storage_location` |
| Dependencies | `dependencies`, `localDependencies` — arrays of IR references | **no equivalent section** |

## What each has that the other does not

**Only in `morphir.json`**: dependency references, with their resolution rules for `data:`, `file:`, `http:`,
`https:`, `ftp:`, and plain paths, plus the reserved-but-unimplemented `git:`, `github:`, and `npm:` schemes. See the
`morphir-ir-v3` bundle's project configuration concept.

**Only in `morphir.toml`**: workspaces and member discovery, `module_prefix`, IR format version and strict mode,
codegen targets, cache, logging, and UI settings, tasks, workflows, toolchains, and the layered merge.

The dependency gap is the notable one. `morphir.toml` covers strictly more ground in every other respect, but the
specification names no section for dependency references — so the two formats are not yet interchangeable for a
project with dependencies.

## Where morphir.toml appears in v4

The v4 Document Tree layout places a `morphir.toml` at the root of a `.morphir-dist/` distribution, as
"project-level configuration". See the `morphir-ir-v4-draft` bundle. That is a further sign of direction of travel,
though nothing states that `morphir.json` is deprecated.

## Practical guidance

If you are working with `morphir-elm`, you are working with `morphir.json` — it is what that toolchain reads. Treat
`morphir.toml` as the configuration model of the newer, polyglot toolchain, and do not assume a tool reads both.
