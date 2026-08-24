---
type: Reference
title: Relationship to morphir.json
description: How the TOML and YAML configuration model compares with the older morphir.json project file, and which tools read each.
tags: [morphir, configuration, morphir-toml, morphir-yaml, morphir-json, migration]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification, status and scope
  - id: yaml-spec
    resource: https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-yaml/morphir-yaml-specification.md
    title: Morphir YAML Configuration Specification
  - id: json-spec
    resource: https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-json/morphir-json-specification.md
    title: Morphir JSON Project Configuration Specification
  - id: rust-parser
    resource: https://github.com/finos/morphir-rust/blob/cdfa6c6323ab0f08a285b77a8a857eb9915a83fb/crates/morphir-common/src/config/mod.rs
    title: Morphir Rust configuration parser
  - id: rust-model
    resource: https://github.com/finos/morphir-rust/blob/cdfa6c6323ab0f08a285b77a8a857eb9915a83fb/crates/morphir-common/src/config/model.rs
    title: Morphir Rust configuration model
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Relationship to morphir.json

Two configuration models exist. They are not versions of each other. The newer model has equivalent TOML and YAML
serializations.

## Baseline

This comparison uses the TOML, YAML, and legacy JSON specifications at `finos/morphir` commit
`4d2a6d836da1c3a114241e911f1af0f38b97b453` and the Rust configuration sources at `finos/morphir-rust` commit
`cdfa6c6323ab0f08a285b77a8a857eb9915a83fb`. Re-read the pinned sources when either pin moves.

| | `morphir.json` | `morphir.toml` or `morphir.yaml` |
| --- | --- | --- |
| **Known readers** | `finos/morphir-elm`; supported for compatibility by Morphir Go | The TOML specification targets Go `pkg/config.Config`; Morphir Rust loads TOML and YAML into `MorphirConfig` |
| **Scope** | One project | Workspace, projects, tasks, workflows, toolchains |
| **Key style** | camelCase | snake_case |
| **Required fields** | `name`, `sourceDirectory`, `exposedModules` | None. Every field is optional. |
| **Layering** | Single file | Six merged sources. See [Merge Rules](/merge-rules.md). |
| **Status** | Draft; `morphir-elm` docs authoritative | TOML draft; YAML supported by the Rust loader and CLI |

[rust-parser](https://github.com/finos/morphir-rust/blob/cdfa6c6323ab0f08a285b77a8a857eb9915a83fb/crates/morphir-common/src/config/mod.rs#L35-L72)
[json-spec-table](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-json/morphir-json-specification.md)
[toml-spec-table](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-toml/morphir-toml-specification.md)
[yaml-spec-table](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-yaml/morphir-yaml-specification.md)

The legacy JSON specification places `morphir.toml` out of scope. The YAML specification places legacy
`morphir.json` out of scope. The TOML specification excludes Morphir IR JSON, not the legacy project file, so it does
not state the reciprocal exclusion.
[json-spec](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-json/morphir-json-specification.md#status-and-scope)
[yaml-spec](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-yaml/morphir-yaml-specification.md#status-and-scope)
[toml-spec](https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-toml/morphir-toml-specification.md#status-and-scope)

## Overlapping fields

| Concept | `morphir.json` | TOML or YAML configuration model |
| ------- | -------------- | -------------- |
| Project name | `name` (required) | `[project].name` |
| Source directory | `sourceDirectory` (required) | `[project].source_directory` |
| Public API | `exposedModules` (required) | `[project].exposed_modules` |
| Decorations | `decorations.<id>` with `displayName`, `ir`, `entryPoint` | `[project.decorations.<id>]` with the same three plus `storage_location` |
| Dependencies | `dependencies` and `localDependencies`, both arrays of IR references | No normative section; Rust adds `dependencies` and `dev-dependencies` maps with a different value shape |

## What each has that the other does not

The legacy specification alone defines dependency references with resolution rules for `data:`, `file:`, `http:`,
`https:`, `ftp:`, and plain paths, plus the reserved-but-unimplemented `git:`, `github:`, and `npm:` schemes. See the
`morphir-ir-v3` bundle's project configuration concept.

The newer normative model alone defines workspaces and member discovery, `module_prefix`, IR format version and
strict mode, codegen targets, cache, logging, and UI settings, tasks, workflows, toolchains, and the layered merge.

The dependency gap is the notable one. The normative TOML and YAML model names no dependency section. The Rust
`MorphirConfig` extends that model with top-level `dependencies` and `dev-dependencies` maps whose values are version
strings or detailed version, path, and Git specifications. Those maps are not equivalent to the legacy arrays of IR
references, so the models remain non-interchangeable for a project with dependencies.
[rust-model](https://github.com/finos/morphir-rust/blob/cdfa6c6323ab0f08a285b77a8a857eb9915a83fb/crates/morphir-common/src/config/model.rs#L35-L44)
[rust-dependency-spec](https://github.com/finos/morphir-rust/blob/cdfa6c6323ab0f08a285b77a8a857eb9915a83fb/crates/morphir-common/src/config/model.rs#L191-L210)

## Where the newer model appears in v4

The v4 Document Tree layout places a `morphir.toml` at the root of a `.morphir-dist/` distribution, as
"project-level configuration". See the `morphir-ir-v4-draft` bundle. That is a further sign of direction of travel,
though nothing states that `morphir.json` is deprecated.

## Practical guidance

`morphir-elm` reads `morphir.json`. Treat the TOML and YAML model as configuration for the newer, polyglot
toolchain, and do not assume a tool reads every serialization.
