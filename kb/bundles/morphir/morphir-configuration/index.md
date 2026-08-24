---
okf_version: "0.2"
title: "Morphir Configuration"
description: "The shared Morphir configuration model, its TOML and YAML serializations, and the implementation status of layered discovery and merging."
---

# Morphir Configuration

Knowledge bundle for the Morphir tooling configuration model and its `morphir.toml` and `morphir.yaml`
serializations. Existing TOML section references retain their original [finos/morphir](https://github.com/finos/morphir)
pin at `4d5e5c06`. The YAML, discovery, overview, and merge updates use `4d2a6d83`. The Rust implementation reference
uses [finos/morphir-rust](https://github.com/finos/morphir-rust) commit `cdfa6c63`.

The configuration model is independent of its file serialization and of the IR format version. It configures the
tooling, not the IR. The older
`morphir.json` project file is a different, narrower thing; see
[Relationship to morphir.json](/relationship-to-morphir-json.md).

> **Status.** The TOML specification remains draft. The YAML specification records Rust loader and CLI support.
> The normative merge specification defines six layers. The Rust configuration context loader used by the CLI
> currently loads the global user and selected project or workspace layers, with an optional workspace-member
> project merge. The daemon loads each discovered file directly without those context merges.

## Orientation

* [Configuration Overview](/overview.md) - How TOML and YAML represent one configuration model, how files are discovered, and where the specification and Rust implementation differ.
* [Merge Rules](/merge-rules.md) - The six configuration sources, their precedence order, and the deterministic deep-merge algorithm.
* [Relationship to morphir.json](/relationship-to-morphir-json.md) - How the TOML and YAML configuration model compares with the older morphir.json project file, and which tools read each.

## Implementation references

* [Morphir Rust configuration support at cdfa6c63](/morphir-rust-configuration-cdfa6c63.md) - Commit-pinned behavior of the Rust parser, discovery logic, merge subset, CLI, and daemon for TOML and YAML configuration.

## Sections

* [Workspace and Project](/workspace-and-project.md) - Workspace discovery and member globs, project metadata, and decoration declarations.
* [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) - The morphir, ir, codegen, cache, logging, and ui configuration sections.
* [Tasks and Workflows](/tasks-and-workflows.md) - Intrinsic and command tasks with their dependencies and hooks, and staged workflow orchestration.
* [Toolchains](/toolchains.md) - External tool adapters — how a tool is acquired, run, and what tasks it contributes.
