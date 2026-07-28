---
okf_version: "0.2"
title: "Morphir Configuration (morphir.toml)"
description: "The morphir.toml workspace and project configuration format, and how layered configuration sources merge."
---

# Morphir Configuration

Knowledge bundle for **`morphir.toml`**, the configuration format used by Morphir tooling for workspaces, projects,
tasks, workflows, and toolchains. Seeded from `docs/spec/morphir-toml/` in
[finos/morphir](https://github.com/finos/morphir) at commit `4d5e5c06`.

`morphir.toml` is orthogonal to the IR format version — it configures the tooling, not the IR. The older
`morphir.json` project file is a different, narrower thing; see
[Relationship to morphir.json](/relationship-to-morphir-json.md).

> **Status.** The upstream specification marks itself *Draft*, "versioned and intended to become the authoritative
> reference". It describes configuration parsed into `pkg/config.Config`.

## Orientation

* [Configuration Overview](/overview.md) - How morphir.toml is discovered, how TOML maps to the internal object model, and what the top-level keys are.
* [Merge Rules](/merge-rules.md) - The six configuration sources, their precedence order, and the deterministic deep-merge algorithm.
* [Relationship to morphir.json](/relationship-to-morphir-json.md) - How morphir.toml and the older morphir.json project file compare, and which tools read which.

## Sections

* [Workspace and Project](/workspace-and-project.md) - Workspace discovery and member globs, project metadata, and decoration declarations.
* [IR, Codegen, and Runtime](/ir-codegen-and-runtime.md) - The morphir, ir, codegen, cache, logging, and ui configuration sections.
* [Tasks and Workflows](/tasks-and-workflows.md) - Intrinsic and command tasks with their dependencies and hooks, and staged workflow orchestration.
* [Toolchains](/toolchains.md) - External tool adapters — how a tool is acquired, run, and what tasks it contributes.
