---
type: Format
title: Tasks and Workflows
description: Intrinsic and command tasks with their dependencies and hooks, and staged workflow orchestration.
tags: [morphir, configuration, morphir-toml, tasks, workflows, build]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification — tasks, workflows
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Tasks and Workflows

## `[tasks.<taskName>]`

A task is a project-scoped execution unit, of one of two kinds:

- **Intrinsic** — a built-in Morphir action. `kind = "intrinsic"` (or omitted, which defaults to intrinsic), with
  `action = "morphir.pipeline.compile"` or similar.
- **Command** — an external command. `kind = "command"`, with `cmd = ["..."]`.

### Common fields

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `depends_on` | string[] | Tasks that must run first |
| `pre` | string[] | Pre-hooks |
| `post` | string[] | Post-hooks |
| `inputs` | string[] | Input file patterns |
| `outputs` | string[] | Output paths |
| `params` | table | Arbitrary parameters |
| `env` | table | `string -> string` environment |
| `mounts` | table | Mount name to permission, `"ro"` or `"rw"` |

`inputs` and `outputs` are what make tasks cacheable and incrementally skippable — they declare the dependency edges
a build system needs. `mounts` with read-only and read-write permissions points at sandboxed execution, which fits
the WASM component extension model.

## `[workflows.<workflowName>]`

Workflows orchestrate targets in ordered stages.

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `description` | string | Human-readable description |
| `extends` | string | Base workflow to inherit from — the specification notes the design may evolve |
| `stages` | array | Ordered stage objects |

### Stage objects

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `name` | string | Stage name |
| `targets` | string[] | Targets to run in this stage |
| `parallel` | bool | Run the stage's targets concurrently |
| `condition` | string | Conditional guard |

Stages are **ordered**; parallelism is opt-in *within* a stage. The `condition` field's expression language is not
specified.

## Tasks versus workflows

Tasks describe *what a single unit of work is* and how it depends on others; workflows describe *an ordered plan*
over targets. A task's `depends_on` produces an implicit graph, while a workflow's `stages` impose an explicit
sequence — the two mechanisms coexist and the specification does not say how they interact.

Toolchains contribute their own task catalogs, and those tasks declare which targets they fulfill — see
[Toolchains](/toolchains.md).
