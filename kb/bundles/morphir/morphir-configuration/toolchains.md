---
type: Format
title: Toolchains
description: External tool adapters — how a tool is acquired, run, and what tasks it contributes.
tags: [morphir, configuration, morphir-toml, toolchains, build]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification — toolchain
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Toolchains

A **toolchain** defines how to acquire and run an external tool, and enumerates the tasks that tool provides. This is
the mechanism by which a polyglot Morphir toolchain drives implementations like `morphir-elm` without hardcoding
them.

## `[toolchain.<toolchainName>]`

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `enabled` | bool | Explicitly enable or disable. If absent, tooling may auto-enable |
| `version` | string | Toolchain version |
| `working_dir` | string | Working directory |
| `timeout` | string | Go-style duration, e.g. `"5m"` |
| `env` | table | `string -> string` |

## `[toolchain.<name>.acquire]`

How the tool is obtained.

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `backend` | string | Acquisition backend — `"path"` is given as an example; others may be planned |
| `package` | string | Package identifier, backend-specific |
| `version` | string | Version constraint, backend-specific |
| `executable` | string | Executable name or path, backend-specific |

Only `"path"` (find it on `PATH`) is named. The backend-specific fields anticipate registry- or package-manager-based
acquisition that does not yet exist.

## `[toolchain.<name>.tasks.<taskName>]`

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `exec` | string | Executable to run |
| `args` | string[] | Arguments |
| `fulfills` | string[] | Targets this task fulfills, e.g. `["make"]` |
| `variants` | string[] | Supported variants, e.g. `["Scala", "TypeScript"]` |
| `env` | table | `string -> string` |

`fulfills` is the indirection that matters: a workflow names an abstract target such as `make`, and whichever
enabled toolchain declares it fulfills that target provides it. `variants` lets one task cover several code
generation targets.

### Inputs

Two accepted forms:

- **Array** — `inputs = ["src/**/*.elm"]`, treated as file patterns.
- **Table** — `files` (string[]) plus `artifacts`, a `string -> string` map of references such as
  `{ ir = "@morphir-elm/make:ir" }`.

The `@toolchain/task:output` reference syntax is what wires toolchains together: one toolchain's declared output
becomes another's input, without either naming a file path.

### Outputs

A map of named artifacts, `[toolchain.<tc>.tasks.<t>.outputs.<outputName>]`:

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `path` | string | Where the artifact lands |
| `type` | string | Artifact type |

Named outputs are what `artifacts` references resolve against.
