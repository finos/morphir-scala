---
type: Format
title: IR, Codegen, and Runtime
description: The morphir, ir, codegen, cache, logging, and ui configuration sections.
tags: [morphir, configuration, morphir-toml, codegen, ir]
status: draft
sources:
  - id: toml-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-specification.md
    title: Morphir TOML Configuration Specification — morphir, ir, codegen, cache, logging, ui
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# IR, Codegen, and Runtime

## `[morphir]`

| Field | Type | Meaning |
| ----- | ---- | ------- |
| `version` | string | SemVer constraint for compatible Morphir **IR** versions, e.g. `"^3.0.0"`. Empty means any |

## `[ir]`

| Field | Type | Default | Meaning |
| ----- | ---- | ------- | ------- |
| `format_version` | int | `3` | IR format version; supported range 1–10 |
| `strict_mode` | bool | `false` | Treat validation warnings as errors |

Two things worth noticing. The default is **3**, matching the current active format — see the `morphir-ir-v3` bundle.
And the supported range runs to **10**, which reserves room for v4 and beyond in a field that is still an integer,
even as the v4 design proposes moving `formatVersion` to a semver string.

`[morphir].version` and `[ir].format_version` overlap: one is a semver constraint on the IR version, the other an
integer format version. The specification does not say how they interact.

## `[codegen]`

| Field | Type | Default | Meaning |
| ----- | ---- | ------- | ------- |
| `targets` | string[] | — | Generation targets, e.g. `"go"`, `"typescript"`, `"scala"`, `"json-schema"` |
| `template_dir` | string | — | Custom templates directory |
| `output_format` | string | `"pretty"` | One of `pretty`, `compact`, `minified` |

The example target list includes `"go"`, which is not among the backends shipped by `morphir-elm` — a reminder that
this configuration format serves the polyglot toolchain, not one implementation.

## `[cache]`

| Field | Type | Default |
| ----- | ---- | ------- |
| `enabled` | bool | `true` |
| `dir` | string | empty means default |
| `max_size` | int64 | `0` — unlimited, in bytes |

## `[logging]`

| Field | Type | Default | Values |
| ----- | ---- | ------- | ------ |
| `level` | string | `"info"` | `debug`, `info`, `warn`, `error` |
| `format` | string | `"text"` | `text`, `json` |
| `file` | string | empty — stderr | Log file path |

## `[ui]`

| Field | Type | Default | Values |
| ----- | ---- | ------- | ------ |
| `color` | bool | `true` | |
| `interactive` | bool | `true` | |
| `theme` | string | `"default"` | `default`, `light`, `dark` |

The `ui` and `logging` sections are the clearest signal that `morphir.toml` is a *tool* configuration file: these
settings are the natural target of the user-override and environment-variable layers described in
[Merge Rules](/merge-rules.md).
