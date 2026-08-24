---
type: Specification Section
title: Merge Rules
description: The six configuration sources, their precedence order, and the deterministic deep-merge algorithm.
tags: [morphir, configuration, morphir-toml, morphir-yaml, merge, precedence]
status: draft
sources:
  - id: merge-rules
    resource: https://github.com/finos/morphir/blob/4d2a6d836da1c3a114241e911f1af0f38b97b453/docs/spec/morphir-toml/morphir-toml-merge-rules.md
    title: Morphir Configuration Merge Rules
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Merge Rules

Morphir configuration is layered. The normative specification loads six sources and merges them into one effective
configuration. Each file source may use TOML or YAML. The algorithm starts after parsing, so serialization does not
change precedence or merge behavior.

## Baseline

This reference uses the merge specification at `finos/morphir` commit
`4d2a6d836da1c3a114241e911f1af0f38b97b453`. Re-read that pinned document and the TOML and YAML specifications when
the pin moves. The [Rust implementation reference](/morphir-rust-configuration-cdfa6c63.md) tracks implemented
coverage separately.

## Sources and precedence

Lowest to highest:

| Priority | Source | Typical path |
| -------- | ------ | ------------ |
| 1 (lowest) | Built-in defaults | compiled in |
| 2 | System config | `/etc/morphir/morphir.toml` or `.yaml` |
| 3 | Global user config | Platform config directory or home `.morphir` directory |
| 4 | Project config | `morphir.toml` or `morphir.yaml` |
| 5 | User override | `.morphir/morphir.user.toml` or `.yaml` |
| 6 (highest) | Environment variables | `MORPHIR_*` |

A hidden project config is an alternate project path, not another layer. The serialization and location ambiguity
rules are summarized in [Configuration Overview](/overview.md).

The user-override layer sits above the project config. It lets a developer change
behavior locally without editing a file that is under version control.

## The merge algorithm

Each source becomes a nested `map[string]any`. The effective configuration applies `DeepMerge` from low precedence to
high:

```
effective = DeepMerge(
  DeepMerge(
    DeepMerge(defaults, system),
    global
  ),
  project
)
... then user overrides, then environment variables
```

Generally: **later maps take precedence over earlier maps.**

### DeepMerge rules (normative)

| Rule | Behavior |
| ---- | -------- |
| 1. Overlay wins | For a key in both maps, the overlay value takes precedence |
| 2. Maps merge recursively | Two maps under the same key are deep-merged |
| 3. **Arrays replace** | The overlay array replaces the base entirely; no concatenation |
| 4. `nil` overlay ignored | A `nil` overlay value does not override the base |
| 5. No mutation | The result is independent; inputs are unmodified |

Rule 3 is the one that surprises people: a project-level `codegen.targets` **replaces** the global list rather than
adding to it. The v4 design's decoration merge concatenates arrays. That merge system is unrelated and behaves
differently.

Rule 4 means a key cannot be *unset* by a higher layer, only overwritten with another value.

The specification attributes these rules to `pkg/config/internal/configloader.DeepMerge` and `MergeAll`. The pinned
Rust implementation has a separate recursive merge with the same map and replacement behavior for the layers it
loads. It does not implement all six layers. See the
[Rust implementation reference](/morphir-rust-configuration-cdfa6c63.md).

## Environment variable mapping (informative)

Variables prefixed `MORPHIR_` (the default prefix) become config keys.

| Syntax | Behavior | Example |
| --- | --- | --- |
| Double underscore | Starts a nested object boundary | `MORPHIR_CODEGEN__GO__PACKAGE=foo` becomes `codegen.go.package = "foo"` |
| Single underscore | Remains part of the key at that level | `MORPHIR_IR_FORMAT_VERSION=3` becomes the single key `ir_format_version = 3` |

The `MORPHIR_IR_FORMAT_VERSION` row is a trap. That variable does **not** set `ir.format_version`; it creates a key named
`ir_format_version` that nothing reads. The correct form is `MORPHIR_IR__FORMAT_VERSION`.

The specification calls this mapping "intentionally mechanical" and says it does not guess dotted paths.
