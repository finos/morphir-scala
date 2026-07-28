---
type: Specification Section
title: Merge Rules
description: The six configuration sources, their precedence order, and the deterministic deep-merge algorithm.
tags: [morphir, configuration, morphir-toml, merge, precedence]
status: draft
sources:
  - id: merge-rules
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/morphir-toml/morphir-toml-merge-rules.md
    title: Morphir TOML Configuration Merge Rules
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Merge Rules

Morphir configuration is **layered**: several sources are loaded and merged into one **effective configuration**.

## Sources and precedence

Lowest to highest:

| Priority | Source | Typical path |
| -------- | ------ | ------------ |
| 1 (lowest) | Built-in defaults | compiled in |
| 2 | System config | `/etc/morphir/morphir.toml` |
| 3 | Global user config | `~/.config/morphir/morphir.toml` |
| 4 | Project config | `morphir.toml` |
| 5 | User override | `.morphir/morphir.user.toml` |
| 6 (highest) | Environment variables | `MORPHIR_*` |

A hidden project config (`.morphir/morphir.toml`) may also be used by some commands; its merge semantics are
identical.

The user-override layer sitting *above* the project config is the notable choice — it lets a developer change
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
| 1 — Overlay wins | For a key in both maps, the overlay value takes precedence |
| 2 — Maps merge recursively | Two maps under the same key are deep-merged |
| 3 — **Arrays replace** | The overlay array replaces the base entirely; no concatenation |
| 4 — `nil` overlay ignored | A `nil` overlay value does not override the base |
| 5 — No mutation | The result is independent; inputs are unmodified |

Rule 3 is the one that surprises people: a project-level `codegen.targets` **replaces** the global list rather than
adding to it. Note the contrast with the v4 design's decoration merge, which concatenates arrays — the two merge
systems are unrelated and behave differently.

Rule 4 means a key cannot be *unset* by a higher layer, only overwritten with another value.

These rules are implemented by `pkg/config/internal/configloader.DeepMerge` and `MergeAll`.

## Environment variable mapping (informative)

Variables prefixed `MORPHIR_` (the default prefix) become config keys.

- **Double underscore** marks a nested object boundary:
  `MORPHIR_CODEGEN__GO__PACKAGE=foo` → `codegen.go.package = "foo"`
- **Single underscores are not split.** They stay part of the key name at that level:
  `MORPHIR_IR_FORMAT_VERSION=3` → `ir_format_version = 3`, a single key in the env-derived map.

The second bullet is a trap. `MORPHIR_IR_FORMAT_VERSION` does **not** set `ir.format_version`; it creates a key named
`ir_format_version` that nothing reads. The correct form is `MORPHIR_IR__FORMAT_VERSION`.

The specification is explicit that this is deliberate — the mapping is "intentionally mechanical; it does not attempt
to guess dotted paths."
