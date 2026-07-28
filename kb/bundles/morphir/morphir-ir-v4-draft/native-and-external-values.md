---
type: Concept
title: Native and External Values
description: First-class representation of platform builtins and foreign function calls in v4.
tags: [morphir, ir, v4, draft, native, ffi, interop]
status: draft
stale_after: 2026-12-31
sources:
  - id: values
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/values.md
    title: Value System (IR v4 draft) — Native, External, NativeInfo, NativeHint
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Native and External Values

v3 had no way to say "this function is implemented by the platform, not by IR". v4 adds two: **Native** for platform
builtins, **External** for foreign function calls. Both exist as value expression nodes and as
[value definition bodies](/value-specifications-and-definitions.md).

## Native

A platform operation with no IR body.

- **Expression**: `Native attributes fqName nativeInfo`
- **Body**: `NativeBody inputTypes outputType nativeInfo`

### NativeInfo

`NativeInfo hint description` — a categorization hint plus an optional human-readable description.

### NativeHint

| Hint | Covers |
| ---- | ------ |
| `Arithmetic` | Basic arithmetic or logic operation |
| `Comparison` | Comparison operation |
| `StringOp` | String operation |
| `CollectionOp` | Collection operation — map, filter, fold, and so on |
| `PlatformSpecific` | Platform-specific operation; includes a platform identifier |

The hint is what lets a backend recognize `add` as arithmetic and emit a native `+` rather than a function call,
without hardcoding a list of SDK FQNames.

## External

A foreign function interface call.

- **Expression**: `External attributes externalName targetPlatform`
- **Body**: `ExternalBody inputTypes outputType externalName targetPlatform`

`externalName` names the external function; `targetPlatform` identifies which platform it belongs to — which means an
IR can carry several platform-specific implementations and a backend can select the one it can honor.

## Boundary behavior

Neither is visible in a specification. Both `NativeBody` and `ExternalBody` derive an ordinary `ValueSpecification`
containing only inputs and output, so a dependent module sees a plain signature and cannot tell how the value is
implemented.
