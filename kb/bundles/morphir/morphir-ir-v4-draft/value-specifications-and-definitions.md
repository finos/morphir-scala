---
type: Specification Section
title: Value Specifications and Definitions
description: The four ValueDefinitionBody variants and the single ValueSpecification they all derive.
tags: [morphir, ir, v4, draft, values, specifications, definitions]
status: draft
stale_after: 2026-12-31
sources:
  - id: values
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft/values.md
    title: Value System (IR v4 draft) — Value Definitions, Value Specifications
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Value Specifications and Definitions

## Value Definition

A **Value Definition** is the complete implementation of a value or function, owned by the defining module and wrapped
in `AccessControlled` (`Public` exposes it to dependents; `Private` does not).

v4 factors the implementation into a **`ValueDefinitionBody`** with four variants. This is the change that lets the
IR describe values that have no IR body at all.

### ExpressionBody

`ExpressionBody inputTypes outputType body` — a normal IR expression implementation. This is v3's behavior.

- `inputTypes` — list of `(Name, Type)` parameters
- `outputType` — return type
- `body` — the [value expression](/value-expressions.md)

### NativeBody (v4)

`NativeBody inputTypes outputType nativeInfo` — a builtin with no IR body. See
[Native and External Values](/native-and-external-values.md).

### ExternalBody (v4)

`ExternalBody inputTypes outputType externalName targetPlatform` — an FFI call with no IR body.

### IncompleteBody (v4)

`IncompleteBody inputTypes outputType incompleteness partialBody` — a best-effort definition. Note that `outputType`
here is **optional**, unlike the other three variants, because an incomplete definition may not have a known return
type yet. `partialBody` is an optional partial implementation. See [Incompleteness](/incompleteness.md).

## Value Specification

`ValueSpecification inputs output` — only the signature.

- `inputs` — list of `(Name, Type)` parameters
- `output` — return type

## The crucial derivation property

**All four body variants derive the same `ValueSpecification` structure.** A specification is produced by extracting
`inputTypes` and `outputType` and discarding everything else — `body`, `nativeInfo`, `externalName`, incompleteness.

The consequence is worth stating plainly: **consumers cannot distinguish how a value is implemented.** A dependent
module type-checking against `morphir/(sdk):basics#add` cannot tell whether it is IR, a platform builtin, an FFI call,
or a half-finished draft. That is the point — it keeps implementation strategy from leaking across module boundaries.
