---
type: Playbook
title: Implementing Morphir Tools
description: Guidance for tools that generate, consume, or transform Morphir IR at format version 3.
tags: [morphir, ir, v3, tooling, playbook]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Usage Guidelines for Tool Implementers
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Implementing Morphir Tools

Three roles, three checklists. All of them assume the structures described in [Morphir IR
Overview](/overview.md).

## Generating IR from source

1. **Preserve names in canonical form** — convert every identifier to a lowercase word list. Splitting acronyms into
   single-letter words is what lets renderers produce `valueInUSD` later. See [Naming](/naming.md).
2. **Use fully-qualified references** — always include package and module paths. Never emit a bare local name in an
   expression.
3. **Maintain access control** — mark public versus private correctly; this is what the public specification is
   derived from.
4. **Extract lambdas into function parameters** — populate `inputTypes` on the definition rather than emitting a chain
   of nested `Lambda` nodes. See [Value Specifications and Definitions](/value-specifications-and-definitions.md).
5. **Preserve documentation** — carry doc strings from source into the `Documented` wrappers.

## Consuming IR

1. **Respect access control** — access only public items from dependencies.
2. **Resolve references** — use the [Distribution](/distribution.md) to look up type and value definitions by FQName.
3. **Handle attributes** — be prepared for different attribute types, or for the unit type used as a placeholder.
4. **Follow naming conventions** — use name conversion utilities to render identifiers for the target platform.
5. **Process hierarchically** — Distribution → Package → Module → Types/Values.

## Transforming IR

1. **Preserve structure** — keep the hierarchical organization intact.
2. **Update references consistently** — renaming an item means rewriting every FQName that points at it.
3. **Maintain type correctness** — transformations must preserve type safety.
4. **Handle both specifications and definitions** — transform both forms consistently, or the derived specification
   will drift from the definition.
5. **Preserve attributes** — carry them forward unless the transformation is explicitly about changing them.

## Validate the output

Whatever the role, validate against the published schema before shipping — see
[JSON Encoding and Format Versions](/json-encoding.md). A format-version mismatch is the most common source of
"mysteriously rejected" IR, because the v1/v2/v3 differences are largely tag capitalization and fail late.
