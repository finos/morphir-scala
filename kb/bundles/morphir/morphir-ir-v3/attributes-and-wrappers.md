---
type: Specification Section
title: Attributes and Wrappers
description: Type and value attributes, the AccessControlled wrapper, and the Documented wrapper.
tags: [morphir, ir, v3, attributes, access-control, documentation]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — Attributes and Annotations, Access Control
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Attributes and Wrappers

Three mechanisms decorate IR nodes without changing their structure: attributes carry extra information on type and
value nodes, `AccessControlled` carries visibility, and `Documented` carries documentation.

## Attributes

The IR is extensible through attributes attached to nodes:

- **Type attributes (`ta`)** — extra information on type nodes, such as source location or type inference results.
- **Value attributes (`va`)** — extra information on value nodes, such as source location or inferred types.

In v3 these are **generic parameters**: `Type a` and `Value ta va` are parameterized over the attribute type, and
every node carries one. When no extra information is needed, the unit type `()` is used as a placeholder — which is
why so much v3 IR in the wild is `Type ()` / `Value () ()`.

> **v4 divergence.** This generic parameterization is removed in v4, replaced by explicit `TypeAttributes` and
> `ValueAttributes` structures with a standard schema. It is the single largest structural change between the
> versions. See the v4 draft bundle's attributes concept.

## AccessControlled

An **AccessControlled** wrapper manages visibility.

- **Structure**: `{ access, value }`
- **Access levels**:
  - **Public** — visible to external consumers of the package.
  - **Private** — visible only within the package.

It wraps modules inside a package definition, and types and values inside a module definition. It is what drives
[specification derivation](/specification-vs-definition.md): private items are dropped when the public specification
is computed.

## Documented

A **Documented** wrapper associates documentation with an IR element.

- **Structure**: `{ doc, value }`
  - `doc` — a documentation string.
  - `value` — the documented element.

Documentation is preserved from source through compilation, so generated code and browsable IR can both carry the
original prose. In a module, types and values are wrapped in both `AccessControlled` and `Documented`.
