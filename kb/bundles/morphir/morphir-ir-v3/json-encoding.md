---
type: Format
title: JSON Encoding and Format Versions
description: The versioned JSON schemas, the v1 to v2 to v3 tag changes, and how to validate a Morphir IR file.
resource: https://morphir.finos.org/schemas/morphir-ir-v3.yaml
tags: [morphir, ir, v3, json, schema, versioning, validation]
status: stable
sources:
  - id: ir-spec
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/morphir-ir-specification.md
    title: Morphir IR Specification — JSON Schema Specifications
  - id: schemas-index
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/schemas/index.md
    title: Morphir IR JSON Schemas — format version differences
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# JSON Encoding and Format Versions

Morphir IR is distributed as JSON (conventionally `morphir-ir.json`). Formal JSON schemas exist for every supported
format version, published in both YAML (readable) and JSON (maximum tool compatibility).

## Format version history

The **format version** is a manually managed integer that tracks breaking changes to the JSON encoding. It is not the
same thing as the version of any tool. Version **3 is current**; version 4 is in draft.

### v1 → v2

- **Distribution tag**: `"library"` → `"Library"`
- **Access control**: `"public"` / `"private"` → `"Public"` / `"Private"`
- **Type tags**: lowercase → capitalized (`"variable"` → `"Variable"`, `"reference"` → `"Reference"`)
- **Module structure**: `{"name": ..., "def": ...}` objects → `[modulePath, accessControlled]` arrays

### v2 → v3

Purely tag capitalization, across the parts v2 had missed:

- **Value expression tags**: `"apply"` → `"Apply"`, `"lambda"` → `"Lambda"`
- **Pattern tags**: `"as_pattern"` → `"AsPattern"`, `"wildcard_pattern"` → `"WildcardPattern"`
- **Literal tags**: `"bool_literal"` → `"BoolLiteral"`

The practical summary: **v3 uses capitalized constructor tags throughout**. v2 capitalized distribution, access, and
type tags but left value, pattern, and literal tags in lowercase-with-underscores. v1 was lowercase everywhere and
used the object-based module structure.

## Schema locations

| Version | YAML | JSON |
| ------- | ---- | ---- |
| v3 (current) | `https://morphir.finos.org/schemas/morphir-ir-v3.yaml` | `https://morphir.finos.org/schemas/morphir-ir-v3.json` |
| v2 | `https://morphir.finos.org/schemas/morphir-ir-v2.yaml` | `https://morphir.finos.org/schemas/morphir-ir-v2.json` |
| v1 | `https://morphir.finos.org/schemas/morphir-ir-v1.yaml` | `https://morphir.finos.org/schemas/morphir-ir-v1.json` |

A v4 draft schema is published alongside these; see the v4 draft bundle's schema architecture concept.

## What the schemas are for

Validation of IR files, documentation of structure and constraints, code generation of parsers and serializers, and
building editors and linters.

## Validating an IR file

```bash
curl -o morphir-ir-v3.json https://morphir.finos.org/schemas/morphir-ir-v3.json
jsonschema validate morphir-ir-v3.json morphir-ir.json
```

That uses the `sourcemeta/jsonschema` CLI (`npm install -g @sourcemeta/jsonschema`, `brew install
sourcemeta/apps/jsonschema`, or `pip install jsonschema-cli`). `ajv-cli` works equally well against the JSON schema;
Python's `jsonschema` plus `pyyaml` can consume the YAML form directly.

## v4 divergence

v4 replaces the single monolithic schema with separate root schemas sharing common `$ref` definitions, splits Classic
from Document Tree mode, and moves names, paths, and FQNames from arrays to canonical strings. See the v4 draft
bundle.
