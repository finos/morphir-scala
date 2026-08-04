---
type: Concept
title: Specification and Design Divergences
description: Where the v4 spec draft, design documents and JSON schemas disagree — what the schemas settle, and what stays open.
tags: [morphir, ir, v4, draft, divergences, open-questions, json-schema]
status: draft
stale_after: 2026-12-31
sources:
  - id: design-ir
    resource: https://github.com/finos/morphir/tree/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/design/draft/ir
    title: docs/design/draft/ir (design documents)
  - id: spec-draft
    resource: https://github.com/finos/morphir/tree/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/draft
    title: docs/spec/draft (specification draft)
  - id: spec-ir
    resource: https://github.com/finos/morphir/tree/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/docs/spec/ir/schemas/v4
    title: docs/spec/ir/schemas/v4 (published v4 spec tree)
  - id: schema-classic
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4.yaml
    title: morphir-ir-v4.yaml (the v4 distribution schema)
  - id: schema-document-tree
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4-document-tree-files.yaml
    title: morphir-ir-v4-document-tree-files.yaml (the document tree file schemas)
generated:
  by: process:kb-seed
  at: 2026-07-28T00:00:00Z
---

# Specification and Design Divergences

There are four bodies of material about v4, not two: the [specification draft](/index.md), the
[design documents](/design/index.md), the published spec tree at `docs/spec/ir/schemas/v4/` — larger and more
detailed than the draft — and the two JSON schemas described in [Schema Architecture](/schema-architecture.md).

The schemas are the tiebreak, and they speak far more often than the first reading of this bundle assumed. Most of
the encoding disagreements below are settled. What replaces them is a smaller, sharper set of problems: the schemas
share no `$ref` with each other, so their vocabularies drift, and in four places a schema contradicts itself.

## `formatVersion` is a `oneOf`, not a disagreement

Every v4 file carries `formatVersion`, and both schemas accept either a semver string or an integer:

| Schema | String arm | Integer arm |
| ------ | ---------- | ----------- |
| `morphir-ir-v4.yaml`, root property | `pattern: "^4\\.0\\.0(-[a-zA-Z0-9.-]+)?(\\+[a-zA-Z0-9.-]+)?$"` | `type: integer`, `minimum: 4` |
| `morphir-ir-v4-document-tree-files.yaml`, all three occurrences | the same pattern | `type: integer`, `const: 4` |

Both schemas describe the integer arm as `"Legacy integer format for backwards compatibility"`. The string arm
carries no such qualifier and is what every non-legacy example writes. So the spec draft's `"formatVersion": 4` and
the design's `"formatVersion": "4.0.0"` are both valid input; the design's is what an encoder should emit.

The real inconsistency is *between the two schemas*: `minimum: 4` admits `5`, `const: 4` does not. So a hypothetical
v5 blob passes the v4 root schema's version check and fails the document tree one. Nothing turns on it today, but the
two arms were meant to be the same rule and are not.

## Encoding questions the schemas settle

| Subject | Spec draft | Design | What the schema says |
| ------- | ---------- | ------ | -------------------- |
| Parameterized type shorthand | `{"Reference": ["morphir/sdk:list#list", "a"]}` | Bare array `["morphir/sdk:list#list", "a"]` | **Spec draft.** `ReferenceType` accepts a bare FQName string, `{"Reference": "fq"}`, `{"Reference": [fq, args…]}` and `{"Reference": {"fqname", "args"}}` — never a bare array. `TupleType` claims the bare array, "unambiguous since Reference doesn't use bare arrays" |
| Tuple type | `{"Tuple": {"elements": [...]}}` | Canonical `{"Tuple": [...]}`; object form "expanded" | **Design.** `"Canonical: {"Tuple": [type1, type2, ...]}"`; the `elements` object is the expanded form, and a bare array is accepted too |
| Record type fields | Fields directly under `Record` | Same, plus a legacy `fields` array on decode | **Spec draft.** `RecordType` accepts only the direct field map; the design's legacy `fields` array is not in the schema |
| Value shorthand | Values **always** use object wrappers | Bare `42` and `true` permitted when attributes are empty | **Design.** `Value` is a `oneOf` whose first five arms are a bare boolean, a bare number, a bare FQName string, a bare Name string and a bare array |
| `DerivedTypeSpecification` | Prose says `typeParams details`, but its JSON examples are flat | Nests a `DerivedTypeSpecificationDetails` under `details` | **Flat.** The schema requires `["typeParams", "baseType", "fromBaseType", "toBaseType"]` directly. The string `details` does not occur anywhere in either schema |
| Value `List` | `{"List": {"items": [...]}}` | Canonical `{"List": [v1, v2]}`; object form "expanded" | **Design.** `"Canonical: {"List": [value1, value2, ...]}"`, with `{"List": {"items": […]}}` accepted. `elements` is `TupleValue`'s expanded key and never `List`'s — `{"List": {"elements": …}}` is not a form anything accepts |
| `AccessControlled` | Flattened `{"access": "Public", ...}` | Canonical is the tag form `{"Public": {...}}` | **Neither, cleanly** — the schema declares the tag form canonical and then writes the flattened form in its own examples. See the contradictions below |

## Document Tree questions the schemas settle

| Subject | Spec draft | Design | What the schema says |
| ------- | ---------- | ------ | -------------------- |
| `module.json` path key | `"module": "main/domain"` | `"path": "my-org/domain"` | **Both.** `ModuleManifestFile` declares `path` and `module` and states "The 'path' field is preferred; 'module' is accepted for backwards compatibility" |
| `module.json` contents | Manifest style has metadata only; inline style embeds definitions | A manifest listing type and value **names** | **Both styles.** `types` and `values` are each a `oneOf` of an array of names (manifest) or an object of definitions (inline) — "but not mixed" |
| Node file header | Not shown | `formatVersion` and `name` precede `def` / `spec` | **Design.** `VfsNodeHeader` has `required: ["formatVersion", "name"]`, and `name` "matches the filename (without the .type.json or .value.json suffix)" |

## Model questions the schemas settle

- **Annotations.** Not a design-only feature. `morphir-ir-v4.yaml` defines `Annotation` (a compact
  `"fqname:value"` string or an object with `name` and `arguments`) and `AnnotationArgument`, and hangs an optional
  `annotations` array off `ModuleSpecification`, `ValueSpecification` and all four type specification variants —
  specifications only, never definitions, exactly as the design has it. The spec draft is simply behind the schema.
- **Module documentation.** Settled for the spec draft. `doc: { type: string }` appears on `ModuleDefinition`,
  `ModuleSpecification` and `ModuleManifestFile`. The design's stated decision — "module-level docs in separate
  metadata", meaning [decorations](/design/decorations.md) — is not what the schema does.
- **Application dependencies.** Settled for the design. `ApplicationDependencies` maps package names to
  `PackageDefinition`, described as "Unlike Library dependencies which use specifications, Application dependencies
  include full implementations for static linking."
- **Package version.** Settled only for the Document Tree. `DistributionManifestFile` carries an optional semver
  `version`. `morphir-ir-v4.yaml` has no version field anywhere: `LibraryDistribution`, `SpecsDistribution` and
  `ApplicationDistribution` carry `packageName`, `dependencies`, `def` and — for Application — `entryPoints`, and
  nothing else. So a Classic blob has nowhere to put the design's `PackageInfo` version.

## `manifest.json` versus `format.json`

Distribution metadata has two names and two shapes, and nothing upstream reconciles them:

| Named by | File | Contents |
| -------- | ---- | -------- |
| `docs/spec/draft/distribution.md` | `.morphir-dist/format.json` | "Layout metadata and spec version" |
| `docs/spec/ir/schemas/v4/document-tree-files.md`, and the schema | `.morphir-dist/manifest.json` | `formatVersion`, `distribution` and `package` required; `version`, `created`, `layout`, `entryPoints` optional |

The document tree schema is unambiguous on its side: its header lists "manifest.json: Distribution-level metadata",
and `DistributionManifestFile`'s description repeats "Located at: `.morphir-dist/manifest.json`". The never-built
schema hierarchy meanwhile promised a `tree/format.yaml` that "validates the `format.json` file at the root of a
distribution" — so the file the spec draft names is the one no schema describes.

A reader with a `.morphir-dist/` in hand has to guess, and the two files are not even the same file: `format.json`
records layout, `manifest.json` records distribution identity.

## Contradictions inside the schemas themselves

The two schemas share no cross-file `$ref` — the document tree one restates the core vocabulary locally with the
comment "these would be `$ref` in practice". Written twice, they have drifted, and each has drifted from itself:

- **Bare arrays as values.** `ListValue`'s description says "Note: Bare arrays are NOT allowed for values (would be
  ambiguous with TupleValue)." The `Value` definition that references it lists `type: array` as a "Shorthand for
  List". Same file, opposite rules.
- **Access on a definition.** `AccessControlled` is a `oneOf` of the tag form (`{"Public": {...}}`, with
  `additionalProperties: false`) and the legacy `{ "access": …, "value": … }`. `ModuleDefinition.types` is an `allOf`
  over it — yet its own description and examples write `{ "user": { "access": "Public", "TypeAliasDefinition": {…} } }`,
  which matches neither arm: the tag arm forbids the extra keys, the legacy arm requires `value`. **The schema's
  stated V4 module encoding does not validate against the schema.** `TypeDefinitionFile.def` in the document tree
  schema uses that same flattened shape, so it is the shape implementations will actually meet.
- **Record fields.** `RecordType` accepts only a direct field map. The document tree schema's one `Record` example
  writes `{"Record": {"fields": {"email": …}}}` — rejected by `RecordType`, and not the design's legacy `fields`
  *array* either. A third shape, in a schema that cannot check it because its `TypeDefinition` is an
  `additionalProperties: true` stub.
- **`formatVersion`'s legacy arm** — `minimum: 4` against `const: 4`, above.

These matter more than the spec-versus-design divergences: a schema is what a validator runs.

## Still open

- **Definition file location.** The schemas describe file contents and never directory layout, so they do not settle
  it. The tally is 3–1 for definition files sitting flat in the module directory — `modules.md` ("reside directly in
  the module directory"), `names.md`'s URI example, and all four directory diagrams in `document-tree-files.md`,
  against `distribution.md`'s `types/` and `values/` subdirectories, which the design follows. See
  [Document Tree Layout](/document-tree-layout.md).
- **Root metadata file name** — `manifest.json` or `format.json`, above.
- **Distribution root extras.** `session.jsonl` (a transaction journal) and `deco/` (layered decorations) appear only
  in the design. No spec document and no schema mentions either.

## Present in the design, absent from the specification and the schemas

Four features, not five — annotations reached the schema, as above:

- [Layered decorations](/design/decorations.md) — the `deco/` tree
- [Document type](/design/document-type.md) — schema-less JSON-like values, adding a `DocumentLiteral`
- [`$meta`](/design/file-metadata.md) — file-level provenance
- [`$ref`](/design/node-references.md) — file-local deduplication

The `Document` type is the consequential one: it adds a variant to `Literal`, so it changes the value model rather
than sitting beside it. `morphir-ir-v4.yaml` defines `Literal` with six variants and no `DocumentLiteral` among them.

## Terminology

The design says **VFS mode**; the spec draft says **Document Tree mode**. Same concept. The design's schema tree
names `schemas/v4/vfs/` where the spec draft writes `schemas/v4/tree/` — and neither directory was ever created, as
[Schema Architecture](/schema-architecture.md) records. The schema keeps both words at once: its title is "Morphir IR
V4 Document Tree File Formats", its prose opens "In VFS (Virtual File System) mode", its shared header definition is
`VfsNodeHeader`, and the `layout` field of a distribution manifest takes `"Classic"` or `"VfsMode"`.

## A separate inconsistency inside the spec draft, now settled

The spec draft's value system names the literal `IntegerLiteral` in its literal table, then writes
`{"Literal": {"IntLiteral": 42}}` in every JSON example. `IntLiteral` occurs **zero** times in either schema;
`IntegerLiteral` is a defined variant, annotated "V4 renamed from WholeNumberLiteral for correctness" — which is the
design's account exactly. The spec draft's examples are an error.

## How to treat all of this

The design documents are ahead of the spec draft and carry more detail, but the design's own status table marks
nothing in IR v4 as Approved or Complete — see [v4 Architecture](/design/architecture.md). Neither prose document is
authoritative over the other, and the schemas now answer most of the encoding questions between them.

The schemas are not a clean authority either. They contradict themselves in the four places above, they validate
almost nothing inside a Document Tree node file, and they are silent on directory layout. Where a schema speaks
consistently, follow it. Where it contradicts itself or says nothing, the question is genuinely open.
