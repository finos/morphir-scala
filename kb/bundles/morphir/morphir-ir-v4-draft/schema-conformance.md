---
type: Specification Section
title: Schema Conformance
description: "What validators actually report when run against the v4 schemas and documents — every complete v4 document upstream ships fails upstream's own v4 schema."
tags: [morphir, ir, v4, draft, json-schema, validation, conformance]
status: draft
stale_after: 2026-12-31
sources:
  - id: schema-classic
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4.yaml
    title: morphir-ir-v4.yaml (the v4 distribution schema)
  - id: schema-document-tree
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/schemas/morphir-ir-v4-document-tree-files.yaml
    title: morphir-ir-v4-document-tree-files.yaml (the document tree file schemas)
  - id: example-complete
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/ir/examples/v4/complete-example.json
    title: complete-example.json (published v4 example)
  - id: example-books
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/website/static/ir/examples/v4/books-and-records-example.json
    title: books-and-records-example.json (published v4 example)
  - id: fixture-library
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/tests/bdd/fixtures/ir/v4/v4-library-distribution.json
    title: v4-library-distribution.json (BDD distribution fixture)
  - id: upstream-tasks
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/.config/mise/config.toml
    title: ".config/mise/config.toml (upstream's fmt, lint and check tasks)"
  - id: upstream-ci
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/.github/workflows/ci.yml
    title: ".github/workflows/ci.yml (upstream's GitHub Actions)"
  - id: upstream-validate-docs
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/.config/mise/tasks/ci/validate_docs.py
    title: "validate_docs.py (the docs job's entire contents)"
  - id: upstream-fixtures-validate
    resource: https://github.com/finos/morphir/blob/4d5e5c06a7cf269c5f86b050a16a6f82bb5c29bc/.config/mise/tasks/fixtures/validate.py
    title: fixtures/validate.py (validates two directories that do not exist)
generated:
  by: process:schemas-check
  at: 2026-08-02T00:00:00Z
---

# Schema Conformance

**Every complete v4 document `finos/morphir` publishes fails validation against the v4 schema `finos/morphir`
publishes.** All three of them, for three different reasons. So when a v4 question comes down to "what does upstream
say", there is no single answer: the schema and the documents are two sources that disagree, and any statement about
v4 has to name which one it is following.

[Specification and Design Divergences](/design/divergences.md) reached part of this by reading — it predicted from
the schema text that the module encoding upstream writes cannot validate. This is the confirmation, plus three
further defects reading did not find, plus one kind of finding reading could not produce at all — what upstream's CI
actually executes, as against what its task definitions say it would.

Everything below was produced by running `jsonschema` 16.3.0 (sourcemeta), the tool upstream itself pins, against the
mirror in the sibling `morphir/morphir-upstream` bundle and against a reference checkout at commit `4d5e5c06`.
[Schema Architecture](/schema-architecture.md) describes the two schema files these results are about.

## The evidence

| Check | Command | Result |
| ----- | ------- | ------ |
| YAML→JSON generation | `schemas-to-json.ts --check` against upstream's committed `.json` | 5 of 5 **identical** |
| Schemas against their metaschema | `jsonschema metaschema website/static/schemas/*.yaml` | 6 of 7 pass; `morphir-ir-v4-document-tree-files.yaml` **fails** |
| Complete v4 documents against `morphir-ir-v4.json` | `jsonschema validate` | 0 of 3 pass |
| Complete v3 documents against `morphir-ir-v3.yaml` | upstream's `examples:validate` | 4 of 4 pass |
| Committed JSON schemas are canonical | `jsonschema fmt --check` | 0 of 7 pass |

The v3 row is the control. v3 documents validate against the v3 schema with the same tool, so nothing here is an
artifact of the validator, of the YAML-to-JSON conversion, or of draft-07. The failures are v4's.

## The generator is faithful, so the YAML is the whole story

Upstream commits both `.yaml` and `.json` for every schema and generates the JSON from the YAML with
`website/scripts/yaml-to-json-schemas.js` during its Netlify build. Our reimplementation of that generator produces
bytes identical to all five committed `morphir-ir-*.json` files. Two things follow: upstream's YAML and its committed
JSON are currently in step, so neither is stale relative to the other; and validating against generated JSON is the
same as validating against the mirrored YAML. Only the YAML needs reading or editing.

## The document tree schema is not a valid schema

`morphir-ir-v4-document-tree-files.yaml` does not satisfy draft-07:

```
The value was expected to be of type boolean, or object but it was of type string
  at instance location "/definitions/ValueSpecification/properties/description" (line 83, column 7)
```

The cause is a two-space YAML indentation slip:

```yaml
      output: {}
      description: "Output type"
```

`output: {}` closed the mapping, so `description` became a *sibling property* of `ValueSpecification` whose schema is
the string `"Output type"` — and a string is not a schema. The intent was to document `output`. Nothing is lost
except the documentation: `output: {}` constrains nothing either way, so re-indenting the line changes no rule. It is
the smallest possible upstream fix, and it is the difference between a file that is a JSON Schema and one that is not.

The other six schemas — v1, v2, v3, v4, config and project — pass.

## Three documents, three unrelated failures

| Document | `formatVersion` | Why it fails |
| -------- | --------------- | ------------ |
| `website/static/ir/examples/v4/complete-example.json` | `"4.0.0"` | Two causes: flattened access on a type definition, and `{"OpaqueTypeSpecification": {}}` |
| `website/static/ir/examples/v4/books-and-records-example.json` | `"4.0.0"` | One cause: `{"OpaqueTypeSpecification": {}}` |
| `tests/bdd/fixtures/ir/v4/v4-library-distribution.json` | `4` | `distribution` is a v3 tagged array, not an object |

Each was isolated by deleting one subtree at a time and revalidating, because `jsonschema` reports every branch of a
`oneOf` it tried — including branches whose failure is expected and harmless. Reading the raw error list will
attribute failures to the wrong node.

### Flattened access on a definition — confirmed

`AccessControlled` is a two-arm `oneOf`: the tag form (itself a five-way `oneOf` over `Public`, `Private`, `public`,
`private`, `pub`, each with `additionalProperties: false`) and the legacy `{ "access": …, "value": … }`. Validated
directly against that definition:

| Shape | Result |
| ----- | ------ |
| `{"Public": {…}}` | passes — tag arm |
| `{"access": "Public", "value": {…}}` | passes — legacy arm |
| `{"access": "Public", "TypeAliasDefinition": {…}}` | **fails** — the tag arm forbids the extra keys, the legacy arm requires `value` |

The third is what `complete-example.json` writes for every entry in a module's `types` map, and it is exactly the
shape [Divergences](/design/divergences.md) flagged from reading the schema's own description and examples. The
document tree schema's `TypeDefinitionFile.def` uses the same flattened shape, so it is what an implementation
reading a `.type.json` will actually meet.

`books-and-records-example.json` writes the canonical `{"Public": {…}}` at both module and type level and passes on
this point. **The two published examples encode access differently from each other.** Nothing would let that stand if
anything validated them.

### `{"OpaqueTypeSpecification": {}}` fails its own schema

`OpaqueTypeSpecification`'s inner `oneOf` has two arms: one object permitting `annotations` and `typeParams`, and one
with `additionalProperties: false` permitting only `annotations`. An empty object satisfies **both**, so a `oneOf`
rejects it:

| Shape | Result |
| ----- | ------ |
| `{"OpaqueTypeSpecification": {}}` | **fails** — matches 2 of 2 arms |
| `{"OpaqueTypeSpecification": {"typeParams": ["a"]}}` | passes |
| `["OpaqueTypeSpecification", []]` | passes — legacy array form |

The failing shape is the schema's own first `examples` entry and the first form its own description documents, and it
is how both published examples spell every opaque type in the `morphir/sdk` dependency graph — `int`, `float`, `bool`
and the rest. Changing the outer `oneOf` to `anyOf`, or deleting the redundant second arm, fixes it.

This is a self-contradiction [Divergences](/design/divergences.md) does not list. Its four came from reading; this one
only shows up when something runs.

### The BDD fixture is a v3 payload with a v4 stamp

`v4-library-distribution.json` declares `"formatVersion": 4` and then writes

```json
"distribution": ["Library", "example/v4-test", {}, { "modules": [ ["domain", …] ] }]
```

— the v3 tagged-array encoding, arrays-of-pairs for maps and all. `morphir-ir-v4.yaml` admits only an object keyed
`Library`, `Specs` or `Application`, so the document fails at `/distribution` and nowhere else. This is not an
encoding disagreement about a corner of the format; it is a whole document in the wrong format.

### What is not a document

The other five fixtures under `tests/bdd/fixtures/ir/v4/` — `literal-examples`, `hole-reason-examples`,
`incompleteness-examples`, `incomplete-type-definition-example`, `native-hint-examples` — are keyed by example name
(`description`, `examples`, `v4CanonicalFormat`, …) and carry no `formatVersion`. They are fragments illustrating one
node kind, not whole distributions, and no root schema applies to them. `website/static/ir/examples/v4/index.json` is
a catalog, which upstream's own task excludes by name. Nine `.json` files, three documents.

## No committed schema is canonically formatted, and the formatter cannot see the YAML

All seven committed `.json` schemas fail `jsonschema fmt --check` run directly against them. That alone would only be
untidy. What makes it worth recording is that upstream's tasks cannot be reporting it:

```toml
[tasks."fmt:schema"]
run = "jsonschema fmt website/static/schemas/"

[tasks."fmt-check:schema"]
run = "jsonschema fmt website/static/schemas/ --check"
```

That directory holds both `.yaml` and `.json`, and in 16.3.0 `jsonschema fmt` refuses YAML outright —
*"This command does not support YAML input files yet"*. Run as written, the check reports the first non-canonical
JSON file, hits the first YAML file, and aborts with exit 3. It has never formatted anything and cannot.

Nor is that the only red task. Of the five `mise run check` depends on, at the pinned commit:

| Task | Exit | What it does |
| ---- | ---- | ------------ |
| `fmt-check:schema` | 3 | aborts on the first YAML file |
| `lint:schema` | 2 | fails |
| `schema:validate` | 2 | catches the document tree metaschema failure above |
| `examples:validate` | 1 | catches both published v4 examples |
| `fixtures:validate` | 0 | validates nothing — see below |

`.husky/pre-push` gates a push on `mise run fmt-check`, so either that hook is not installed or it is routinely
bypassed. Upstream's own `schema:validate` and `examples:validate` already find two of the four defects here; nobody
is running them.

## Why none of this was caught

The checks are real. They are just not wired to anything that runs.

- **GitHub Actions runs none of them.** `.github/workflows/ci.yml` has three jobs. Two are gated on a `rust` paths
  filter (`Cargo.toml`, `Cargo.lock`, `crates/**`, the workflow file). The third, `docs`, is gated on `website/**`,
  `docs/**` and `**/*.md` — so a change to a schema under `website/static/schemas/` reaches only that job.
- **The `docs` job runs one script, and the script is a no-op.** `.config/mise/tasks/ci/validate_docs.py` globs
  markdown, slices to `md_files[:50]`, checks each is readable and non-empty, and returns 0 on every path — the
  failure branch prints "Validation completed with warnings" and falls through to the same `return 0`. No mise task,
  and therefore no `jsonschema` invocation, executes in CI.
- **Some changes trigger no job at all.** Neither `tests/bdd/**` nor `.config/mise/**` matches either paths filter,
  so editing an IR fixture or a validation task starts nothing.
- **`fixtures:validate` validates nothing.** Its `FIXTURE_DIRS` are `.morphir/testing/fixtures` and
  `tests/bdd/testdata/morphir-ir`. Neither exists at this commit — the fixtures live at `tests/bdd/fixtures/`. It
  prints "No fixture files found to validate" and returns 0, which reads as a pass.

Four independent gaps, and the v4 material sits in the intersection of all of them.

## What to do with each finding

**Ours to send upstream.** Mechanical, decided by the schema's own stated intent, no format question involved:

- the `description` indentation at `morphir-ir-v4-document-tree-files.yaml` line 83;
- the `OpaqueTypeSpecification` `oneOf`/`anyOf` overlap that rejects the schema's own example;
- `fixtures:validate` pointing at two directories that do not exist;
- `fmt:schema` and `fmt-check:schema` pointing a JSON-only formatter at a directory of YAML;
- the CI gap — the `docs` job runs no validator, and fixture and task changes trigger no job.

**Open questions about the format**, which no one can close without deciding what v4 means:

- **Is flattened access on a definition legal?** The schema says no. `complete-example.json` and the document tree
  schema's `TypeDefinitionFile.def` both write it; `books-and-records-example.json` does not. Either
  `AccessControlled` grows a third arm or the example is rewritten. Until then the question of which of two published
  examples is right is genuinely open — see [Divergences](/design/divergences.md).
- **Is `v4-library-distribution.json` meant to be a v4 document?** It turns on whether v4 keeps any array-tagged
  distribution form. `morphir-ir-v4.yaml` has none, so on today's schema the fixture is a v3 payload that was version-
  stamped and never checked.

## Reproducing

```bash
mise run schemas:check
```

It generates the JSON from the mirrored YAML (`mise run schemas:build`, output in `.dev/out/squire/schemas/`), runs
the metaschema check over every mirrored schema, and validates every mirrored document that carries a
`formatVersion`. It **reports** rather than gates: these are upstream's defects at a pinned commit, and failing every
local run over them would train people to ignore the output. The numbers to expect are the ones above — one
metaschema failure, three documents checked, three failing.
