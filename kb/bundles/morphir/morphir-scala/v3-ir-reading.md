---
type: Capability
title: v3 IR Reading
description: "morphir-model-compat-v3 reads the v3 morphir-ir.json that morphir-elm writes straight into the Kyo code model, with no classic library or ZIO, on the JVM and Scala.js, held to the Morphir Compatibility Kit."
tags: [model, ir, v3, morphir-elm, kyo, mck]
status: stable
---

# v3 IR Reading

`V3Reader`, in `morphir-model-compat-v3`, reads Morphir IR in the v3 format family — the `morphir-ir.json` that
morphir-elm writes, and the v3.1 `Specs` documents — into the code model (`org.finos.morphir.codemodel`). It needs only
`morphir-model` and Kyo: no classic `org.finos.morphir.ir` types and no ZIO, on the JVM or Scala.js. The same module
writes v3 back with `V3WireProjection`, so v3 is handled in one place, at the edge of the code model.

It replaces the classic path — the zio-json decoders in `morphir-interop-zio-json` followed by `V3Lowering` in
`morphir-model-lowering` — which is deprecated with the rest of the classic family
([intent 0042](../../intent/0042-deprecate-the-classic-zio-based-morphir-library.md)).

## What it reads

| Method | Reads |
| --- | --- |
| `decodeIRDocument` | a whole document; returns the release it declares and its distributions |
| `decodeIRFile` | a whole document; returns the distributions only |
| `decodeDistribution` | a bare `Library`, `Specs` or `Bundle` distribution |
| `decodeFormatVersion` | a `formatVersion` value on its own |
| `decodeName`, `decodePath`, `decodeFQName` | names, paths, fully qualified names |
| `decodeLiteral`, `decodeType`, `decodePattern`, `decodeValue` | literals, types, patterns, values |
| `decodeTypeDefinition`, `decodeTypeSpecification` | type definitions and specifications, `DerivedTypeSpecification` included |
| `decodeValueDefinition`, `decodeValueSpecification` | value definitions and specifications |
| `decodeModuleDefinition`, `decodeModuleSpecification` | modules, with their documentation |

A `Library` decodes to one code-model library distribution and a `Specs` document to one specs distribution. A
`Bundle` decodes to one library per package, ordered by package name. Every method returns a `Result`; nothing
throws.

## Format versions

`formatVersion` follows finos/morphir's format-version specification. It is the integer `3` or a release string
`"3.minor.patch"`, and the supported table is `[3.0.0,3.2.0)`, which includes the 3.1.x `Specs` distribution. A
`Specs` document must declare at least `3.1.0`. `V3WireProjection` writes the version back canonically: the integer `3`
for `3.0.0`, the exact string otherwise.

## How v3 maps to the code model

The rules match what the classic decoder and `V3Lowering` produced, so existing consumers see the same code model:

- **Attributes are erased**, except that a typed value's or pattern's type becomes its `inferredType`. Inside a
  `Library` every value, pattern and value-definition input must be typed; a node read on its own may be untyped.
- **Order is deterministic.** Constructors, `LetRecursion` definitions and record-update fields are ordered by name,
  bundle libraries by package name; v3 holds each of them in a map.
- **Documentation** is split into lines; an empty string is no documentation. Module documentation is kept on
  `ModuleDefinition.doc` and `ModuleSpecification.doc`, and written back as `"doc": null` when there is none.
- **Package version** is left empty, because v3 does not record one.
- A bare `ValueDefinition` read on its own is public, because the code model needs an access level and the v3 node
  has none; inside a module the access comes from the module entry.

## What it rejects

Every error is a `V3ReadError` with the JSON path of the fault and a diagnostic code from the format-version
specification or the Morphir Compatibility Kit:

| Fault | Code |
| --- | --- |
| The text is not JSON, or exceeds Kyo's parser limits (512 levels of nesting, 100,000 members in one array or object) | `invalid_json` |
| No `formatVersion`, or more than one | `missing_format_version`, `duplicate_format_version` |
| `formatVersion` of the wrong type, malformed, or out of range | `invalid_format_version_type`, `invalid_format_version_syntax`, `format_version_out_of_range` |
| Format version 1 or 2, or a family of 5 or above | `unsupported_format_version_major` |
| A v3 release outside `[3.0.0,3.2.0)` | `unsupported_format_version_minor` |
| A document of the v4 family read as v3 | `version_mismatch` |
| A `Specs` document below `3.1.0` | `specs_before_3_1` |
| An unknown tag, a node of the wrong shape, or an untyped node inside a `Library` | `invalid_type` |

IR written in format versions 1 or 2 must be regenerated with current morphir-elm.

## How it is checked

- **Morphir Compatibility Kit.** `mise run mck:run` runs finos/morphir's `morphir mck run --suite ir` against the
  adapter in `morphir.model.compat.v3.mck`, using the Morphir CLI pinned in `mill-build/src/millbuild/MorphirCli.scala`,
  and gates on `morphir mck report check` with an empty `allowed-failing.json`. The `mck-ir` CI job runs the same
  steps. The adapter claims format versions `[3.0.0,3.2.0)`, the `json` profile and the `single` layout, so every
  `@version:3` case runs and every v4 case is skipped. See `morphir/model/CONTRIBUTING.md` to run it locally, or
  against a kit checkout with `--kit`.
- **Unit tests** run on the JVM and Scala.js in `morphir.model.compat.v3.{jvm,js}.test`, ported from the classic
  decoder's tests and the format-version conformance corpus.
- Before the classic path is removed, every IR file morphir-elm generates from `examples/morphir-elm-projects` and
  `morphir-elm/sdks` (15 files) decoded to exactly what the classic decoder plus `V3Lowering` produced.

## Known limits

- **Input type expansions.** The code model holds one type per value-definition input. Where morphir-elm declares an
  alias and writes its expansion as the input's attribute, the declared alias is kept and the expansion is lost, as
  the classic path did.
- **Trailing blank lines in documentation** do not survive a write through `V3WireProjection`: the line split drops a
  trailing empty line. Reading is unaffected.
- **`Bundle`** is read, although the kit has not settled whether it belongs to v3.
