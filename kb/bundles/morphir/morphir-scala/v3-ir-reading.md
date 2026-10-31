---
type: Capability
title: v3 IR Reading
description: "morphir-model-compat-v3 reads the v3 morphir-ir.json that morphir-elm writes straight into the Kyo code model, with no classic library or ZIO, on the JVM and Scala.js."
tags: [model, ir, v3, morphir-elm, kyo]
status: stable
---

# v3 IR Reading

`V3Reader`, in `morphir-model-compat-v3`, reads Morphir IR in format version 3 — the `morphir-ir.json` that
morphir-elm writes — into the code model (`org.finos.morphir.codemodel`). It needs only `morphir-model` and Kyo:
no classic `org.finos.morphir.ir` types and no ZIO, on the JVM or Scala.js. The same module writes v3 back with
`V3WireProjection`, so v3 is handled in one place, at the edge of the code model.

It replaces the classic path — the zio-json decoders in `morphir-interop-zio-json` followed by `V3Lowering` in
`morphir-model-lowering` — which is deprecated with the rest of the classic family
([intent 0042](../../intent/0042-deprecate-the-classic-zio-based-morphir-library.md)).

## What it reads

| Method | Reads |
| --- | --- |
| `decodeIRFile` | a whole document: `{"formatVersion": 3, "distribution": …}` |
| `decodeDistribution` | a bare `Library` or `Bundle` distribution |
| `decodeName`, `decodePath`, `decodeFQName` | names, paths, fully qualified names |
| `decodeLiteral`, `decodeType`, `decodePattern`, `decodeValue` | literals, types, patterns, values |
| `decodeTypeDefinition`, `decodeTypeSpecification` | type definitions and specifications |
| `decodeValueDefinition`, `decodeValueSpecification` | value definitions and specifications |
| `decodeModuleDefinition`, `decodeModuleSpecification` | modules |

A `Library` decodes to one code-model library distribution. A `Bundle` decodes to one library per package, ordered
by package name. Every method returns a `Result`; nothing throws.

## How v3 maps to the code model

The rules are the ones the classic decoder and `V3Lowering` applied together, so existing consumers see the same
code model:

- **Attributes are erased**, except that a typed value's or pattern's type becomes its `inferredType`. An untyped
  value's `{}` gives empty attributes.
- **Order is deterministic.** Constructors, `LetRecursion` definitions and record-update fields are ordered by name,
  bundle libraries by package name; v3 holds each of them in a map.
- **Documentation** is split into lines; an empty string is no documentation. The module `doc` member morphir-elm
  writes has no place in the code model and is ignored.
- **Package version** is left empty, because v3 does not record one.
- A bare `ValueDefinition` read on its own is public, because the code model needs an access level and the v3 node
  has none; inside a module the access comes from the module entry.

## What it rejects

Only `formatVersion` 3 is read. IR written in format versions 1 or 2 must be regenerated with current morphir-elm.

| Error | When | MCK code |
| --- | --- | --- |
| `MalformedJson` | the text is not JSON, or exceeds Kyo's parser limits (512 levels of nesting, 100,000 members in one array or object) | `invalid_json` |
| `UnsupportedFormatVersion` | `formatVersion` is not 3 | `unsupported_format_version` |
| `UnknownTag` | a tagged array names a tag its node kind lacks | `unknown_tag` |
| `WrongShape` | any other structural fault | `invalid_shape` |

Every error carries the JSON path of the fault, such as `$.distribution[3]`.

## How it is checked

- Per-node unit tests run on the JVM and Scala.js in `morphir.model.compat.v3.{jvm,js}.test`, ported from the classic
  decoder's tests.
- Before the classic path is removed, every IR file morphir-elm generates from `examples/morphir-elm-projects` and
  `morphir-elm/sdks` (15 files) decoded to exactly what the classic decoder plus `V3Lowering` produced.

## Known limits

- Documentation that ends in a blank line loses that line when written back through `V3WireProjection`: the line split
  drops a trailing empty line. Reading is unaffected.
- The reader is more lenient than the classic decoder on input morphir-elm does not produce: a non-type value
  attribute becomes empty attributes rather than an error, and a `CharLiteral` may be any one code point.
