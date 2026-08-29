---
type: Intent
title: morphir-scala Elm frontend extension
description: "Ship morphir-scala's Elm source-to-IR compiler as an independently selectable Morphir Extension Protocol provider."
state: InProgress
kind: feature
breaking: false
created: 2026-08-26
state_since: 2026-08-28
tags: [elm, extension-protocol, langkit]
sources:
  - id: morphir-host
    title: Shipped Morphir MEP host and verified extension acquisition
    resource: https://github.com/finos/morphir/tree/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f
  - id: mep-0.1
    title: Morphir Extension Protocol 0.1
    resource: https://github.com/finos/morphir/blob/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f/docs/design/draft/extensions/protocol.md
  - id: elm-conformance
    title: Shared Elm extension conformance tests
    resource: https://github.com/finos/morphir/blob/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f/crates/integration-tests/tests/elm_extension.rs
  - id: reference-extension
    title: Shipped morphir-elm MEP reference extension
    resource: https://github.com/finos/morphir-elm/tree/b065e493d7a4256ed47878b129abf2333e977313/cli2/mep
  - id: morphir-rust-sdk
    title: Shipped MEP SDK and process host
    resource: https://github.com/finos/morphir-rust/tree/8e069a3c0ba2bc057555d951f4bac15458c366ad/crates
  - id: scala-elm-compiler-api
    title: Current morphir-scala Elm compiler API
    resource: https://github.com/finos/morphir-scala/tree/43439fcccec3da5f78b4a314f19f8919912fefc1/morphir/langkit/elm/compiler/api
  - id: kyo-bignum-json
    title: Kyo arbitrary-precision Structure number follow-on
    resource: https://github.com/getkyo/kyo/pull/1920
---

# 0037: morphir-scala Elm frontend extension

Ship morphir-scala's Elm source-to-IR compiler as an independently selectable Morphir Extension Protocol provider.

## Problem

The Morphir CLI needs more than one implementation of Elm-to-Morphir-IR compilation. The first implementation will
wrap `finos/morphir-elm` and prove the proposed Morphir Extension Protocol, abbreviated MEP. The Scala implementation
should follow it so the protocol is tested against an independent compiler and morphir-scala can provide its Elm
tooling through the same CLI contract.

The current Elm langkit parses one or more modules into concrete and abstract syntax trees. Its compiler API exposes
parsing and tree queries through a JSON envelope on JVM, Scala.js, Scala Native, and a Scala.js WebAssembly link.
It does not yet lower an Elm module to Morphir IR or implement the MEP lifecycle. Treating the current JSON envelope
as the extension protocol would expose a private API that has different operations and diagnostic rules.

[Intent 0010](/0010-elm-frontend-buildkit-adapter.md) owns the typed, in-process Elm frontend used by the standard
buildkit pipeline. This intent owns the external MEP binding and its distributable runtime. They must share one
Elm-to-IR implementation. Neither intent should create a second buildkit adapter or a separate lowering pipeline.

## Approach

The prerequisite is satisfied. The `finos/morphir-elm` reference extension shipped at `b065e493`, and the Morphir
CLI's MEP host and verified acquisition path shipped through `f7bede45`. The
[Elm frontend extension Design Note](../morphir/morphir-scala/design/elm-frontend-extension.md) is the narrative
home for the Scala provider's compiler, protocol, runtime, identity, and distribution boundaries.

The proposed delivery order is:

```mermaid
flowchart LR
    Reference["morphir-elm reference sidecar"] -->|establishes fixtures and host behavior| Refinement["detailed Scala design"]
    Refinement -->|settles compiler and runtime boundaries| Lowering["shared Elm-to-IR compiler"]
    Lowering -->|supplies compilation| Adapter["morphir-scala MEP frontend"]
```

**Figure 1:** The Scala extension follows the reference sidecar and reuses the compiler work shared with intent 0010.

The first Scala slice accepts one complete Elm module as source text and returns one Morphir IR distribution. The
module may use the supported Morphir SDK types. It does not import another user module. The host supplies or
synthesizes the package name, exposed module, IR version, document URI, and document version required by MEP.

The extension implements the MEP lifecycle and the `morphir.frontend.compile` capability needed by that slice. It
uses `Content-Length` framed JSON-RPC 2.0 when shipped as a native executable. It advertises only the language,
IR versions, and operations it supports. The implementation must pass the same protocol fixtures and conformance
tests used by the reference sidecar.

Compilation failures return a normal compile result with `success: false`. Parse, type, and lowering diagnostics use
stable codes when morphir-scala has them. Source locations use the request document URI, zero-based lines and
characters, an inclusive start, and an exclusive end. Invalid protocol messages, failed initialization, and internal
extension failures use JSON-RPC errors. Standard output contains protocol frames only; operational logs go to
standard error.

Refinement settles the first implementation choices as follows:

- build the first compiler on the JVM and ship it as one GraalVM Native Image process artifact, then expand the
  platform matrix from repeatable CI builds;
- introduce a pure compiler seam shared by the in-process adapter and the MEP adapter, leaving the existing JSON ABI
  as a parser/query compatibility surface;
- keep parsing and Elm semantics in langkit, compile directly to the Kyo code model, and keep protocol concerns
  outside the compiler;
- emit the MEP 0.1 Morphir IR v3 result through a bounded one-way wire projection from the Kyo code model. The
  projection uses Kyo JSON and rejects code-model features that v3 cannot represent;
- use Kyo Schema, JSON, JSON-RPC, effects, and path support in the process adapter. Do not add ZIO, zio-json, or
  classic `org.finos.morphir.ir` dependencies to the new compiler or adapter;
- identify the provider as `morphir-scala-elm`, distinct from the `morphir-elm` reference provider, and add explicit
  provider selection to the host so both can be installed at once;
- use the acquired extension index, checksum, catalog, and exact-version lock already shipped by the Morphir CLI;
- normalize parser failures to stable MEP diagnostics, including `elm.parser`, the request URI, and zero-based ranges.

The Kyo code model is the compiler's domain boundary. Morphir IR v3 is an external compatibility format for MEP 0.1,
not a second domain model. The compiler and adapter must never reconstruct classic IR objects to produce that format.
This keeps the work inside the strangler boundary established by
[decision 0005](../morphir/morphir-scala/decisions/0005-bridge-nothing-between-zio-and-kyo.md) and the wire-projection
rule in [decision 0017](../morphir/morphir-scala/decisions/0017-deprecated-ir-formats-are-wire-projections.md).

The first implementation uses current stable project toolchains. Morphir is greenfield, has no published Scala or
Rust crates, and has no downstream compatibility promise. Toolchain upgrades therefore optimize for a correct,
maintainable implementation rather than preserving a hypothetical minimum supported version. A compatibility floor
may be introduced later when a real consumer or published artifact requires one.

The implementation stays on the published Kyo `1.0.0-RC6` release. Its self-describing JSON reader materializes
integral numbers through signed `Long`, so MEP document versions from `Long.MaxValue + 1` through the protocol's
unsigned 64-bit maximum cannot cross this provider's JSON boundary yet. The Scala domain type retains the full
unsigned range, but the first executable supports wire values from zero through `Long.MaxValue`. Kyo
[PR 1920](https://github.com/getkyo/kyo/pull/1920) tracks the upstream fix. Adopting a reviewed, published Kyo version
is follow-on work and does not block this slice. The provider must not depend on a local or unpublished snapshot.

The first slice excludes multi-file projects, user-module imports, project manifest discovery, dependency resolution,
incremental document sessions, progress, and cooperative cancellation. It also excludes backend generation,
validation, IR transforms, changes to the MEP host, and ownership of the generic buildkit contracts. Later intent can
add those capabilities after the single-file path proves the compiler and protocol boundary.

The initial slice is complete when a Morphir CLI host can install and explicitly select the packaged
`morphir-scala-elm` extension, compile one valid Elm module to schema-valid Morphir IR, and render structured
diagnostics for an invalid module. The same conformance fixtures must pass against the reference Elm sidecar and the
Scala extension. Offline activation must succeed from the verified content-addressed store, and tampered installed
bytes must be rejected before process launch.

## Unresolved

MEP 0.1 is the shipped compatibility target but remains explicitly versioned and evolvable. Any Scala implementation
feedback that changes the common contract must update the protocol and both providers rather than creating an
undocumented provider-specific behavior.

The current langkit proves parsing and cross-language invocation, not Elm-to-IR semantic correctness. Refinement must
identify the smallest supported Elm subset and the conformance corpus that demonstrates equivalent Morphir IR. It
must also decide whether equivalence means schema validity, semantic equality with `morphir-elm`, or both.
