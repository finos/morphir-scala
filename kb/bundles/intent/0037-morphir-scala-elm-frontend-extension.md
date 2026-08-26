---
type: Intent
title: morphir-scala Elm frontend extension
description: "Expose morphir-scala's Elm source-to-IR compiler through the Morphir Extension Protocol after the reference morphir-elm sidecar ships."
state: Backlog
kind: feature
breaking: false
created: 2026-08-26
state_since: 2026-08-26
tags: [elm, extension-protocol, langkit]
sources:
  - id: mep-0.1-proposal
    title: Morphir Extension Protocol 0.1 proposal
    resource: https://github.com/finos/morphir/blob/5a697e4eab0db662f9bc0b339ade80630701b420/docs/design/draft/extensions/protocol.md
  - id: scala-elm-compiler-api
    title: Current morphir-scala Elm compiler API
    resource: https://github.com/finos/morphir-scala/tree/355eb96e2eaa5e5bea0f68d83e9ff9aa7293a2c7/morphir/langkit/elm/compiler/api
---

# 0037: morphir-scala Elm frontend extension

Expose morphir-scala's Elm source-to-IR compiler through the Morphir Extension Protocol after the reference morphir-elm sidecar ships.

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

Keep this intent in Backlog until the `finos/morphir-elm` sidecar and CLI path establish the first working MEP
frontend. Then move it to Refinement and write a detailed design against that implementation, the conformance
fixtures, and the MEP 0.1 proposal pinned at commit `5a697e4e`.

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

The refinement stage must settle these questions before implementation starts:

- whether the first distributable runtime is a Scala Native executable, a JVM process, or both;
- how the MEP handler calls the shared compiler without making the existing JSON ABI a second public protocol;
- where Elm type checking and lowering to Morphir IR live relative to the langkit and intent 0010 buildkit adapter;
- which operating-system and architecture artifacts can ship, including Windows ARM64;
- how extension metadata, protocol versions, checksums, discovery, and release versions are packaged;
- which diagnostics the current parser can map losslessly to MEP and where conversion needs more compiler data.

The first slice excludes multi-file projects, user-module imports, project manifest discovery, dependency resolution,
incremental document sessions, progress, and cooperative cancellation. It also excludes backend generation,
validation, IR transforms, changes to the MEP host, and ownership of the generic buildkit contracts. Later intent can
add those capabilities after the single-file path proves the compiler and protocol boundary.

The initial slice is complete when a Morphir CLI host can select the packaged Scala extension, compile one valid Elm
module to schema-valid Morphir IR, and render structured diagnostics for an invalid module. The same conformance
fixtures must pass against the reference Elm sidecar and the Scala extension.

## Unresolved

MEP 0.1 remains a proposal. The reference implementation may expose contract gaps that require a newer pinned
revision before Scala refinement begins. The detailed design must record any divergence instead of copying behavior
that conflicts with the agreed protocol.

The current langkit proves parsing and cross-language invocation, not Elm-to-IR semantic correctness. Refinement must
identify the smallest supported Elm subset and the conformance corpus that demonstrates equivalent Morphir IR. It
must also decide whether equivalence means schema validity, semantic equality with `morphir-elm`, or both.
