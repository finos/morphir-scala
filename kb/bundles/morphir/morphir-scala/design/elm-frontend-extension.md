---
type: Design Note
title: morphir-scala Elm frontend extension
description: "The shared Elm compiler, MEP process adapter, provider identity, and verified executable distribution."
tags: [elm, langkit, extension-protocol, scala-native, distribution]
status: draft
stale_after: 2026-11-28
sources:
  - id: intent-0037
    title: morphir-scala Elm frontend extension intent
    resource: /0037-morphir-scala-elm-frontend-extension.md
  - id: morphir-host
    title: Morphir MEP host and verified extension acquisition
    resource: https://github.com/finos/morphir/tree/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f
  - id: mep-protocol
    title: Morphir Extension Protocol 0.1
    resource: https://github.com/finos/morphir/blob/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f/docs/design/draft/extensions/protocol.md
  - id: host-conformance
    title: Shared Elm extension conformance tests
    resource: https://github.com/finos/morphir/blob/f7bede45d6a97ad5e673bc4a1371e2665fd22d1f/crates/integration-tests/tests/elm_extension.rs
  - id: reference-extension
    title: morphir-elm MEP reference extension
    resource: https://github.com/finos/morphir-elm/tree/b065e493d7a4256ed47878b129abf2333e977313/cli2/mep
  - id: scala-compiler-api
    title: morphir-scala Elm compiler API
    resource: https://github.com/finos/morphir-scala/tree/43439fcccec3da5f78b4a314f19f8919912fefc1/morphir/langkit/elm/compiler/api
---

# morphir-scala Elm frontend extension

Morphir-scala should ship a directly executable Scala Native MEP provider named `morphir-scala-elm`. The process
adapter and the in-process buildkit adapter must call one pure Elm-to-Morphir-IR compiler. The MEP layer owns framing,
lifecycle, metadata, and conversion to protocol values. It does not own parsing, type checking, lowering, or a second
compiler pipeline.

This note is the Narrative Home for
[intent 0037](../../../intent/0037-morphir-scala-elm-frontend-extension.md). It records the first vertical slice while
implementation can still refine details. The shared pipeline boundary remains in
[Multi-frontend pipeline and workspace boundaries](/design/pipeline-workspace-boundaries.md).

## Shipped contract and observed gap

The Morphir CLI now hosts MEP 0.1 processes, installs checksum-verified executable artifacts into a content-addressed
store, locks exact versions, and validates installed bytes again before activation. The `morphir-elm` provider is the
reference implementation, and the host integration tests define a reusable single-file Elm compilation contract.

Morphir-scala currently parses Elm into CST and AST values on JVM, JavaScript, WebAssembly, and Scala Native. Its JSON
ABI exposes parse and query operations. It does not type-check Elm or lower an Elm module to classic Morphir IR v3.
Wrapping that ABI in JSON-RPC would therefore preserve the missing compiler and publish the wrong abstraction.

## Compiler and adapter boundaries

The implementation has three dependency-directed layers:

```mermaid
flowchart LR
    Request["MEP compile request"] --> Adapter["MEP process adapter"]
    InProcess["buildkit Elm frontend"] --> Compiler["shared Elm compiler"]
    Adapter --> Compiler
    Compiler --> Parser["langkit parser and semantics"]
    Compiler --> Lowering["classic IR v3 lowering"]
    Lowering --> Result["typed compilation result"]
    Result --> Adapter
    Result --> InProcess
```

**Figure 1:** Both public adapters depend on one compiler and lowering path.

The shared compiler accepts immutable source and request context and returns a typed success or failure value with
diagnostics. It has no dependency on stdin, stdout, JSON-RPC envelopes, process termination, buildkit scheduling, or
the existing JSON ABI. The first API can be specialized to one source document while keeping package identity,
exposed modules, and requested IR version explicit.

`ElmParse` remains frontend-internal. The generic `Compile[I, O, D]` and typed `Frontend` contracts remain in-process
buildkit boundaries. The MEP adapter converts protocol values to the same compiler request and converts the result
back to MEP values. It never becomes a buildkit stage or another lowering implementation.

The first lowering target is the classic Morphir IR v3 JSON contract required by MEP 0.1. The implementation should
introduce only the portable IR value and encoder surface needed by the supported Elm slice. It should not pull the
JVM-only classic runtime or ZIO model through the Scala Native dependency graph. A future shared IR module may absorb
that narrow representation after its cross-platform API is proven.

## First supported Elm slice

The first slice compiles one complete Elm module supplied by value. It supports the package name, exposed module,
document URI, document version, and IR version supplied by the host. The initial conformance fixture requires:

- a valid module declaration with an exposing clause;
- top-level type annotations and definitions;
- curried functions over `Int`;
- local name references; and
- integer addition lowered to the Morphir SDK function used by the reference provider.

The valid result must be schema-valid classic Morphir IR v3, preserve the requested `local/example` package identity,
and contain exactly the requested `Example` module in both IR and MEP result metadata. Invalid syntax returns
`success: false`, no IR, and no compiled modules. A malformed module header returns an error diagnostic with code
`elm.parser`, the request URI, and a nonempty zero-based source range.

This slice does not claim full Elm semantic compatibility. Multi-file projects, user-module imports, dependency
resolution, manifest discovery, complete type inference, incremental sessions, progress, cancellation, and backend
generation remain outside it. Each additional language feature should enter through a failing compiler test and a
shared conformance fixture.

## Protocol process

The executable reads and writes Content-Length-framed JSON-RPC 2.0. Standard output contains frames only; diagnostics
about the process itself go to standard error. The state machine is:

```mermaid
stateDiagram-v2
    [*] --> AwaitInitialize
    AwaitInitialize --> Running: morphir.initialize / compatible 0.1
    Running --> Running: morphir.initialized
    Running --> Running: morphir.frontend.compile
    Running --> AwaitExit: morphir.shutdown
    AwaitExit --> [*]: exit
```

**Figure 2:** The first executable implements the complete MEP lifecycle around a stateless compiler.

Initialization advertises protocol `0.1`, capability `frontend`, language `elm`, extension `.elm`, IR version `3`,
and compile support. The response identity must exactly match the acquired index record. Compilation failures are
normal MEP results. Invalid framing, invalid lifecycle transitions, unsupported protocol requests, and internal
failures are JSON-RPC errors.

Framing and decoding are bounded. The process rejects missing, duplicate, malformed, or oversized Content-Length
headers before allocating a body. EOF during a frame is an error. Unknown JSON fields remain forward-compatible where
MEP allows them, but required identity and compilation fields are validated before compiler invocation.

## Provider identity and host selection

Language and provider are separate dimensions. `morphir-elm` and `morphir-scala-elm` both compile Elm, but they are
independently versioned implementations and must be installable at the same time. The host therefore needs an
explicit provider selector while retaining language-based defaults for simple use.

The first host interface is `morphir compile --language elm --extension morphir-scala-elm`. Configuration may select
the same provider for repeated builds. Without an explicit selection, the existing `morphir-elm` default remains
compatible. The host validates that the selected provider advertises Elm frontend support.

Using the shared `morphir-elm` identity for both executables was rejected. It fits the current language-derived slot,
but causes unrelated provider versions to compete in one history, prevents side-by-side installation, and makes a
lock insufficient to explain which implementation produced an IR distribution.

## Artifact and release model

The first distributable artifact is one directly executable Scala Native process. This matches the host's shipped
`runtime: process` contract without requiring Java, a classpath, an archive launcher, or multiple installed files.
Mill builds the executable, but the Morphir extension index and store distribute and activate it.

Each supported operating-system and architecture pair has an immutable index artifact with:

- schema version 1, extension ID `morphir-scala-elm`, and one exact semantic version;
- channel membership, MEP version `0.1`, and capability `frontend`;
- runtime `process`, exact operating system and architecture, portable filename, and executable flag; and
- immutable source location plus SHA-256 digest.

The first CI tracer may cover only its build runner's platform. A platform is advertised as supported only after CI
produces its artifact and executes the common host conformance tests against that exact binary. Linux, macOS, Windows,
x86-64, ARM64, and any cross-build strategy remain claims to prove, not metadata to predict.

Acquisition publishes verified bytes to the host content-addressed store and records catalog and lock state
transactionally. Offline activation rehashes the installed artifact. A checksum mismatch during acquisition creates
no active catalog entry; a later mutation fails before process launch.

The extension index is not the source-package registry described by the Package URL design. Its store is not Mill's
machine tool cache. Mill may cache build inputs and produce the native executable, but the common host owns runtime
selection, installation, locking, and activation.

## Toolchain policy

Morphir is greenfield. Morphir-scala artifacts and the Rust host crates have not been published, and there are no
downstream compatibility consumers. The implementation uses current stable Scala, Scala Native, Mill, and Rust
toolchains rather than maintaining a legacy minimum supported version. Toolchain pins remain exact and reproducible;
they are upgraded deliberately and verified in CI. A compatibility floor becomes a product requirement only when a
published artifact or real consumer creates one.

This policy permits the host's Rust 1.98 baseline and allows the Scala extension to adopt the latest stable compiler
or Native release needed for a safe process implementation. It does not permit unreviewed floating CI versions.

## Acceptance

The vertical slice is complete only when all of the following are repeatable:

1. Shared compiler tests fail first, then prove the valid function and invalid-header cases without a process.
2. Framing and lifecycle tests cover fragmented input, multiple frames, malformed headers, initialize, compile,
   shutdown, and exit.
3. The native executable passes the same ignored `elm_extension` integration suite as `morphir-elm`.
4. The host installs `morphir-scala-elm` from an index, explicitly selects it, and compiles after that index is gone.
5. Acquisition rejects altered source bytes, and activation rejects altered stored bytes before launch.
6. CI builds and tests every advertised platform artifact using pinned current toolchains.

Schema validity is necessary but not sufficient. For the supported subset, normalized semantic IR must also match
the reference provider. Differences in nonsemantic metadata may be normalized explicitly in the conformance helper.

## Alternatives and unresolved work

A JVM jar was rejected for the first artifact because the shipped acquisition model installs one executable file and
does not describe a Java launcher or classpath. The existing JSON ABI was rejected as the MEP implementation because
it exposes parse/query operations and does not define the compiler contract. Duplicating a minimal lowering inside
the process adapter was rejected because intent 0010 and intent 0037 must converge on one compiler.

Implementation must still determine the smallest typed IR representation that can encode classic v3 without a
JVM-only dependency, the exact native main-linking shape in the current Mill build, and which parser positions need
normalization to MEP's exclusive end range. Multi-platform publication, complete Elm typing, source dependencies, and
versioned protocol evolution remain later slices.
