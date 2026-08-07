---
type: Design Note
title: Package URL-centered package management
description: "An evolving design for canonical Package URL identities, VERS requirements, reproducible materialization, and packages outside ecosystem registries."
tags: [buildkit, package-management, purl, vers, resolution, elm]
status: draft
stale_after: 2026-10-05
sources:
  - id: issue-932
    title: Multi-frontend Morphir transformation pipeline
    resource: https://github.com/finos/morphir-scala/issues/932
    last_modified: 2026-07-28
  - id: purl-standard
    title: Package URL specification
    resource: https://github.com/package-url/purl-spec/blob/d6ecb3989ecb6486b3b3729c8e19d59793411b7c/docs/specification/standard/specification.md
  - id: purl-qualifiers
    title: Package URL common qualifiers
    resource: https://github.com/package-url/purl-spec/blob/d6ecb3989ecb6486b3b3729c8e19d59793411b7c/docs/specification/common-qualifiers.md
  - id: purl-types
    title: Package URL type index and candidate types
    resource: https://github.com/package-url/purl-spec/tree/d6ecb3989ecb6486b3b3729c8e19d59793411b7c/docs/types
  - id: purl-conformance
    title: Package URL language-neutral conformance tests
    resource: https://github.com/package-url/purl-spec/tree/d6ecb3989ecb6486b3b3729c8e19d59793411b7c/tests
  - id: vers-standard
    title: Package VERS specification
    resource: https://github.com/package-url/vers-spec/blob/c62c6c1f2acb660e1d6757beff76dfb523f4ef27/docs/specification/standard/specification.md
  - id: vers-conformance
    title: Package VERS language-neutral conformance tests
    resource: https://github.com/package-url/vers-spec/tree/c62c6c1f2acb660e1d6757beff76dfb523f4ef27/tests
  - id: elm-cache
    title: Elm 0.19.1 package-cache paths
    resource: https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/builder/src/Stuff.hs
  - id: elm-solver
    title: Elm 0.19.1 online and offline dependency solver
    resource: https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/builder/src/Deps/Solver.hs
  - id: elm-details
    title: Elm 0.19.1 package verification and download behavior
    resource: https://github.com/elm/compiler/blob/c9aefb6230f5e0bda03205ab0499f6e4af924495/builder/src/Elm/Details.hs
  - id: shelm
    title: shelm unpublished-package implementation
    resource: https://github.com/robx/shelm/blob/4417730f5847e6ccba1b19f1b25166471433d633/shelm
---

# Package URL-centered package management

Morphir package management should use [Package URL](https://github.com/package-url/purl-spec) as its canonical
identity and interchange model from the first manifest read through resolution, locking, diagnostics, and public API
results. Package VERS expresses range requirements inside that Package URL representation. Content locations,
credentials, integrity, caches, and compiler-specific layouts remain separate concerns.

This is a Design Note because several type-specific and policy decisions still require research and review. It may be
updated while [intent 0013](../../../intent/0013-pluggable-package-resolution-and-materialization.md) is in Refinement.
Once those questions settle, an immutable Decision Record will capture the accepted answer and rejected alternatives.

## Goals

- Reference packages from different ecosystems without translating them into a Morphir-only coordinate hierarchy.
- Give Morphir-native distributions a well-specified provisional Package URL type and validate it with real packages.
- Resolve public, private, vendored, and unpublished packages reproducibly through interchangeable backends.
- Support offline and air-gapped compilation without exposing an ecosystem tool's cache layout as a shared contract.
- Make locked content identity, source provenance, and integrity independently auditable.

This design does not turn a Package URL into a download URL, make a mutable branch reproducible, or standardize Elm's
private package-cache representation.

## Standards facts that constrain the design

A Package URL comprises seven components: scheme, type, namespace, name, version, qualifiers, and subpath. The scheme
is required and has the constant value `pkg`; type and name are also required. Canonicalization lowercases the type,
percent-encodes components with the specification's UTF-8 rules, sorts unique qualifier keys, and applies registered
type-specific normalization. Version comparison is outside the Package URL layer.

The common `vers` qualifier carries a Package VERS value on a versionless purl. It is mutually exclusive with the
exact `@version` component. VERS types define comparison and interval membership; their `|`-separated constraints are
not conventional boolean expressions. An ecosystem's native range syntax must be translated to a canonical VERS
before it is used in a canonical package requirement.

`elm` is a candidate Package URL type but is not registered. `morphir` is neither registered nor currently listed as
a candidate. This project owns only the provisional Morphir convention. Strict third-party validators may reject it,
and this initiative does not include proposing either type upstream.

## Proposed value model

`PackageUrl` is a validated decoded value with canonical serialization. The shared implementation must not depend on
JVM-only URI or form-encoding APIs because the package model is cross-platform.

A package reference is one of two canonical Package URL forms:

- a versioned purl for an exact request, such as `pkg:morphir/finos/risk-model@1.2.3`;
- a versionless purl carrying the percent-encoded `vers` qualifier for a range.

The Scala API exposes a parsed `PackageRequirement` view so callers manipulate a typed VERS expression rather than
raw qualifier text, but its canonical interchange form remains one Package URL. Exact version and VERS range are
mutually exclusive. The original requirement is retained in lock data and diagnostics.

A `ResolvedPackage` contains:

- an exact, canonical, versioned Package URL;
- dependency metadata expressed as package requirements;
- an immutable source revision when the source system provides one;
- a content digest calculated over the normalized materialized package;
- a separately typed content location and provenance.

The location may be a registry record, Git repository and commit, archive, local snapshot, or vendored directory. Two
locations may provide the same resolved identity; mirrors and private sources therefore do not fragment package
identity. A raw location never becomes a purl qualifier merely because an interpreter needs it.

A lock records the original requirement, exact resolved purl, complete transitive graph, immutable source revision,
content digest, and selected source provenance. Reusing a lock verifies content before compiler artifacts can be
reused. Refreshing a mutable branch or path is an explicit lock operation rather than an incidental build side effect.

## Ecosystem and Morphir identities

Packages retain the identity of the ecosystem that names and provisions them. Maven, npm, PyPI, and other registered
types pass through unchanged. A Morphir project consuming an npm package does not relabel it as `pkg:morphir`.

The Elm adapter may recognize `pkg:elm/<author>/<project>@<version>` as an explicitly local interoperability
convention matching Elm's observed `author/project` coordinate without assuming GitHub is the content location. This
project does not own Elm's package semantics, claim that identifier is a registered purl type, or propose it upstream.
The adapter's unpublished-package acceptance work therefore does not become an Elm Package URL standardization task.

The working Morphir form is `pkg:morphir/<namespace>/<name>@<version>`. Research and implementation must settle whether
a Morphir package is a distribution unit distinct from its Maven, npm, or Elm publication; namespace and case rules;
version ordering; the package root addressed by `subpath`; and default repository semantics. Real source-package and
registry workflows must demonstrate that this convention meets Morphir's needs before a later, separately scoped
effort even considers an upstream proposal. If a distribution is only an artifact of another ecosystem, that
ecosystem's registered purl remains authoritative.

## Resolution and materialization

The package capability covers version discovery, dependency metadata, resolution, locking, materialization, module
enumeration, and source reading. Policy for network access, mirrors, credentials, caches, and fatality belongs to
interpreters.

The launch backends are a pinnable git-file index and a local-directory registry. Both key records by canonical purl
and return typed locations plus integrity information. Later Git, HTTP registry, archive, vendored-tree, cache, and
test backends use the same contract.

The [MoonBit registry reference](/design/moonbit-package-management.md) provides evidence for the git-file-index
shape: Git-distributed per-package line-delimited histories, a small resolution kernel inside extensible metadata,
checksums that bind selected versions to source archives, and a mockable registry/materialization boundary. It is
precedent, not a normative format; Morphir defines its own schema from Package URL identity, source-package needs,
portable namespace rules, and lock acceptance criteria.

### Morphir source packages

The current proposal treats a Morphir package as a source distribution first. Its materialized root contains the
native project manifest, Morphir configuration when present, source modules, and package metadata needed to run the
appropriate frontend. Morphir IR distributions, generated schemas, and backend files are derived artifacts; a
registry does not require publishers to precompile every target in order to distribute reusable business logic.

The same canonical `pkg:morphir` identity may be supplied by a verified registry archive, immutable Git commit with
an optional package subdirectory, vendored tree, or local workspace snapshot. Those are source descriptors attached
to resolution or workspace policy, not alternative identities. Materialization validates that the source manifest's
declared package identity agrees with the resolved purl before a frontend runs.

Local paths and moving Git branches or tags are useful request-time selectors, but a publishable lock resolves them
to a normalized content digest and, for Git, an immutable commit. Local workspace overrides remain development policy
over a stable logical identity; they are not serialized as dependencies of a published package. This lets an
unpublished source package participate in exactly the same build pipeline without pretending it has been uploaded to
an ecosystem registry.

The first source-package acceptance fixture resolves one package identity independently from a local workspace, a
pinned local Git repository, and a registry-indexed source archive. All three materializations must validate the same
manifest identity and produce equivalent Morphir IR. Derived compiler caches and generated IR are excluded from the
package identity and content digest unless a later package format explicitly makes them distributable content.

Source acquisition and compilation are separate phases. Acquisition may contact only declared origins. Compilation
receives a prepared, opaque environment and runs with package-network access denied. Private-source credentials are
scoped to acquisition, never serialized into purls or locks, and redacted from diagnostics.

## Elm and shelm compatibility boundary

Elm 0.19.1 has no supported Git or path dependency syntax. Its compiler can nevertheless use an unpublished package
when a complete package source and registry view are prepared before compilation. shelm demonstrates GitHub archive,
Git, and local-path acquisition by fabricating the compiler's registry and isolated home before invoking Elm offline.

Morphir adopts the capability, not those mechanics. A version-specific Elm compiler-sandbox adapter may privately
create `registry.dat`, redirect `HOME` or `ELM_HOME`, and arrange compiler cache directories under an isolated task
destination. Shared types and acceptance tests see only an opaque prepared compiler environment. The adapter is
replaceable when Elm offers supported source injection or offline controls.

Elm treats pre-populated package sources as cached without applying the official download integrity check, and its
compiled-artifact fingerprint is not a source-content digest. The Morphir materializer must therefore verify its own
lock digest and must reject or rematerialize altered content before the Elm compiler can reuse stale artifacts.

## Capability-focused acceptance

The primary fixture is an Elm consumer that imports a direct deliberately unpublished package and evaluates its
result. That direct package's implementation calls a uniquely named symbol supplied only by a transitive unpublished
package, so neither an unused import nor materializing only the direct dependency can produce a false positive. The
same complete package graph is provided through the git-file index and local-directory registry.

Each backend also has a preseeded offline run. Its index metadata and package content are prepared first; an external
egress barrier or controlled observing proxy then denies package-network access for resolution, materialization, and
compilation together. The run starts from a clean workspace and compiler sandbox, may read only the preseeded backend,
and asserts that no undeclared network request was attempted.

Acceptance also covers:

- the transitive-only symbol affecting the evaluated result, proving that the complete graph is prepared and used;
- a pinned Git commit whose branch has advanced, proving that the lock—not the branch head—selects behavior;
- two clean temporary roots producing the same resolved graph and content digests;
- a poisoned ambient Elm cache, proving that only the prepared environment is visible;
- missing and tampered locked content failing before compilation;
- declared coordinate versus materialized `elm.json` name/version mismatch;
- ordinary public packages through the same boundary;
- credential redaction for authenticated acquisition.

Package-network denial is enforced outside the package workflow or observed by a controlled test proxy. Setting an
invalid proxy environment variable alone does not establish hermeticity. Tests assert the compiled consumer's
behavior, resolution graph, digests, and absence of egress across resolution, materialization, and compilation; they
never assert `ELM_HOME`, `registry.dat`, or cache paths.

Package URL and VERS parsing use the upstream language-neutral conformance vectors pinned in this note. The same
shared implementation and vectors run on JVM, JavaScript, and Native. Registered ecosystem types and the provisional
Morphir rules receive repository-owned vectors. Elm adapter tests cover observed compatibility behavior without
claiming ownership of an Elm purl type.

## Open questions

1. Is a Morphir-native package a distinct registry-level distribution, and what precisely is its package root?
2. Are Morphir versions exactly SemVer, compatible with another existing VERS type, or a new documented ordering?
3. Which Package URL types and VERS comparators must the first release validate semantically rather than preserve?
4. When is `repository_url` identity-significant for Morphir packages with no default repository?
5. What normalization produces the content digest while remaining portable across filesystems and archives?
6. Which operating-system mechanism gives CI a portable, observable package-workflow egress barrier?
7. What credential providers are in the first source-acquisition scope?

Filing Package URL proposals is not an open question or deliverable for this initiative. The project first needs
evidence that `pkg:morphir` works for real Morphir source packages, registries, locks, and cross-ecosystem references.

## Decision gates

An immutable package-management Decision Record is ready only after the first four open questions have explicit
answers, Package URL and VERS vectors validate the proposed shared model, and the unpublished-package fixture proves
the opaque Elm adapter boundary. Implementation that does not depend on those answers—buildkit task graphs, runtime
work, and restoration of Morphir-Elm generation—continues independently.
