# Proposal

## Why

morphir-scala carries two library families: the classic ZIO-based `org.finos.morphir` library and the newer Kyo-based
modules. Decision 0005 forbids bridging them, yet ZIO still reaches 35 of 164 build modules through the classic root,
and new work keeps paying for both stacks. The project means to replace the classic family with the Kyo modules, and
the only thing the Kyo side cannot yet do that a current consumer needs is read the v3 `morphir-ir.json` that
morphir-elm produces.

## What Changes

The change runs in two phases, separated by at least one published milestone.

**Phase 1 — replace, then deprecate (next milestone).**

- Add a Kyo-native reader that decodes morphir-elm v3 `morphir-ir.json` straight into the code model
  (`org.finos.morphir.codemodel.Distribution`), with no classic IR types and no ZIO on its classpath.
- Hold that reader to the shared Morphir Compatibility Kit (MCK) in finos/morphir: provide an MCK IR adapter that claims
  format version 3, and run `morphir mck run --suite ir` in CI against a pinned, verified Morphir CLI.
- Add `@version:3` cases built from real morphir-elm output to the MCK IR kit upstream in finos/morphir
  (`spec/ir/mck`), so every binding's v3 reader is held to the same contract.
- Drop `knowledge.logic`'s unused dependency on the classic root, which today puts ZIO on its classpath.
- Mark the published classic modules deprecated: a `### Deprecated` CHANGELOG entry, a deprecation notice in each
  module's scaladoc entry points and README text, and a pointer to the Kyo module that replaces each one.
- Record the retirement as a `kind: deprecation` intent and a `kind: removal` intent in the knowledge base.

**Phase 2 — remove (a later milestone).** It starts only when a Morphir CLI release whose kit contains the upstream
v3 cases is pinned here and the adapter passes them.

- **BREAKING** Stop building and publishing the classic family: `morphir` (the root library), `interop.zio.json`,
  `interop.borer`, `model.lowering`, `runtime.classic`, `tools` and `tests` on the JVM and Scala.js, plus the
  unpublished `testing.zio` and `testing.generators`. Versions already on Maven Central stay there.
- **BREAKING** Remove Scala evaluation of Morphir IR. The classic runtime has no Kyo replacement; building one is
  separate work.
- Remove what exists only to serve the classic family: the `runtime-generated-fixtures` and `runtime-tests` CI jobs,
  the `test:runtime-jvm` task, the classic runtime fixture and discovery wiring, `examples/morphir-elm-projects` and
  its `morphir-elm-projects` CI job (whose IR only the classic runtime consumes), the ZIO suites in the dependency
  pins, and the unwired classic leftovers under `morphir/meta`, `morphir/elm/facade` and `morphir/toolkit`.
- Leave no published module with ZIO on its runtime classpath.

## Capabilities

### New Capabilities

- `ir-v3-ingestion`: decoding morphir-elm v3 `morphir-ir.json` into the Kyo code model, and which v3 inputs are
  accepted, rejected or reported.
- `published-library-families`: which library families morphir-scala publishes, the effect stack they may depend on,
  and how a family is retired (deprecation notice first, removal in a later milestone).

### Modified Capabilities

None. No capability specs exist yet under `openspec/specs/`.

## Tracking

- Knowledge base: a new `kind: deprecation` intent and a new `kind: removal` intent, created during Phase 1. The
  removal intent supersedes intent 0028 (remove or absorb `morphir/toolkit`), whose scope it includes.
- Relevant records: decision 0005 (bridge nothing between ZIO and Kyo), decision 0013 (published library families),
  decision 0017 (deprecated IR formats are wire projections).
- Upstream dependency: a finos/morphir PR adding the `@version:3` cases to `spec/ir/mck`, and a Morphir CLI release
  that ships them. Phase 2 waits on both.
- Follow-up outside this change: declaring v4 in the adapter, which tests whether the code model's derived v4 JSON
  layout matches the specification's canonical v4 spellings.
- Optional task tracking follows AGENTS.md and `docs/task-tracking.md`.

## Impact

- **Consumers.** Users of the 14 published classic artifacts (`morphir_3`, `morphir_sjs1_3`,
  `morphir-interop-zio-json_*`, `morphir-interop-borer_*`, `morphir-model-lowering_*`, `morphir-runtime-classic_*`,
  `morphir-tools_*`, `morphir-tests_*`) must move to the Kyo modules or stay on a released version. Scala evaluation of
  Morphir IR has no replacement after Phase 2.
- **Code.** About 490 Scala source files in the classic modules, tests included, are removed in Phase 2.
  `model.lowering` is replaced by the new reader.
- **Build and CI.** Phase 1 adds an `mck-ir` CI job and acquires the Morphir CLI. Phase 2 removes three CI jobs
  (`runtime-generated-fixtures`, `runtime-tests`, `morphir-elm-projects`), the classic mise tasks and
  `examples/morphir-elm-projects`. The ZIO version pins and suites in `mill-build` go once nothing uses them.
- **Docs.** AGENTS.md (test framework and dependency guidance), module CONTRIBUTING files, and knowledge-base
  Capability and Design Note pages that describe the classic library.
- **Unaffected.** The MEP provider, langkit, buildkit, kit, model, naming, extensibility, connector, appkit,
  knowledge.okf, ui, web, `lib.interop` and the Mill plugins do not depend on the classic family.
