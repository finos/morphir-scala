# Contributing to `morphir/model`

This file covers dev notes and known follow-up work specific to the `morphir/model` module tree (`morphir/model` -
the code model / v4 IR, its `datamodel.Val` runtime value model, and `morphir/model/lowering` - the v3 -> code model
lowering) that don't belong in the root [CONTRIBUTING.md](../../CONTRIBUTING.md) (project governance/process) or
[AGENTS.md](../../AGENTS.md) (general AI agent guidelines). See those first; this file only covers what's local to
this module tree.

The design spec and task-by-task progress ledger for the work that built this module tree (the kyo-based code model,
`Val`, kyo-schema derivation, and the v3 lowering) live under the gitignored `.dev/.sdlc/` and `.superpowers/`
directories, so none of it is visible from a checkout of this repository. The items below are extracted from that
record so the follow-up work is findable by anyone who doesn't have the original session. Each is a candidate for a
tracked issue; none is fixed by this commit unless said otherwise.

## Known follow-ups

### 1. The classic runtime's test suite does not run

`morphir/runtime/classic/test/jvm/src/` holds 4400 lines across six files: `EvaluatorMDMTests.scala` (3652 lines),
`DefaultsSpec` (241), `UnitTestingSpec` (234), `TypeCheckerTests.scala.scala` (146), `GatherRefsSpec` (77),
`parsing/ParseTest` (50). Mill's cross-platform source layout reads `test/src` and `test/src-jvm`, not `test/jvm/src`
(see the root [AGENTS.md](../../AGENTS.md#cross-platform-sources)) - so none of this compiles as part of
`morphir.runtime.classic.jvm.test`, and `morphir.runtime.classic.jvm.test` actually runs 7 tests, not the several
hundred this suite implies. `test/src/EvaluatorQuickSpec.scala` is additionally commented out in its entirety.

**This is pre-existing on `main`, not caused by any work in this module tree** - the files were moved verbatim from
their previous location. It was mistaken for a passing 500+-test suite during this work (a Mill task-count line was
misread as a test count); flagging it here so the mistake doesn't repeat.

Diagnostic finding: the suite *does* compile once its sources are pointed at `test/jvm/src` (a `package.mill.yaml`
fix). It then fails at runtime with `FileNotFoundException` for
`examples/morphir-elm-projects/evaluator-tests/morphir-ir.json`, which is **gitignored** - it's `morphir-elm make`
output, produced by actually running the Elm toolchain (see `MorphirElmModule.dist` /
`incrementalMakeSourceFiles` in the root `build.mill`). The tracked fixture in the same directory is
`morphir-ir2.json`, a different file. So resurrecting this suite needs either the Elm toolchain wired into CI, or the
suite's fixture references repointed at `morphir-ir2.json` (and verified to still make sense against that IR).

**Record this prominently for whoever plans the next slice of work on the runtime**: if that plan assumes this suite
acts as a differential oracle (asserting the new `Val`-based evaluator against the classic one), it currently cannot,
because it does not run at all.

### 2. VFS directory-layout loading was deleted and not restored

`VfsLoader` and `DistributionLoader` were removed together with the dead v4 zio-json codecs they supported.
`CodeModelCodecs` (in `morphir/model/src/org/finos/morphir/codemodel/CodeModelCodecs.scala`) restores
single-document `Distribution` encode/decode via derived kyo-schema, but there is currently no code that reads a
distribution spread across a directory tree (a "VFS" layout - one file per module, a manifest describing the whole).

`VfsManifest` and `DistributionMode` (in
`morphir/model/src/org/finos/morphir/codemodel/VfsDistribution.scala`) survive as orphaned types: they describe a
format nothing in the current codebase can read or write. Either:

- document them as the forward contract for a VFS loader that's planned but not yet built, or
- drop them until a loader exists, so a reader doesn't infer support that isn't there.

### 3. `Val.Closure` cannot represent a pattern-binding lambda

`Val.Closure` (in `morphir/model/src/org/finos/morphir/datamodel/Val.scala`) binds a `Chunk[Name]`. `Expr.Lambda`
(the code model's expression node) binds a `Pattern`, and `Pattern` includes `TuplePattern`, `ConstructorPattern`,
`HeadTailPattern` and `AsPattern` alongside the simple by-name case. An Elm lambda that destructures its argument -
`\(x, y) -> x + y`, or `\(Just x) -> x` - therefore has no representable closure under the current `Val` shape.

A future evaluator over `Val` must either:

- desugar a pattern-binding lambda to a synthetic single fresh-name binding plus a `PatternMatch` on that name (no
  `Val` shape change, but the evaluator carries the desugaring), or
- change `Val.Closure` to carry `Chunk[Pattern]` instead of `Chunk[Name]` (a `Val` wire-format change).

**Undecided.** This must be resolved before the next slice of work commits to a wire format for `Val`, since
changing `Val.Closure`'s shape after `Val` ships anywhere is a breaking change.

### 4. Real-IR coverage is one fixture, one distribution kind

`morphir/model/lowering`'s `RealIrCodecRoundTripSpec` (`morphir/model/lowering/test/src-jvm/.../RealIrCodecRoundTripSpec.scala`)
runs against exactly one real fixture (`examples/morphir-elm-projects/evaluator-tests/morphir-ir2.json`), and that
fixture is a `Library`-only distribution - v3's `Specs` and `Application` distribution kinds, and `Bundle`, are
exercised only by hand-constructed unit fixtures in `V3LoweringSpec`, never by real compiler output. The golden
corpus under `morphir/interop/zio/json/test/resources/golden/` is a second source of real IR that nothing in
`morphir/model/lowering` currently reads. Broadening coverage to that corpus (and to a `Specs`/`Application` fixture
if a real one can be sourced) is a named, not-yet-started follow-up.

### 5. Two kyo versions coexist in the dependency graph

`Versions.kyo` (`mill-build/src/millbuild/deps.scala`) pins the `1.0.0-RC5+50-7da9d49b-SNAPSHOT` build that
`morphir/model` and everything depending on it require (kyo-schema derivation needs fixes only in the snapshot - see
the kyo-related task notes this file's history is drawn from). Four `langkit` modules - `langkit/core`,
`langkit/trees`, `langkit/elm/core`, `langkit/elm/compiler/api` - still hardcode the released `1.0.0-RC5` in their
`package.mill.yaml`. Their classpaths are currently disjoint from `morphir/model`'s, so there is no live conflict
today, but Coursier resolves a single version per artifact coordinate across a build - the moment something depends
on both sides (`morphir.kit.kyo` looks like the likely meeting point, since `kit/` modules bridge upstream libraries
into the kyo ecosystem this codebase uses), Coursier will unify to the snapshot and the four `langkit` modules will
silently start building against a version they didn't pin. Worth moving them onto `Versions.kyo` before that
happens, rather than after something breaks from it.

### 6. Split packages across artifacts

`org.finos.morphir` now spans two published artifacts: `morphir` and `morphir-naming`. `org.finos.morphir.runtime`
spans `morphir` and `morphir-runtime-classic`. Maven/Coursier consumers are unaffected (transitive dependency
resolution makes split packages invisible at that layer), but it forecloses JPMS `module-info` and OSGi bundling for
either package, should that ever be wanted.

Separately: `org.finos.morphir.datamodel` (the runtime value model, including `Val`) is owned by `morphir-model`,
while its child package `org.finos.morphir.datamodel.classic` (the pre-existing MDM-based runtime value model) is
owned by `morphir` (the root module) - which does **not** depend on `morphir-model`. The parent/child package-name
reading is therefore conceptually right (`.classic` genuinely is the older sibling of the new `datamodel` types) but
dependency-inverted: someone tracing `org.finos.morphir.datamodel.classic` upward expecting to land in the
`morphir-model` jar (because that's where its parent package lives) will look in the wrong artifact.

### 7. `morphir-runtime` is no longer published

`morphir/runtime/package.mill.yaml` is now an empty container module (the actual runtime moved to
`morphir/runtime/classic`, publishing as `morphir-runtime-classic_3`). This means `org.finos.morphir:morphir-runtime_3`
silently stops being produced by this build. Nothing currently catches this class of change:
`mimaPreviousArtifacts` is not configured for these modules, and `mimaReportBinaryIssues` is not among the globs CI
runs. Any downstream consumer of `morphir-runtime_3` needs to move to `morphir-runtime-classic_3`; this is worth a
release note when this lands in a published version, since nothing else will tell them.

### 8. `morphir/naming` has no test module of its own

`morphir/naming`'s specs currently live in `morphir/tests`, which depends on the full core `morphir` module. So the
one module whose entire selling point is an empty dependency closure (see its module docstring / `AGENTS.md`
guidance on why `morphir/naming` was extracted) cannot be tested without building all of `morphir` first. Giving
`morphir/naming` its own `test/` would let it be verified in isolation, matching its design intent.

### 9. Stack safety in the lowering

`V3Lowering.lowerType`, `lowerExpr` and `lowerPattern` (`morphir/model/lowering/src/.../V3Lowering.scala`) all
recurse without any stack-safety mechanism (trampolining, explicit stack, etc.). The 25-module real-IR fixture used
by `RealIrCodecRoundTripSpec` passes comfortably, but a deeply nested `Apply` chain (e.g. from generated or
machine-produced IR, or a long `a |> f |> g |> h |> ...` pipeline) could overflow the JVM stack. Not urgent while the
lowering is a one-off migration path; relevant if it becomes something invoked repeatedly in a production pipeline.

## Also see

- `Expr.scala`'s file header (`morphir/model/src/org/finos/morphir/codemodel/Expr.scala`) documents a separate,
  already-tracked debt: the v4 draft spec bundle under `kb/bundles/morphir/morphir-ir-v4-draft/` still calls this
  type `Value`, while the code renamed it to `Expr`. That header is the source of truth for what needs updating
  there (`value-expressions.md`, `value-specifications-and-definitions.md`, and a `design/divergences.md` entry) -
  it's cross-referenced here so the full follow-up list for this module tree is findable from one place.
