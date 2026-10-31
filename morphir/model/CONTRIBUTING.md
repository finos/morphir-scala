# Contributing to `morphir/model`

This file covers dev notes and known follow-up work specific to the `morphir/model` module tree (`morphir/model` -
the code model / v4 IR and its `datamodel.Val` runtime value model, and `morphir/model/compat/v3` - `V3Reader`, which
reads v3 `morphir-ir.json` into the code model, and `V3WireProjection`, which writes it back) that don't belong in the root [CONTRIBUTING.md](../../CONTRIBUTING.md) (project governance/process) or
[AGENTS.md](../../AGENTS.md) (general AI agent guidelines). See those first; this file only covers what's local to
this module tree.

The design spec and task-by-task progress ledger for the work that built this module tree (the kyo-based code model,
`Val`, kyo-schema derivation, and the v3 lowering) live under the gitignored `.dev/.sdlc/` and `.superpowers/`
directories, so none of it is visible from a checkout of this repository. The items below are extracted from that
record so the follow-up work is findable by anyone who doesn't have the original session. Each is a candidate for a
tracked issue; none is fixed by this commit unless said otherwise.

## Running the Morphir Compatibility Kit against the v3 reader

The Morphir Compatibility Kit (MCK) from finos/morphir holds the cases that a conforming IR reader must pass. The
`morphir` CLI contains the kit. `morphir/model/compat/v3/mck` is an adapter that connects the kit to the v3 reader. It
claims IR version 3, the `json` profile and the `single` layout. The kit skips every case that needs a different
version, profile, layout or node kind.

Run the IR suite:

```sh
mise run mck:run
```

The task runs `./mill --ticker false -i morphir.model.compat.v3.mck.mckRun`. This command:

1. builds the adapter launcher;
2. downloads the pinned `morphir` CLI and verifies its SHA-256 (one time; the file stays in the machine cache);
3. runs `morphir mck run --suite ir` with the kit in the CLI;
4. writes `.dev/mck/report.json` and `.dev/mck/report.html`;
5. runs `morphir mck report check` against `morphir/model/compat/v3/mck/allowed-failing.json`.

Step 5 is the gate. It fails when a case fails that the baseline does not list, when the baseline lists a case that
passes, or when no case passes. Open `report.html` to see each case. To write the reports to a different directory,
give it to the command: `./mill --ticker false -i morphir.model.compat.v3.mck.mckRun --reportDir .dev/mck-other`.

To run cases that the pinned CLI does not have yet, give the command a kit directory with `--kit`, for example a
finos/morphir checkout's `spec/ir/mck`:
`./mill --ticker false -i morphir.model.compat.v3.mck.mckRun --reportDir .dev/mck-next --kit <checkout>/spec/ir/mck`.
The run and the report check both use that kit. The CLI resolves the kit's `text` fences against the checkout, and the
report check reads other files of the checkout as well (`spec/mck`, `website/static/schemas`), so give it a whole
checkout, not a copy of `spec/ir/mck` alone.

The run does not use `--strict`. That flag fails the run for every skip, and every v4 case is a skip for this adapter.

To send requests to the adapter yourself, build the launcher and write one JSON request on each line:

```sh
./mill --ticker false morphir.model.compat.v3.mck.launcher
printf '%s\n' '{"id":1,"op":"capabilities"}' '{"id":2,"op":"exit"}' | out/morphir/model/compat/v3/mck/launcher.dest/run
```

The CLI version and the checksum of each platform's archive are in `mill-build/src/millbuild/MorphirCli.scala`. The
kit version changes with the CLI version. To move to a new release, change `Version` and all six checksums together.
`MORPHIR_TOOLS_CACHE` sets a different machine cache. `MORPHIR_TOOLS_OFFLINE=1`, or Mill's `--offline`, stops the
download.

## Known follow-ups

### 1. VFS directory-layout loading was deleted and not restored

`VfsLoader` and `DistributionLoader` were removed together with the dead v4 zio-json codecs they supported.
`CodeModelCodecs` (in `morphir/model/src/org/finos/morphir/codemodel/CodeModelCodecs.scala`) restores
single-document `Distribution` encode/decode via derived kyo-schema, but there is currently no code that reads a
distribution spread across a directory tree (a "VFS" layout - one file per module, a manifest describing the whole).

`VfsManifest` and `DistributionMode` (in
`morphir/model/src/org/finos/morphir/codemodel/VfsDistribution.scala`) survive as orphaned types: they describe a
format nothing in the current codebase can read or write. Either:

- document them as the forward contract for a VFS loader that's planned but not yet built, or
- drop them until a loader exists, so a reader doesn't infer support that isn't there.

### 2. `Val.Closure` cannot represent a pattern-binding lambda

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

### 3. Real-IR coverage comes from the MCK

`V3Reader`'s unit tests in `morphir/model/compat/v3/test` use hand-written JSON. Real morphir-elm output reaches the
reader only through the MCK's `@version:3` cases, which the `mck-ir` CI job runs (see above). Add a new real-output
case to finos/morphir's `spec/ir/mck`, not as a local fixture, so that every binding's v3 reader must pass it.

Before the classic modules were removed, every IR file that morphir-elm generated from `examples/morphir-elm-projects`
and `morphir-elm/sdks` (15 files) decoded through `V3Reader` to the same code model as through the classic path. That
check was removed with those projects.

### 3a. The reader's determinism sorts are required

`V3Reader` sorts at three sites. Each sort makes the decoded code model reproducible:

| Site | Sorts |
| ---- | ----- |
| `V3TypeDecoding.constructors` | a custom type's constructors, by name |
| `V3ValueDecoding.byName` | `LetRecursion` definitions and `UpdateRecord` fields, by name |
| `V3Decoding`, the `Bundle` case | the bundle's libraries, by package name |

v3 holds these as a map, and Scala does not guarantee `Map` iteration order. Without the sorts, the same input can
decode to two equivalent but different code models. The derived JSON is then not reproducible, and a golden fixture
cannot be checked by hand. The sort gives *a* reproducible order. It does not recover the declaration order, which
v3's map had already lost. `Name` has no `Ordering`, so the key is `.toList`.

The constructor and by-name sites have a comment that says so; the `Bundle` site does not. **Do not remove the sorts as
redundant.** They make only these sites
order-stable, not the whole tree. To make the whole tree order-stable, the code model must stop using `Map`.

### 3b. What kyo-schema derivation costs at compile time

Measured on `morphir/model` when `derives Schema` was introduced, alternating `./mill clean morphir.model.jvm`
with `time ./mill morphir.model.jvm.compile` between the pre- and post-derivation states:

| State | Steady-state compile |
| ----- | -------------------- |
| 8 sources, no `derives Schema` | ~1.8-4.0s, median ≈2.9s |
| 9 sources, full `derives Schema` closure | ~4.3-6.8s, median ≈5.6s |

So roughly +2.7s, under 2x - against a stop condition of 3x, which is why derivation over the whole
`Type`/`Expr` closure was accepted rather than narrowed. Derivation succeeded in a single pass over all 23
product/sum types with no manual intervention beyond the instances in `NamingSchemas.scala`.

Treat this as a rough floor, not a precise number: it is a 9-file module on a warm Mill daemon, so JVM and
class-loading time dominate and the run-to-run variance is large relative to the delta. **If `codemodel` grows
substantially, re-measure rather than assuming this still holds.**

### 4. One kyo version, and one place that names it (resolved)

This module tree used to require a `1.0.0-RC5+…-SNAPSHOT` build of kyo, because the kyo-schema derivation fixes it
depends on had not been released. It is now on `1.0.0-RC6`, which carries them, so the Sonatype snapshot repository
and the `MorphirSnapshotRepos` trait that added it are both gone.

At the same time, eight modules that hardcoded a kyo version literal in their `package.mill.yaml` - `langkit/core`,
`langkit/trees`, `langkit/elm/core`, `langkit/elm/compiler/api`, `kit/kyo`, `knowledge/logic`, `intelligence` and
`intelligence/sdk` - moved onto the `Morphir*MvnDeps` traits in `build.mill`, which source their version from
`Versions.kyo` alone. That is what keeps the guarantee: Coursier resolves a single version per artifact coordinate
across the whole build, so a second literal anywhere is a silent version unification waiting for the first module
that depends on both sides.

**When adding extra artifacts from a versioned suite, `mvnDeps: !append` them without a version.**
`MorphirSuiteBom` pins every suite in `Deps.managedSuites` through Mill `depManagement`. Kyo and upickle are the
suites. Neither publisher is imported as a Maven BOM; the pin is ours. Suite
traits still *fetch* a bundle that always travels together: `MorphirKyoCoreMvnDeps` (kyo-core, kyo-prelude),
`MorphirKyoTestMvnDeps` (those two plus kyo-test-api and kyo-test-runner), `MorphirKyoMvnDeps` (kyo-data,
kyo-schema), `MorphirKyoSchemaMvnDeps` and `MorphirKyoSchemaJsonMvnDeps`. Do not add
a new trait for each extra artifact. A YAML block that also has non-suite deps declares them with
`mvnDeps: !append` too, since a bare `mvnDeps:` replaces what the trait contributed rather than adding to it.

### 5. Split packages across artifacts

`org.finos.morphir` spans two published modules: `morphir/naming` and `morphir/lib/interop`. Maven and Coursier
consumers are not affected, because transitive resolution hides split packages at that layer. A split package does
prevent a JPMS `module-info` or an OSGi bundle for that package.

### 6. `morphir/naming` has its own tests (resolved)

`morphir/naming`'s specs lived in `morphir/tests`, which was removed with the classic library. They now run in
`morphir.naming.{jvm,js}.test` as kyo-test classes (`NameTests`, `PathTests`, `QNameTests`), with every case kept.
The old `QualifiedModuleNameSpec` held no live test, and `FQNameSpec` was commented out, so neither was ported.

### 7. Stack safety in the reader

`V3Reader`'s type, value and pattern decoders (`V3TypeDecoding`, `V3ValueDecoding`) call themselves directly, with no
trampoline or explicit stack. `V3WireProjection` uses `scala.util.control.TailCalls`, and its tests write 10,000
nested levels. Kyo's JSON parser refuses input nested deeper than 512 levels, and that limit also bounds the depth
of the decoders' recursion. No test reads a document near that limit, on the JVM or on Scala.js.

## Also see

- `Expr.scala`'s file header (`morphir/model/src/org/finos/morphir/codemodel/Expr.scala`) documents a separate,
  already-tracked debt: the v4 draft spec bundle under `kb/bundles/morphir/morphir-ir-v4-draft/` still calls this
  type `Value`, while the code renamed it to `Expr`. That header is the source of truth for what needs updating
  there (`value-expressions.md`, `value-specifications-and-definitions.md`, and a `design/divergences.md` entry) -
  it's cross-referenced here so the full follow-up list for this module tree is findable from one place.
