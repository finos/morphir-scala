# Scala Wasm host spike

This is an unpublished experiment, not a replacement Morphir CLI. The approved
scope and remaining work live in `openspec/changes/spike-scala-wasm-host/`.

Use GraalVM 25.0.1 consistently for compilation, JVM probes and Native Image:

```sh
export MORPHIR_WASM_JAVA_HOME="$JAVA_HOME"
./mill --ticker false -i spikes.wasm-host.tools.fixtures
./mill --ticker false -i spikes.wasm-host.endive.run probe --guest out/spikes/wasm-host/tools/fixtures.dest/echo.wasm
./mill --ticker false -i spikes.wasm-host.graal.run probe --guest out/spikes/wasm-host/tools/fixtures.dest/echo.wasm
./mill --ticker false -i spikes.wasm-host.core.test
./mill --ticker false -i spikes.wasm-host.endive.nativeImage
```

`probe --help` documents repetition, lifecycle observation and result-file options.
The echo probe checks Unicode and binary byte callbacks and an `answer` export.
Its result explicitly identifies synthetic scope; it does not qualify MEP or a
real Morphir guest. `second.wasm` has a different answer, 43, and is loaded from
disk using the same executable with `--expected-answer 43`.

Fixtures are original WAT sources compiled by pinned `run.endive:wabt:1.2.0` in
the cached Mill task. Derived Wasm files remain under `out/`. The tools module
and its compiler/codegen dependencies are not on the host runtime classpaths.

## Extism binding experiment

```sh
./mill --ticker false -i spikes.wasm-host.tools.ffiGeneration
./mill --ticker false -i spikes.wasm-host.tools.nativeRuntime
```

The declarations in `ffi-api` compile against kyo-ffi 1.0.0-RC7 and are inspected
by its standalone Scala generator through Mill. The generator rejects the
pointer-bearing Extism host callback with
`callbackValueLayout: unsupported callback position type HandleT(morphir.spikes.wasmhost.ffi.CurrentPlugin)`.
The task retains `result.json`, including the failure and direct-FFM fallback
decision. It does not claim generated binding compatibility.

The native acquisition task pins Extism v1.41.0's macOS aarch64 archive to its
GitHub-published SHA-256 and writes an extracted file inventory with hashes.
Other native targets need their own verified inputs before running this task.
The native runtime is a distribution dependency, not part of the executable-only
size. No library is loaded from an unverified ambient search path.

```sh
export MORPHIR_EXTISM_LIBRARY="$PWD/out/spikes/wasm-host/tools/nativeRuntime.dest/libextism.dylib"
export MORPHIR_EXTISM_SHA256=6ea6a864e95b3e550675c98f6eb17b43cefe7d815ab6010a24b213762e0c646d
./mill --ticker false -i spikes.wasm-host.ffi.run probe --guest out/spikes/wasm-host/tools/fixtures.dest/echo.wasm
./mill --ticker false -i spikes.wasm-host.ffi.run safety --guest out/spikes/wasm-host/tools/fixtures.dest/echo.wasm --runaway out/spikes/wasm-host/tools/fixtures.dest/runaway.wasm
./mill --ticker false -i spikes.wasm-host.tools.nativeAbi
```

Direct FFM uses shared arenas for retained callbacks and per-call confined
arenas for arguments. Calls are serial within a session. A lifetime lock keeps
cancellation and teardown from freeing resources beneath a downcall. Native
Image includes explicit foreign descriptors and shared-arena support.

## Qualification and footprint

Run the opt-in tasks separately. Serialize Native Image builds with `-j 1`:

```sh
./mill --ticker false -j 1 -i spikes.wasm-host.tools.distribution
./mill --ticker false -j 1 -i spikes.wasm-host.tools.checks
./mill --ticker false -j 1 -i spikes.wasm-host.tools.qualify
./mill --ticker false -j 1 -i spikes.wasm-host.tools.checkpoint
./mill --ticker false -j 1 -i spikes.wasm-host.tools.report
./mill --ticker false -i spikes.wasm-host.tools.moonbitGuest
```

`report` includes five fresh lifecycle trials, 10 warm-up and 100 measured
calls, 20 worker sessions with two-second post-reap quiet intervals and a
two-worker observation. It profiles a synthetic byte guest and the pinned
Python legacy MEP compile workload. A failed configuration retains its exit and
stderr instead of becoming a zero memory observation. Raw task outputs stay in
`out/spikes/wasm-host/tools/`; copy the final evidence to the ignored `.dev/`
spike directory when recording a checkpoint.

`distribution` inventories installed logical bytes and SHA-256 identities,
creates normalized tar/gzip archives preserving executable permissions, and
keeps external Java runtime bytes separate. `checks` extracts each package into
a fresh directory and runs it, then checks help, usage, callback and memory
failures, second-guest loading and worker cancellation/reaping. Sampler tests
cover KiB normalization, missing observations, PID deduplication, exclusions
and concurrent aggregate peaks. The existing `LinkerBenchmarkProcess` informed
the process-tree sampling approach; the spike retains full time series and
phase events instead of only its peak summary.

`mep --guest FILE` is the experimental NDJSON worker transport. Requests and
responses are bounded at 1 MiB and require valid UTF-8. It suppresses all Extism
notifications and closes on exit, matching the pinned host behavior. It is not a general Morphir CLI.
`supervise` runs a probe in a worker of the same program and releases it with
Kyo Scope. Exit statuses are 3 for guest/output failure, 4 for deadline and 5
for explicit cancellation. Argument parsing errors are rejected before worker
launch. `safety` adds forced GC pressure as a separate diagnostic test; normal
profiles do not force GC.

Host JVM launches and Native Image builds enable
`--enable-native-access=ALL-UNNAMED`. The FFM native build also uses
`-H:+UnlockExperimentalVMOptions -H:+SharedArenaSupport` and the foreign-call
descriptors under `ffi/resources/META-INF/native-image/`. The common native
options are `--no-fallback -march=compatibility`; optimization and GC remain
the pinned toolchain defaults, recorded in build logs.

The offline evidence is in `.dev/.sdlc/scala-wasm-host/out/report/index.html`.
It contains 144 retained footprint runs, distribution file inventories, per-call
timings, MCK run/check receipts and lifecycle/retention charts. Fresh-process
trials do not flush OS or runtime caches. No-RSS-sampler timing runs still emit
phase/heap/GC events; fully instrumentation-free timings are unavailable.
The raw `heapCommittedBytes` field records `Runtime.totalMemory()`, which is
runtime-reported heap capacity rather than OS committed or resident memory.

When regenerating presentation or extending checks against retained delivery
builds, use `tools.runMain morphir.spikes.wasmhost.tools.Report` or `Checks` with
explicit existing input/output directories. This preserves the measured
distribution identities instead of rerunning profiling after a tools-only
change. The report retains input file digests; a changed host binary requires
new measurements. The umbrella `tools.report` task rebuilds its dependencies.

The pure Scala Extism compatibility subset accepts an explicit import/signature
list, bounds kernel allocations and cache state, provides empty WASI arguments
and environment, and denies filesystem/HTTP imports. It targets the pinned
Morphir guests. It does not claim general Extism compatibility. GraalWasm
25.0.1 lacks a public memory-limit option in its pinned option definitions, so
its experiment refuses an absent memory maximum when enforcing the 256 MiB
linear-memory ceiling. Endive preserves the declared initial pages and caps
the memory factory's maximum; FFM uses libextism manifest limits and a 64-bit
5,000,000,000 instruction budget.

The growth probe found a qualification failure in libextism 1.41.0: the manifest
`max_pages` value supplies a growth allowance that resets per call, rather than
an absolute per-instance ceiling. A one-page guest can grow by another 4096
pages despite `max_pages: 4096`. The FFM candidate therefore does not currently
meet the spike's strict 256 MiB limit for guests without declared maxima.
Its functional and footprint results remain separate from that failed limit
qualification. See the pinned runtime's `MemoryLimiter` in
`runtime/src/current_plugin.rs` and the retained `bounded-growth` receipts.

## Compiler hosting and execution speed

```sh
./mill --ticker false -j 1 -i spikes.wasm-host.tools.moonbitCompiler
./mill --ticker false -j 1 -i spikes.wasm-host.tools.benchmarks
```

`moonbitCompiler` acquires MoonBit Wasm compiler release
`v0.10.19+97801ee1b` with its published archive SHA-256, loader, core library,
revision markers and licenses. It inventories imports/exports independently of
the Morphir admission parser and tries the compiler module with all six hosts.
The current adapters refuse GC array types. The compiler's `bindings`, JavaScript
string/encoding imports and workspace require a separate compiler runtime
adapter. This result does not establish the underlying engines' full compiler
compatibility. Native moonc and Node are not substituted as successful hosts.
Compiler compilation latency and runtime memory remain unavailable for these
original six adapter configurations. The isolated polyglot experiment below
has separate results.

`benchmarks` runs five fresh processes per configuration/workload, with twenty
warm-up and one hundred measured invocations. It rotates configuration order
and runs serially without RSS sampling or artificial holds. Workloads cover
byte callbacks, 100,000 dependent i32 LCG steps, eight 64 KiB memory fill/read
passes, and the real Python compile request. The kernels consume independently
checked checksums and verify the byte round-trip. The memory fixture declares
headroom for Extism's kernel/input allocation while touching the same 64 KiB
in every measured pass.

Latency includes in-process adapter/callback/copying and safety costs. The
sequential throughput metric excludes launch, warm-up and parent/worker IPC;
its denominator is the sum of measured invocation durations. Raw warm-up and
measured samples, nearest-rank p50/p95/p99, trial median ranges, early/late
trends and startup-to-first-result are retained. Fixed warm-up does not prove
JIT stabilization. These results do not replace the earlier ten-warm-up
footprint protocol or provide a single engine ranking across workloads.

The aggregated report links the `speed/` and `moonbit-compiler/` experiments.
The first interrupted speed cohort used an overly restrictive memory-fixture
maximum and is retained separately under `out/speed-benchmarks/` in the ignored
spike evidence directory. It is excluded from the verified benchmark charts.

## GraalJS + GraalWasm MoonBit compiler proof

```sh
./mill --ticker false -j 1 -i spikes.wasm-host.polyglot.polyglotEvidence
```

This macOS arm64 experiment acquires a digest-verified GraalVM Community
25.5.5.0.3 distribution, aligns Polyglot/JS/Wasm artifacts to that release,
and acquires the pinned MoonBit compiler separately. It does not change the
original six delivery configurations. The generated report and raw receipts
are in `out/spikes/wasm-host/polyglot/polyglotEvidence.dest/`.

The JVM and Native Image compile the tiny `answer.mbt` fixture using the Wasm
compiler and execute the emitted Wasm with result 42. The second-source check
returns 43 without changing executable bytes. Invalid source, GC-array host
callbacks, exception tags, UTF-8, file cleanup and denied virtual filesystem
operations have explicit checks. The upstream JavaScript loader runs inside
GraalJS with a bounded runtime bridge. Neither Node nor native moonc executes
the compiler.

The unmodified loader fails at a repeated function import when its arity
decreases. `--unique-imports` enables a tested in-memory import-name adapter.
Receipts retain original/adapted module hashes and verify all non-import
sections are unchanged. This workaround is required evidence, not an assertion
of unmodified upstream compatibility or complete JavaScript string-builtin
coverage.

Packages inventory the application, external JVM runtime and separate shared
compiler payload, including core and licenses. Five fresh-process compile/run
trials per target collect timing without RSS sampling or phase holds. Five
additional trials sample process-tree RSS with 200 ms holds per phase. The
compiler is the parent process and has no workers; parent and aggregate RSS
therefore coincide. Samples exclude Mill, Native Image and the profiler.
Runtime heap observations, native build time and build peak RSS remain separate.
These trials do not measure warm compiler throughput or repeated long-lived
compiler sessions. Multi-file/core builds and compiler cancellation/isolation
qualification remain open in task 8.3.
