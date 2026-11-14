# Proposal

## Why

We need an early, evidence-based answer to whether Scala can host Morphir extensions on the JVM and in a GraalVM Native Image executable, with Kyo managing execution and kyo-case-app processing CLI arguments. Compatibility, loading extensions after the host is built, executable/distribution size and runtime memory footprint must inform the decision before investing in a replacement CLI.

## What Changes

- Add an unpublished, JVM-only qualification module under `spikes/wasm-host/`, built through Mill and written in Scala. Keep its reports and generated outputs under `.dev/.sdlc/scala-wasm-host/out/` or Mill task outputs.
- Compare Endive's interpreter, GraalWasm and an Extism C API backend through JDK Foreign Function & Memory (FFM), on the JVM and in separate Native Image executables. Use the same guest bytes and requests within each supported ABI. Test native execution immediately after a minimal callback works, before building the protocol adapter.
- Check the pinned Kyo release's `kyo-ffi` runtime and standalone code generator first for the FFM binding. Invoke generation through Mill; if it cannot express the required ABI or native-image registrations within the timebox, record why and use minimal direct JDK FFM bindings for this candidate. Own the Scala/Morphir host layer while reusing the native `libextism` runtime.
- Load a second, previously unbundled guest into each already-built native executable. Compiling a fixed guest into the executable does not pass this gate.
- Exercise current Morphir `morphir-bytes-v1` and `extism-kernel-v1` ABIs, MEP lifecycle and real frontend/backend behavior. Bound partial results to the exact ABI, protocol version, operation and artifact tested.
- Exercise Kyo cancellation, scoped cleanup, instance isolation, bounded output and host callbacks. Run guests in workers of the same executable so the parent can terminate and reap a runaway worker.
- For the FFM candidate, qualify native downcalls and retained host-function upcalls, C struct/union layouts, borrowed output/error buffers, cancellation from another thread, and cleanup order. Acquire and package verified OS/architecture-specific native libraries with an explicit loading policy and record their runtime dependencies.
- Use kyo-case-app for the spike CLI, including worker mode, help, invalid arguments and meaningful exit statuses. Reuse the existing pinned Kyo version.
- Measure executable bytes, full installed distribution and compressed package size for each runnable configuration, accounting separately for native libraries, resources, debug symbols and any bundled Java runtime. Record build settings and artifact identities so a small launcher cannot hide a large deployment.
- Profile memory through cold startup, initialized idle, guest load, active/repeated calls and cleanup. Capture parent and worker processes, time-aligned aggregate RSS, available heap/native/guest-memory breakdowns, and retained memory across repeated session cycles. Keep build peak memory separate from runtime memory.
- Deliver size comparison charts and phase-annotated memory timelines across all six configurations, with raw samples, measurement methods, instrumentation overhead and explicit gaps for blocked configurations. Use comparable workload, build and runtime settings; do not invent size or memory acceptance thresholds.
- Load the released Wasm-based MoonBit compiler toolchain into the experimental hosts and test compiling MoonBit source. Pin the compiler bundle, companion runtime modules, JavaScript host ABI, core library and licenses independently of the Morphir MoonBit guest. Verify generated Wasm by executing its expected result. Native `moonc`, Node or a remote compiler may be controls but cannot count as compiler execution inside our Wasm hosts.
- Benchmark execution speed across all six configurations with identical checked callback-boundary, integer-compute, linear-memory-loop and real Python compile workloads. Separate process startup/load, post-warm-up invocation latency, sequential throughput and compiler-hosting latency. Retain five fresh-process trials, raw call samples, percentile and trial-spread summaries, balanced run ordering and runtime/metering settings. Failed workloads remain unavailable, with no overall winner inferred from a single kernel.
- Produce reproducible receipts and a recommendation: proceed with a named runtime and supported subset, reject a named combination, or remain inconclusive with a precise next experiment. Assess suitability for a host separately from suitability to replace the Morphir CLI.

The proposed effort remains two engineering days, with a first checkpoint after two hours and at most one hour diagnosing an individual blocked runtime/configuration before recording it and testing another candidate. Attempt the FFM callback/native-loading proof early, before investing in a complete Extism compatibility layer on the JVM runtimes. These are investigation budgets, not performance acceptance thresholds; missing later experiments remain explicitly not-run.

## Capabilities

### New Capabilities

- `wasm-host-qualification`: Repeatable execution and evidence collection for Morphir Wasm hosting across JVM and Native Image configurations, including Kyo lifecycle, CLI integration, native C ABI hosting, executable/package size and memory profiles.

### Modified Capabilities

None. The existing published-library and IR-ingestion requirements remain applicable and unchanged.

## Tracking

Initial planning and research: local bead `morphir-a9k`. Implementation: local bead `morphir-1k0`, including the added FFM candidate and footprint profiling. Beads data and code are not published by this proposal.

## Execution checkpoint

Execution began on 2026-11-10. The first six-configuration callback and footprint checkpoint was retained after 45 minutes, within the proposed two-hour checkpoint. All three candidates loaded external guests on JVM and Native Image. Kyo FFI RC7's generator rejected pointer-bearing callbacks, so the approved direct JDK FFM fallback is in use.

Real SDK and Python legacy MEP compile/generate flows pass on Endive and FFM on both targets. Their eight pinned MCK guest runs and independent report checks also pass. GraalWasm's pinned public API does not supply the required memory ceiling for guests without declared maxima, so those guests are refused. A focused growth probe found that libextism 1.41.0's manifest allowance resets per call; it does not establish the required absolute 256 MiB ceiling. These are qualification findings, not relaxed requirements. The portable MoonBit source build is blocked by a compiler/core mismatch.

The retained footprint matrix contains 144 runs across synthetic callbacks and real Python legacy compile work. The 24 failed runs are GraalWasm's refused real workloads; the other 120 completed. Installed native distributions measure 28.52 MiB for Endive, 71.73 MiB for GraalWasm and 51.25 MiB for FFM including libextism. Highest observed parent/worker RSS sums for real Python work are 276.83 MiB for native Endive and 182.31 MiB for native FFM. RSS sums can double-count shared pages and do not override functional or limit failures. Exact bytes, packages, five-trial timings, lifecycle timelines and 20-cycle retention observations are in `.dev/.sdlc/scala-wasm-host/out/report/`. Java runtime provisioning is accounted for separately. These macOS arm64 observations support continuing with Endive as a bounded host candidate; they do not approve a CLI replacement.

## Impact

The isolated GraalJS + GraalWasm compiler proof uses aligned release `25.5.5.0.3` and verified GraalVM Community JDK `25.0.5.0.3`. Both JVM and Native Image run the pinned Wasm MoonBit compiler and execute its generated 173-byte Wasm with result 42. This uses a bounded JavaScript runtime bridge and an explicit in-memory import-name adapter. The unadapted loader fails on repeated import names with decreasing function arity; the adapter changes only import names and verifies every non-import section is unchanged. Original compiler and adapted module hashes are retained. The measured Native Image build produced a 129.59 MiB executable in about one minute with 6.26 GiB build peak RSS. Preserve the original six 25.0.1-based measurements as their own baseline.

The separate polyglot experiment retains 12 positive/negative checks and 20 fresh-process trials. Both targets pass invalid-source diagnostics, file cleanup, GC callbacks, UTF-8/tags and workspace denials including a real symlink escape. JVM/native installed host bytes are 92.09/129.59 MiB and compressed packages 49.20/57.36 MiB. Shared compiler/core/licenses add 15.78 MiB installed and 3.33 MiB compressed; the JVM separately requires the inventoried 633.62 MiB JDK. Median process-to-verified-result latency is 1.902/0.637 seconds. Separate sampled trials observe median peak parent RSS of 973.23/316.14 MiB, with no workers. These are tiny-source cold-process observations, not warm compiler throughput or full compiler lifecycle qualification. Evidence is retained under `.dev/.sdlc/scala-wasm-host/out/report/polyglot-compiler/`.

The compiler-hosting experiment acquired release `v0.10.19+97801ee1b` with verified archive SHA-256 `ab55f4d92206916308922c768033d8a1b0eef5a2a3ea4c3ba8569c52cd40e2a9`. All six original Morphir adapters refuse its GC array types. Independent module inspection finds 73 compiler imports, comprising 72 functions and one exception tag. The separate polyglot proof establishes tiny-source compilation through a compiler-specific adapter; multi-file/core compilation, cancellation and long-lived compiler qualification remain open. Do not infer complete JavaScript string-builtin support from the tiny example.

The verified execution-speed matrix contains 120 fresh-process runs and 11,000 measured calls across four workloads. All generic workloads pass on all six configurations. The ten refused runs are GraalWasm's real Python workload. Native median-of-trial-medians for Python compile is 7.7092 ms with Endive and 0.0598 ms with FFM, including their current in-process adapters. GraalWasm is much faster than the Endive interpreter on the checked compute/memory kernels, but has no admitted real Python result under this policy. Timing results do not override functional, memory-limit or compiler-hosting failures. See `report/speed/` and `report/moonbit-compiler/` under the ignored evidence directory for raw data and separate verdicts. The interrupted first speed cohort is excluded from those charts.

- Proposed additions: `spikes/package.mill.yaml`, `spikes/wasm-host/package.mill.yaml`, Scala sources/tests/resources under that module, and a small named Mill helper trait/task if needed. Confirm existing parent-module conventions before adding the intermediate module.
- Existing integration points: `build.mill` `MorphirNativeImage`, `mill-build/src/millbuild/deps.scala`, and the MEP codec/provider under `morphir/langkit/elm/compiler/mep/`. The provider is a guest implementation, not a reusable host session. Share framing only where its semantics and visibility fit.
- Endive baseline: `run.endive:runtime:1.2.0`, adding `wasi` only if required by the selected real guest. Graal baseline: align Polyglot and Wasm language artifacts with the installed GraalVM `25.0.1`, preserving dependency JARs for Native Image instead of flattening them into an assembly. Resolve and record exact artifacts before running.
- Kyo baseline: repository `1.0.0-RC7`, including `io.getkyo::kyo-case-app`, already used by the repository's KB and squire tools. Add it to the managed suite if a build module needs it.
- FFM baseline: JDK/GraalVM 25.0.1. Verify matching `kyo-ffi` and `kyo-ffi-codegen` availability and APIs before adopting them. Pin the Extism C header and matching released native binaries together; record hashes, licenses, dependent libraries and target-specific layout/callback metadata. This candidate ships native runtime code on both JVM and Native Image, so a native host executable is not evidence of a single-file distribution.
- Measurement code remains Scala/Mill. Inspect the existing `LinkerBenchmarkProcess` process-tree RSS sampling and `LinkerBenchmark` heap/GC observations for reusable parts. Reports and charts are generated outputs; OS memory probes and supported runtime diagnostics must state their coverage and overhead, particularly for allocations inside native Extism.
- Pinned upstream contract evidence and differences between current host behavior and historical documentation are recorded in `design.html`.
- No published module, existing CLI, CI workflow, registry or extension installation changes are part of this spike. Component Model/WIT hosting is a separately reported feasibility question, not an assumed requirement of the current core-Wasm guest ABIs.
