# Spec Delta

## Purpose

Provide reproducible evidence for deciding whether Scala can host Morphir Wasm extensions on the JVM and as a native executable. Qualification records the exact guests, protocol subset, enforced bounds and failures so an experiment cannot be mistaken for general ecosystem conformance.

## ADDED Requirements

### Requirement: Comparable execution matrix
Qualification SHALL report all three requested candidates on the JVM and as native executables, producing six configuration results. Comparisons within an ABI SHALL use identical guest bytes and request fixtures. Each result SHALL be pass, fail, blocked or not-run, with a reason and evidence. A failure in one configuration SHALL NOT suppress results for another candidate.

#### Scenario: One runtime cannot build natively
- **WHEN** a candidate's native build fails
- **THEN** the report records the failing command and diagnostics and continues independent configurations within the investigation budget

### Requirement: Runtime guest loading in a native host
A native-host result SHALL pass dynamic loading only when an already-built executable loads and executes a second guest supplied from disk that was absent from its build inputs. Qualification SHALL record the executable and both guest digests and SHALL distinguish bundled-guest execution from runtime loading.

#### Scenario: New guest after host compilation
- **WHEN** a native executable is built and then receives a previously unbundled guest
- **THEN** it executes that guest's request and verifies the expected response without rebuilding itself

### Requirement: Real Morphir guest compatibility
Qualification SHALL test the pinned `morphir-bytes-v1` and `extism-kernel-v1` profiles independently. Each successful profile SHALL exercise a real ecosystem guest through description, exact version negotiation, supported work, repeated requests and shutdown. Synthetic arithmetic or callback fixtures alone SHALL NOT establish Morphir host compatibility.

#### Scenario: Only the byte callback profile succeeds
- **WHEN** a real byte-callback guest passes but an Extism guest cannot link
- **THEN** the result identifies the byte profile as supported for the tested scope and reports the Extism failure without claiming general Wasm guest support

#### Scenario: Protocol tiers differ
- **WHEN** a guest supports legacy `0.1` and rich-model `0.2.0-draft.1`
- **THEN** qualification records each tier separately and verifies exact draft matching and the admitted operations for each tier

#### Scenario: Native Extism passes only its kernel profile
- **WHEN** the C API candidate passes the Extism kernel guest but byte-callback return semantics are unsupported or untested
- **THEN** the report limits its supported scope to the observed Extism profile and records the byte-profile outcome separately

### Requirement: Verified native runtime delivery
A candidate using native libraries SHALL record the exact C API version, library digests, target OS/architecture and runtime dependencies. It SHALL load verified artifacts from an explicit location and refuse a digest or target mismatch. Native-host qualification SHALL exercise the delivered distribution and SHALL NOT infer a single-file deployment from executable creation.

#### Scenario: Wrong native artifact
- **WHEN** a native library has the wrong digest or target architecture
- **THEN** the host refuses it before loading and reports the mismatch without searching an uncontrolled library path

#### Scenario: Native host requires a runtime library
- **WHEN** a native executable passes with an accompanying runtime library
- **THEN** the report identifies and tests that full distribution and its dependencies

### Requirement: Foreign callback and memory ownership
Qualification of native C ABI hosting SHALL exercise calls, retained callbacks, binary input/output, errors and cleanup on both targets. Callback state and native allocations SHALL remain valid until all calls and callbacks finish. Borrowed output SHALL be bounded and copied before invalidation. Callback failures SHALL remain within the host's error boundary.

#### Scenario: Retained callback survives registration
- **WHEN** the guest invokes a host callback on repeated calls after registration returns
- **THEN** the callback still has its own session state and no temporary or already-freed memory is accessed

#### Scenario: Next call replaces output
- **WHEN** the host accepts output and then invokes another guest call
- **THEN** the first response remains correct because the host owns its accepted bytes

#### Scenario: Host callback fails
- **WHEN** callback processing throws or encounters invalid pointer/length arguments
- **THEN** the host records a bounded failure without propagating a language exception across the C boundary

### Requirement: Protocol and callback integrity
Qualification SHALL verify request identity, Unicode bytes, bounded input and output, errors, profile-specific notification behavior and session state. Host callbacks SHALL be tested against guest-visible results. Undeclared imports and ABI declaration mismatches SHALL fail before guest execution without granting additional authority.

#### Scenario: Notifications through an Extism adapter
- **WHEN** initialized and exit notifications pass through the Extism profile
- **THEN** initialized produces no guest call or response, and exit closes the instance without invoking handle

#### Scenario: Byte callback guest returns a notification result
- **WHEN** a byte-callback guest handles a notification and returns its profile's notification sentinel
- **THEN** the host emits no response and preserves the session as prescribed by that profile

#### Scenario: Callback state is private
- **WHEN** one guest caches a value and a second session requests the same key
- **THEN** the first session reads its value and the second sees an empty cache

### Requirement: Cancellation and bounded execution
Qualification SHALL check deadline, explicit cancellation, traps, memory growth and excessive output in worker processes. Cancellation SHALL terminate and reap the worker before completion is reported. Receipts SHALL identify each enforced limit and SHALL NOT describe a deadline as instruction metering.

#### Scenario: Growth allowance resets without an absolute ceiling
- **WHEN** a backend limits growth per call but permits a guest to exceed the required per-instance memory ceiling
- **THEN** qualification records the ceiling check as failed, retains the reproducing guest and observations, and keeps functional/footprint results separate from that failed limit

#### Scenario: Guest never returns
- **WHEN** a guest loops inside an exported call and its deadline expires
- **THEN** the parent terminates and reaps the worker, reports the deadline outcome and remains able to start a healthy session

#### Scenario: Guest has no declared memory maximum
- **WHEN** the guest requests growth above the configured linear-memory ceiling
- **THEN** the host denies the growth and records the observed failure behavior and whether the call/session was invalidated

#### Scenario: Metering is unavailable
- **WHEN** a runtime has no instruction budget mechanism in the tested mode
- **THEN** its receipt explicitly reports instruction metering as not enforced and does not substitute a timing measurement

#### Scenario: Native call is cancelled
- **WHEN** a guest is running through the C API and the host cancels it from another thread
- **THEN** qualification observes whether the call actually returns, retains plugin/callback resources until it does, and terminates and reaps the worker if the bounded cancellation grace period expires

### Requirement: CLI lifecycle parity
The qualification CLI SHALL use the requested Kyo entrypoint and argument-processing integration on both execution targets. It SHALL provide help, reject invalid arguments, keep worker stdout limited to protocol frames, and preserve cancellation and cleanup behavior across targets.

#### Scenario: Invalid CLI request
- **WHEN** a user supplies an unknown runtime, malformed limit or missing required guest path
- **THEN** the CLI returns a nonzero usage status with a useful diagnostic and starts no worker

#### Scenario: Native cancellation
- **WHEN** cancellation interrupts a native CLI session
- **THEN** its registered finalizers run, worker processes are reaped and the cancellation status is distinguishable from a guest failure

### Requirement: Executable and package footprint
Qualification SHALL report executable or launcher bytes, installed distribution bytes and compressed package bytes for each produced configuration. It SHALL identify native libraries, resources, debug symbols and whether a Java runtime is bundled or required externally. Sizes SHALL name the exact artifacts and build settings. An absent executable SHALL be unavailable, not zero bytes.

#### Scenario: Small executable with native dependencies
- **WHEN** a native host requires accompanying runtime libraries
- **THEN** the report includes their bytes in the installed/package totals and retains the executable-only measurement separately

#### Scenario: JVM distribution uses an external runtime
- **WHEN** the application package expects Java to be provisioned separately
- **THEN** the report states that requirement and records the runtime identity and size separately from application bytes

### Requirement: Memory profile across lifecycle phases
Qualification SHALL capture cold startup, initialized idle, guest load, active calls, repeated calls and post-cleanup memory. It SHALL report per-process RSS and time-aligned parent/worker aggregate RSS with phase markers. Available heap, native allocation and guest-memory measures SHALL retain their definitions and coverage. Build peak memory SHALL be separate from runtime memory.

#### Scenario: Worker owns most guest memory
- **WHEN** a host launches a worker for a guest
- **THEN** the memory profile identifies both processes and their simultaneous aggregate rather than report only the parent

#### Scenario: Native allocations escape runtime tracking
- **WHEN** a runtime diagnostic omits allocations inside a native library
- **THEN** the report states that gap and retains OS process-memory observations without claiming the diagnostic measures total memory

#### Scenario: Runtime heap capacity differs from resident memory
- **WHEN** Runtime.totalMemory or Runtime.maxMemory reports a heap capacity larger than observed process RSS
- **THEN** the report names those APIs and their coverage, preserves the raw values, and does not label that capacity as measured OS committed or resident bytes

#### Scenario: Fresh process uses existing caches
- **WHEN** startup trials launch fresh processes without flushing OS or runtime caches
- **THEN** the report states that cache policy and does not claim a cache-cold machine measurement

#### Scenario: Independent peaks occur at different times
- **WHEN** the parent and worker reach their highest RSS at different sample times
- **THEN** the aggregate peak is the highest observed simultaneous sum, not the sum of their independent peaks

### Requirement: Retained memory observations
Qualification SHALL measure repeated calls in a live session and repeated session open/close cycles, retaining raw observations and post-cleanup samples for the surviving parent. It SHALL record cache and GC policy and distinguish retained process memory from reachable allocations. A single high RSS sample SHALL NOT establish a leak.

#### Scenario: Sessions finish while the parent stays alive
- **WHEN** repeated guest sessions are closed and their workers are reaped
- **THEN** the profile shows the surviving parent's memory and any retained state across cycles without substituting exited-worker memory for cleanup evidence

### Requirement: Comparable footprint evidence and charts
The final report SHALL include size comparison charts and phase-annotated memory timelines for the six configurations, marking absent observations explicitly. Comparisons SHALL identify workload, target, sampling/repetition protocol, build/runtime settings and profiler overhead. Raw data SHALL support each chart. Instrumented diagnostic results SHALL be separate from delivery-build baselines.

#### Scenario: One candidate cannot run
- **WHEN** a configuration is blocked before footprint measurement
- **THEN** its chart entry is marked unavailable with the blocker and no synthetic zero or interpolated result is plotted

#### Scenario: Diagnostic build changes footprint
- **WHEN** extra monitoring or debug settings change the binary or runtime
- **THEN** the report identifies that build separately and does not present it as the delivery-build baseline

### Requirement: MoonBit compiler hosting
Qualification SHALL separately assess executing the Wasm-based MoonBit compiler inside each of the six experimental host configurations. It SHALL pin compiler/runtime/core inputs and licenses, inspect required features and imports, and distinguish host-adapter rejection from engine capability. Native moonc, Node and remote compilation SHALL NOT count as hosted compiler success.

#### Scenario: Compiler depends on a companion runtime
- **WHEN** a compiler bundle requires JavaScript host functions, companion modules or features outside the current adapter
- **THEN** the report identifies exact dependencies and observed loading failures per configuration, and leaves dependent compilation and compiler-speed measurements blocked or not-run

#### Scenario: Hosted compiler produces executable output
- **WHEN** a configuration can execute the compiler with a bounded workspace
- **THEN** it compiles pinned valid MoonBit source and verifies the resulting Wasm's semantic result, rejects invalid source with bounded diagnostics, and checks repeated compilation, workspace isolation, cancellation and cleanup

#### Scenario: Compiler needs workspace access
- **WHEN** implementing the compiler-specific runtime adapter
- **THEN** its capability profile restricts reads to pinned toolchain/core and source inputs and writes to a private temporary workspace, without relaxing Morphir guest filesystem/environment/process/network admission

#### Scenario: Compiler delivery adds dependencies
- **WHEN** compiler hosting requires a core library, loader or auxiliary modules
- **THEN** size and lifecycle-memory reports include those dependencies separately from the base host and retain compiler startup/load/compile timings

### Requirement: Graal polyglot compiler proof
The follow-up SHALL test an isolated, version-aligned GraalJS and GraalWasm context against the pinned MoonBit compiler loader independently of Morphir ABI admission. It SHALL retain verified toolchain identities, actual JavaScript/Wasm feature probes, upstream loader identity and compatibility shims, bounded runtime authority, and separate JVM/Native Image outcomes. Original six-configuration evidence SHALL remain identifiable and unchanged.

#### Scenario: JavaScript runtime enables compiler hosting
- **WHEN** the upstream compiler can execute through the combined context
- **THEN** the proof compiles pinned MoonBit source, executes the emitted Wasm with its expected result, checks invalid-source diagnostics and retains commands/output identities

#### Scenario: Newer Wasm support leaves a JavaScript API gap
- **WHEN** a required string builtin, encoding function, tag or runtime API is unavailable
- **THEN** the proof records the precise failing feature and any tested compatibility shim without claiming compilation or Native Image success from GC support alone

#### Scenario: Repeated import names need a compatibility adapter
- **WHEN** the original compiler fails at the JavaScript/Wasm callback boundary and import aliasing is tested
- **THEN** the proof retains a minimal decreasing-arity regression on JVM and Native Image, the unadapted failure, adapted success, original/adapted SHA-256 hashes and a byte-level check that every non-import section is unchanged
- **AND** a positive compile/run result is described as adapted compiler support, with larger core/string-builtin and cancellation coverage reported separately

#### Scenario: The isolated compiler proof has different measurement scope
- **WHEN** the JVM and Native Image tiny-source proof produces runnable packages
- **THEN** its report retains five fresh compile/run timing trials and five separate RSS-sampled trials per target, source/output identities, package inventories, shared compiler/core/license bytes and external Java-runtime bytes
- **AND** it labels the compiler process as parent with zero workers, records phase holds and load-plus-compile timing boundaries, separates build RSS from runtime RSS, and leaves warm compiler throughput and full task 8.3 lifecycle qualification unavailable

### Requirement: Comparable execution-speed benchmarks
Qualification SHALL benchmark checked callback-boundary, integer-compute, memory-loop and real compile workloads across all six configurations. Within each workload it SHALL use identical guest bytes, inputs and work units, consume deterministic results, and report startup and post-warm-up execution separately. Failed or blocked configurations SHALL remain visible without invented timings.

#### Scenario: General execution speed differs by workload
- **WHEN** engines trade places between compute, memory and callback workloads
- **THEN** the report retains per-workload latency and throughput comparisons and does not infer an overall engine winner from one kernel

#### Scenario: Warm-up and runtime safety affect timing
- **WHEN** collecting speed samples
- **THEN** the report retains five fresh-process trials, twenty warm-up and one hundred measured calls per trial, rotated configuration order, raw samples, trial spread, pooled p50/p95/p99 latency, sequential-throughput definitions, early/late timing trends, metering/cache settings and instrumentation coverage

#### Scenario: Compiler hosting is blocked
- **WHEN** the MoonBit compiler cannot run in a host
- **THEN** its compiler-execution benchmark remains unavailable while independent generic and Morphir workload benchmarks continue

### Requirement: Reproducible and scoped conclusions
Receipts SHALL retain source revision and dirty state, toolchain and dependency versions, runtime mode, target OS/architecture, exact commands, artifact digests, request/response observations and failures. The final assessment SHALL separate host feasibility, native dynamic loading, component hosting and CLI replacement readiness. Missing prerequisites SHALL yield an inconclusive result for that question.

#### Scenario: A measurement is absent
- **WHEN** an experiment has not run or an artifact cannot be acquired
- **THEN** its result remains not-run or blocked and no timing, conformance or performance claim is inferred from documentation

#### Scenario: Guest qualification kit passes
- **WHEN** an upstream guest qualification corpus passes through the experimental host
- **THEN** the report identifies that exact corpus and guest profile and does not claim independent host admission or full CLI conformance
