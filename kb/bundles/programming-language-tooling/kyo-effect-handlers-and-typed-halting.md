---
type: Reference
title: Kyo effect handlers and typed halting
description: "Record verified Kyo 1.0.0-RC6 ArrowEffect handler variants, typed-halting shapes, and executor-relevant pitfalls for task-graph interpreter design."
tags: [kyo, effects, pipeline]
status: draft
verified:
  by: claude/fable-5
  at: 2026-08-12T00:00:00Z
sources:
  - id: kyo-rc6
    resource: https://github.com/getkyo/kyo/tree/2e58c0550b209317b85a30fc5787c24b7e4dd63c
    title: Kyo 1.0.0-RC6
  - id: kyo-arrow-effect
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-kernel/shared/src/main/scala/kyo/kernel/ArrowEffect.scala
    title: ArrowEffect at 1.0.0-RC6
  - id: kyo-abort
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-prelude/shared/src/main/scala/kyo/Abort.scala
    title: Abort at 1.0.0-RC6
  - id: kyo-contributing
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/CONTRIBUTING.md
    title: Kyo effect implementation reference
  - id: kyo-emit
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-prelude/shared/src/main/scala/kyo/Emit.scala
    title: Emit at 1.0.0-RC6
  - id: kyo-stream
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-prelude/shared/src/main/scala/kyo/Stream.scala
    title: Stream at 1.0.0-RC6
  - id: kyo-choice
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-prelude/shared/src/main/scala/kyo/Choice.scala
    title: Choice at 1.0.0-RC6
  - id: kyo-isolate
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-kernel/shared/src/main/scala/kyo/kernel/Isolate.scala
    title: Isolate at 1.0.0-RC6
  - id: kyo-var
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-prelude/shared/src/main/scala/kyo/Var.scala
    title: Var at 1.0.0-RC6
  - id: kyo-llm
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-ai/shared/src/main/scala/kyo/LLM.scala
    title: LLM effect at 1.0.0-RC6
  - id: kyo-readme
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/README.md
    title: Kyo README effect-ordering example
  - id: kyo-kernel-readme
    resource: https://github.com/getkyo/kyo/blob/2e58c0550b209317b85a30fc5787c24b7e4dd63c/kyo-kernel/README.md
    title: Kyo kernel README
  - id: morphir-elm-parse
    resource: https://github.com/finos/morphir-scala/blob/dbf53136888fd7810b70a51e7f4622c5f519d18a/morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala
    title: morphir-scala ElmParse effect
  - id: morphir-flux-engine
    resource: https://github.com/finos/morphir-scala/blob/dbf53136888fd7810b70a51e7f4622c5f519d18a/morphir/contrib/knowledge/src/morphir/kb/logic/core/FluxEngine.scala
    title: morphir-scala FluxEngine
  - id: morphir-query-logic
    resource: https://github.com/finos/morphir-scala/blob/dbf53136888fd7810b70a51e7f4622c5f519d18a/morphir/langkit/trees/src/morphir/langkit/trees/query/QueryLogic.scala
    title: morphir-scala QueryLogic
---

# Kyo effect handlers and typed halting

At the pinned Kyo baseline, an interpreter for a domain effect (including a task-graph executor) is one
`ArrowEffect` handler. Halting is not a special API: a handler halts by producing a value of the handled
computation's final result type instead of calling the continuation. The result type therefore fixes the set of
halt outcomes the interpreter can express. That is where "typed halting" comes from.[^kyo-arrow-effect]

This document is a companion to [Scala 3 and Kyo implementation notes](/scala-3-and-kyo-implementation-notes.md),
which pins the same `1.0.0-RC6` baseline. Each document carries its own `verified` entry recording who checked its
claims against that pin, and when. When the project's Kyo version moves again, both documents need their own
re-verification; neither vouches for the other.

## Baseline and freshness

| Component | Baseline used here | Source of truth |
| --- | --- | --- |
| Kyo | `1.0.0-RC6` | `Versions.kyo` in `mill-build/src/millbuild/deps.scala` |
| Kyo source | commit `2e58c0550b209317b85a30fc5787c24b7e4dd63c` | annotated tag `v1.0.0-RC6`[^kyo-rc6] |

If the project's Kyo version moves, treat every claim here as provisional immediately: recheck the cited signatures
against the new tag, then move this pin.

## Declaring an effect

`abstract class ArrowEffect[-Input[_], +Output[_]] extends Effect` lives in `kyo-kernel`, not `kyo-prelude` (the
prelude holds the concrete effects).[^kyo-arrow-effect] A concrete effect is a `sealed trait` extending it, with
`Input` and `Output` built from `Const[X]` (ignore the type parameter) or `Id` (the answer type is the operation's
own type parameter):

```scala
sealed trait Abort[-E] extends ArrowEffect[Const[Error[E]], Const[Unit]]
sealed trait Emit[-V] extends ArrowEffect[Const[V], Const[Unit]]
sealed trait Choice extends ArrowEffect[Seq, Id]
```

Operations suspend through `ArrowEffect.suspend` (request only) or `suspendWith` (request fused with a transform of
the answer). Two encodings exist for a multi-operation effect:

- An indexed GADT `Op[A]` with `Output = Id`. Each case fixes its own answer type, the compiler checks the handler
  match for exhaustiveness, and the continuation needs no cast beyond the standard `asInstanceOf[C]` idiom. Kyo's
  own `LLM` effect[^kyo-llm] and morphir-scala's `ElmParse`[^morphir-elm-parse] both use this.
- An untagged union `Op[V]` disambiguated by runtime match, as `Var` does.[^kyo-var] The match is order-dependent
  and unchecked; the GADT is the safer default for new effects.

An operation that must never resume declares its answer type as `Nothing`. `Abort.error` is the template: the
suspend site returns `Nothing < Abort[E]` with an unreachable transform. This is legitimate precisely because the
only handler for that operation never calls the continuation.[^kyo-abort] morphir-scala's `ElmParseOp.Halt
extends ElmParseOp[Nothing]` is the same idiom.[^morphir-elm-parse]

## Handler variants

All variants sit in `ArrowEffect`'s companion.[^kyo-arrow-effect] Every variant takes the effect's `Tag`, the
computation, and a polymorphic handler function receiving the operation input and the continuation.

| Variant | State | Fits |
| --- | --- | --- |
| `handle` (1 effect) | none | Uniform per-occurrence interpretation |
| `handle` (2–4 effects) | none | Interleaving several effects in one loop, when nesting would fix an ordering you do not want |
| `handleFirst` | none | Peel exactly one occurrence and get the continuation back **as a value**; the caller owns the driving loop |
| `handleLoop` | optional `State`, optional `done` | The interpreter workhorse: thread private state across resumptions, fold `(State, A)` into a report type in `done` |

The `handleLoop` body returns `Loop.continue(state, cont(answer))` to resume or `Loop.done(finalValue)` to stop the
loop and discard the rest of the computation. `Loop.Outcome` is an unboxed union, so `done` allocates nothing.
`handleFirst` matters for schedulers: it reifies "the rest of the computation" into an ordinary value the caller can
queue, interleave, or drop. morphir-scala's `FluxEngine` uses it as a pull primitive over
`Emit`.[^morphir-flux-engine]

## The halting shapes

| Shape | How | Kyo exemplar |
| --- | --- | --- |
| Hard halt | ignore the continuation, return a value of the result type | `Abort.run`: `handle = [C] => (input, _) => input`, well-typed because `Error[E] <: Result[E, A]`[^kyo-abort] |
| Loop-level halt | `Loop.done(finalValue)` from a `handleLoop` body | `Stream` short-circuits; `ElmParse.run` on `Halt`[^morphir-elm-parse] |
| Soft halt (drain) | keep looping but substitute a trivial continuation | `Stream.take` continues with `Kyo.unit` once exhausted[^kyo-stream] |
| Prune (multi-shot) | call the continuation zero-to-n times over a collection | `Choice.run`; `Choice.drop` suspends an empty `Seq`, so pruning falls out of the input encoding[^kyo-choice] |

Two further facts make halting *selectively* typed:

- `Abort`'s error channel distinguishes `Failure[E]` (expected, participates in `E`) from `Panic` (a defect carrying
  a `Throwable`, outside `E`). `Abort.runPartial` discharges failures while re-raising panics.[^kyo-abort] This maps
  directly onto the pipeline requirement that domain failures stay typed values while interpreter defects use a
  separate channel ([Transformation pipelines](/transformation-pipelines.md)).
- A handler can discharge a union error type (`Abort[NodeSkipped | PipelineAborted]`) one member at a time:
  `Abort.run[E]` requires `ConcreteTag[E]` and a `Reducible` on the remainder, so one boundary can absorb
  `NodeSkipped` while `PipelineAborted` passes through to an outer boundary. Internally this is an `accept`
  predicate on the private `handleCatching`; publicly it is the `ConcreteTag` + `Reducible` pattern.[^kyo-abort]

## Progress and state

`Emit[V]` is the stock shape for structured progress events: suspend a value, answer `Unit`. Its six runners
(`run` collecting a `Chunk`, `runFold`, `runDiscard`, `runForeach`, `runWhile`, `runFirst`) mean the *caller*
picks the sink (live logging, a replayable trace, or an aggregate) without the emitting code
changing.[^kyo-emit]

For interpreter-private state, prefer the `State` parameter of `handleLoop` over a `Var`: state inside the handler
is invisible to user code, and user code cannot corrupt it. Reserve `Var` for state the interpreted computation is
*meant* to read and write.

## Pitfalls

- Handler nesting order is a semantic decision. `Var.runTuple(0)` outside `Abort.run` preserves state across an
  abort; the reverse loses it.[^kyo-readme] The same mechanism extends to `Emit` versus `Abort`: events emitted
  before a failure survive only if the emit handler is outside the abort handler, which is the nesting
  morphir-scala's `QueryLogic` runner uses.[^morphir-query-logic] An interpreter that must report partial progress
  across a halt therefore sits **outside** `Abort.run`. When no nesting expresses the ordering needed, the
  2–4-arity `handle` interleaves the effects in one loop.

  ```mermaid
  flowchart LR
    subgraph keeps["Emit.run outside: events survive a halt"]
      EK["Emit.run"] -->|handles| AK["Abort.run"] -->|handles| CK["computation"]
    end
    subgraph loses["Abort.run outside: events die with the halt"]
      AL["Abort.run"] -->|handles| EL["Emit.run"] -->|handles| CL["computation"]
    end
  ```

  <a id="figure-1" name="figure-1"></a>Figure 1: the outer handler survives the inner one's short-circuit; put the handler whose output must
  outlive a halt outside the halting handler.
- Short-circuiting effects must not provide `Isolate` instances. Kyo's own documentation says automatic isolation
  derivation over an effect that can halt produces order-dependent results. Provide isolation for the `Var`/`Emit`
  parts of a row only.[^kyo-isolate]
- Continuations are multi-shot. A handler may invoke the continuation any number of times; if the interpreted code
  performs `Sync`, each invocation re-runs those side effects. Keep an executor handler single-shot unless
  re-execution (retry, speculation) is the point.[^kyo-arrow-effect]
- `Tag` erasure bites union-parameterised effects. Kyo mints `Tag[Abort[E]]` for arbitrary `E` by casting
  `Tag[Abort[Any]]`, and recovers precision at runtime via `ConcreteTag`. A too-wide tag swallows occurrences meant
  for an outer handler, because handler dispatch is subtype-checking on tags.[^kyo-abort]
- Pending computations are not `Tag`-derivable. The `Tag` macro fails on the `<` type constructor, so box a value
  holding `A < S` in an ordinary case class before anything needs its `Tag`. `FluxEngine.Boxed` exists for exactly
  this reason; a pipeline node type holding stage computations hits the same wall.[^morphir-flux-engine]
- Stack safety is periodic re-suspension, not magic. The kernel suspends every `maxStackDepth` frames and the
  `handleLoop` loops re-suspend safely, but a handler body's own non-Kyo recursion is unprotected; use `Loop.*` for
  explicit iteration. Resuming a computation on a different thread than built it forces re-entry through the
  suspension machinery, which matters if a planner thread builds node computations another thread
  runs.[^kyo-kernel-readme]
- Kyo's house conventions for handlers: one canonical `runWith(...)(f)` from which the public `run`/`runTuple`
  variants derive; `inline` on suspend sites and kernel entry points, while public runners such as `Abort.run` are
  deliberately not inline; effect types as `trait`, everything else `abstract class` or
  `final class`.[^kyo-contributing]

## In-repo contrast: one effect versus a stacked row

morphir-scala holds both shapes today. `ElmParse` is a single domain `ArrowEffect` (options, report, halt) whose
whole implicit bill is `(using Frame)`. Its test suite proves the design by writing a second interpreter that
differs only in halt policy.[^morphir-elm-parse] `QueryLogic` is the alternative: a stacked
`Var[State] & Emit[Log] & Abort[Err]` row whose runner nests three stock handlers and whose implicit bill is seven
`using` parameters: `Tag`s for the `Var` and `Emit` members and their element types, plus `ConcreteTag[Err]` and
`Frame`.[^morphir-query-logic] Both work. The
single-effect shape concentrates policy in one handler and keeps stage signatures to one capability name, at the
cost of writing the handler yourself. Which trade a pipeline should take is a design question for the design
notes, not this reference.

## What this does not settle

Handler mechanics do not decide graph semantics: node identity, readiness, join, skip, cancellation, and
deterministic ordering are contracts the executor must define before any of the machinery above interprets them
([Transformation pipelines](/transformation-pipelines.md),
[Guidance for a Morphir toolchain](/morphir-toolchain-guidance.md)). The open design questions for the buildkit
executor (outcome type, halt mechanism, skip propagation, stop-or-continue policy) live in the morphir-scala
[pipeline and workspace boundaries design note](../morphir/morphir-scala/design/pipeline-workspace-boundaries.md),
not here. This reference serves the buildkit task-graph capability; its narrative home is the
[buildkit task-graph design note](../morphir/morphir-scala/design/buildkit-task-graph.md).

[^kyo-rc6]: Kyo 1.0.0-RC6 source tree.
[^kyo-arrow-effect]: `kyo-kernel/shared/src/main/scala/kyo/kernel/ArrowEffect.scala` at 1.0.0-RC6.
[^kyo-abort]: `kyo-prelude/shared/src/main/scala/kyo/Abort.scala` at 1.0.0-RC6.
[^kyo-emit]: `kyo-prelude/shared/src/main/scala/kyo/Emit.scala` at 1.0.0-RC6.
[^kyo-var]: `kyo-prelude/shared/src/main/scala/kyo/Var.scala` at 1.0.0-RC6.
[^kyo-llm]: `kyo-ai/shared/src/main/scala/kyo/LLM.scala` at 1.0.0-RC6.
[^kyo-stream]: `kyo-prelude/shared/src/main/scala/kyo/Stream.scala` at 1.0.0-RC6.
[^kyo-choice]: `kyo-prelude/shared/src/main/scala/kyo/Choice.scala` at 1.0.0-RC6.
[^kyo-isolate]: `kyo-kernel/shared/src/main/scala/kyo/kernel/Isolate.scala` scaladoc at 1.0.0-RC6.
[^kyo-readme]: Kyo `README.md`, the worked `Var` and `Abort` ordering example.
[^kyo-kernel-readme]: `kyo-kernel/README.md`, stack-depth and cross-thread notes.
[^kyo-contributing]: Kyo `CONTRIBUTING.md`, "Effect Implementation Reference".
[^morphir-elm-parse]: `morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala` and `ParseEffectSpec.scala`.
[^morphir-flux-engine]: `morphir/contrib/knowledge/src/morphir/kb/logic/core/FluxEngine.scala`.
[^morphir-query-logic]: `morphir/langkit/trees/src/morphir/langkit/trees/query/QueryLogic.scala`.
