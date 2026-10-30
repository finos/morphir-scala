---
type: Reference
title: Kyo effect handlers and typed halting
description: "Record verified Kyo 1.0.0-RC7 ArrowEffect handler variants, typed-halting shapes, and executor-relevant pitfalls for task-graph interpreter design."
tags: [kyo, effects, pipeline]
status: draft
verified:
  - by: claude/fable-5
    at: 2026-08-12T00:00:00Z
  - by: claude/opus-5.5
    at: 2026-10-29T00:00:00Z
sources:
  - id: kyo-rc7
    resource: https://github.com/getkyo/kyo/tree/1d54c02a3842a5792814a9d1abc7df2fb629eb3e
    title: Kyo 1.0.0-RC7
  - id: kyo-kernel-rewrite
    resource: https://github.com/getkyo/kyo/commit/645a6095a8c87708925c177d2994891bd6fb5aca
    title: "Kyo kernel rewrite (getkyo/kyo#1988), breaking-change notes"
  - id: kyo-arrow-effect
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/shared/src/main/scala/kyo/kernel/ArrowEffect.scala
    title: ArrowEffect at 1.0.0-RC7
  - id: kyo-region
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/shared/src/main/scala/kyo/kernel/Region.scala
    title: Region at 1.0.0-RC7
  - id: kyo-loop
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/shared/src/main/scala/kyo/kernel/Loop.scala
    title: Loop at 1.0.0-RC7
  - id: kyo-abort
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-prelude/shared/src/main/scala/kyo/Abort.scala
    title: Abort at 1.0.0-RC7
  - id: kyo-is-fatal
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-data/shared/src/main/scala/kyo/IsFatal.scala
    title: IsFatal at 1.0.0-RC7
  - id: kyo-contributing
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/CONTRIBUTING.md
    title: Kyo effect implementation reference
  - id: kyo-emit
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-prelude/shared/src/main/scala/kyo/Emit.scala
    title: Emit at 1.0.0-RC7
  - id: kyo-stream
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-prelude/shared/src/main/scala/kyo/Stream.scala
    title: Stream at 1.0.0-RC7
  - id: kyo-choice
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-prelude/shared/src/main/scala/kyo/Choice.scala
    title: Choice at 1.0.0-RC7
  - id: kyo-isolate
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/shared/src/main/scala/kyo/kernel/Isolate.scala
    title: Isolate at 1.0.0-RC7
  - id: kyo-var
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-prelude/shared/src/main/scala/kyo/Var.scala
    title: Var at 1.0.0-RC7
  - id: kyo-llm
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-ai/shared/src/main/scala/kyo/LLM.scala
    title: LLM effect at 1.0.0-RC7
  - id: kyo-readme
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/README.md
    title: Kyo README effect-ordering example
  - id: kyo-kernel-readme
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/README.md
    title: Kyo kernel README
  - id: kyo-kernel-contributing
    resource: https://github.com/getkyo/kyo/blob/1d54c02a3842a5792814a9d1abc7df2fb629eb3e/kyo-kernel/CONTRIBUTING.md
    title: Kyo kernel contributing guide, tunable flags
  - id: morphir-elm-parse
    resource: https://github.com/finos/morphir-scala/blob/925c2db18b3a0d9ed4d239e63d1b00b72fba8485/morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala
    title: morphir-scala ElmParse effect
  - id: morphir-flux-engine
    resource: https://github.com/finos/morphir-scala/blob/925c2db18b3a0d9ed4d239e63d1b00b72fba8485/morphir/knowledge/logic/src/morphir/knowledge/logic/core/FluxEngine.scala
    title: morphir-scala FluxEngine
  - id: morphir-query-logic
    resource: https://github.com/finos/morphir-scala/blob/925c2db18b3a0d9ed4d239e63d1b00b72fba8485/morphir/langkit/trees/src/morphir/langkit/trees/query/QueryLogic.scala
    title: morphir-scala QueryLogic
---

# Kyo effect handlers and typed halting

At the pinned Kyo baseline, an interpreter for a domain effect (including a task-graph executor) is one
`ArrowEffect` handler. Halting is not a special API: a handler halts by answering with a value of the handled
computation's final result type instead of resuming. A `handleCont` clause returns that value without applying the
continuation; a loop clause returns `Loop.done(result)`. The result type therefore fixes the set of halt outcomes
the interpreter can express. That is where "typed halting" comes from.[^kyo-arrow-effect]

This document is a companion to [Scala 3 and Kyo implementation notes](/scala-3-and-kyo-implementation-notes.md),
which pins the same `1.0.0-RC7` baseline. Each document carries its own `verified` entry recording who checked its
claims against that pin, and when. When the project's Kyo version moves again, both documents need their own
re-verification; neither vouches for the other.

## Baseline and freshness

| Component | Baseline used here | Source of truth |
| --- | --- | --- |
| Kyo | `1.0.0-RC7` | `Versions.kyo` in `mill-build/src/millbuild/deps.scala` |
| Kyo source | commit `1d54c02a3842a5792814a9d1abc7df2fb629eb3e` | annotated tag `v1.0.0-RC7`[^kyo-rc7] |

If the project's Kyo version moves, treat every claim here as provisional immediately: recheck the cited signatures
against the new tag, then move this pin.

RC7 replaced the kernel (getkyo/kyo#1988). It removed `ArrowEffect.handle`, `handleCatching`, `handlePartial`,
`Effect.catching`, `ContextEffect.Noninheritable` and the `Safepoint` parameters. The public handler surface is now
`handleCont`, `handleContRepeated`, `handleLoop` and `handleLoopState`.[^kyo-kernel-rewrite] Code written against
the RC6 handler table does not compile at RC7.

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
  match for exhaustiveness, and the answer needs no cast beyond the standard `asInstanceOf[C]` idiom. Kyo's
  own `LLM` effect[^kyo-llm] and morphir-scala's `ElmParse`[^morphir-elm-parse] both use this.
- An untagged union `Op[V]` disambiguated by runtime match, as `Var` does.[^kyo-var] The match is order-dependent
  and unchecked; the GADT is the safer default for new effects.

An operation that must never resume declares its answer type as `Nothing`. `Abort.error` is the template: the
suspend site returns `Nothing < Abort[E]` with an unreachable transform. This is legitimate because the only
handler for that operation never applies the continuation.[^kyo-abort] morphir-scala's `ElmParseOp.Halt
extends ElmParseOp[Nothing]` is the same idiom.[^morphir-elm-parse]

## Handler variants

All variants sit in `ArrowEffect`'s companion and handle one effect.[^kyo-arrow-effect] Every variant takes the
effect's `Tag` and the computation. The variants differ in what the clause (the polymorphic handler function)
receives.

| Variant | Clause receives | Fits |
| --- | --- | --- |
| `handleCont` | input and continuation; applied at most once | Per-occurrence interpretation that resumes once or drops the rest |
| `handleContRepeated` | input and continuation; may be applied many times | Multi-shot interpretation: backtracking, non-determinism (`Choice.run`) |
| `handleLoop` | input only; returns `Loop.Outcome` | Answer or stop, with no private state |
| `handleLoopState` | state and input; returns `Loop.Outcome2` | The interpreter workhorse: thread private state across occurrences, fold `(State, A)` into a report type in `done` |

Each variant has overloads that add a `done` arm (transform the final value) and a `recover` arm
(`Throwable => Maybe[B < S]`, offered a failure of the handled computation). Fused `handleContWith`,
`handleLoopWith` and `handleLoopStateWith` forms take the caller's next step as an extra argument.[^kyo-arrow-effect]

A `handleLoopState` clause returns `Loop.continue(nextState, answer)` to resume or `Loop.done(result)` to stop the
loop and discard the rest of the computation. `handleLoop` uses the same pair without the state. `Loop.Outcome` is
an opaque union (`O | Continue[A]`), so `done` allocates nothing.[^kyo-loop] The clause never holds the
continuation, so each occurrence is answered once or not at all.

No public variant hands out "the rest of the computation" as a value. RC6's `handleFirst` did; at RC7 it and
`handleFirstRepeated` are `private[kyo]`. Every public clause's continuation carries `Region.NoEscape`, a phantom
marker in its effect row. Storing it, returning it, or moving it to another fiber does not compile.[^kyo-region] A
scheduler that needs to queue or interleave remainders must model them as its own data. morphir-scala's `FluxEngine`
did this on the move to RC7: an `Emit` source pulled through `handleFirst` became a plain lazy step
type.[^morphir-flux-engine]

## The halting shapes

| Shape | How | Kyo exemplar |
| --- | --- | --- |
| Hard halt | a `handleCont` clause ignores the continuation and returns a value of the result type | `Abort.run`: `[C] => (input, _) => input`, well-typed because `Error[E] <: Result[E, A]`[^kyo-abort] |
| Loop-level halt | `Loop.done(result)` from a `handleLoop` or `handleLoopState` clause | `Stream.take` once `n` elements have passed[^kyo-stream]; `ElmParse.run` on `Halt`[^morphir-elm-parse] |
| Prune (multi-shot) | a `handleContRepeated` clause applies the continuation zero-to-n times over a collection | `Choice.run`; `Choice.drop` suspends an empty `Seq`, so pruning falls out of the input encoding[^kyo-choice] |

RC6 had a fourth shape, the soft halt (drain): keep looping but resume with a trivial continuation. `Stream.take`
continued with `Kyo.unit` once exhausted. At RC7 a loop clause no longer receives the continuation, so the shape has
no direct form, and `Stream.take` ends with `Loop.done(())` instead.[^kyo-stream]

Two further facts make halting *selectively* typed:

- `Abort`'s error channel distinguishes `Failure[E]` (expected, participates in `E`) from `Panic` (a defect carrying
  a `Throwable`, outside `E`). `Abort.runPartial` discharges failures while re-raising panics.[^kyo-abort] This maps
  onto the pipeline requirement that domain failures stay typed values while interpreter defects use a separate
  channel ([Transformation pipelines](/transformation-pipelines.md)). At RC7 a handler sees a throw through its
  `recover` arm. `Abort.run` uses that arm to turn a non-fatal throw into `Result.Panic`, and `Abort.catching` is
  built from a private `Catching` marker effect plus a `recover` arm. Fatal means `kyo.IsFatal`:
  `VirtualMachineError` or `ControlThrowable`.[^kyo-abort][^kyo-is-fatal]
- A handler can discharge a union error type (`Abort[NodeSkipped | PipelineAborted]`) one member at a time:
  `Abort.run[E]` requires `ConcreteTag[E]` and a `Reducible` on the remainder, so one boundary can absorb
  `NodeSkipped` while `PipelineAborted` passes through to an outer boundary. Internally `Abort.run` handles every
  `Abort` under one erased tag, and its `done` arm re-raises any error that `ConcreteTag[E]` does not accept.
  Publicly it is the `ConcreteTag` + `Reducible` pattern.[^kyo-abort]

## Progress and state

`Emit[V]` is the stock shape for structured progress events: suspend a value, answer `Unit`. Its five public
runners (`run` collecting a `Chunk`, `runFold`, `runDiscard`, `runForeach`, `runWhile`) mean the *caller* picks the
sink (live logging, a replayable trace, or an aggregate) without the emitting code changing. RC6's sixth runner,
`runFirst`, is `private[kyo]` at RC7.[^kyo-emit]

For interpreter-private state, prefer the state of `handleLoopState` over a `Var`: state inside the handler
is invisible to user code, and user code cannot corrupt it. Reserve `Var` for state the interpreted computation is
*meant* to read and write.

## Pitfalls

- Handler nesting order is a semantic decision. `Var.runTuple(0)` outside `Abort.run` preserves state across an
  abort; the reverse loses it.[^kyo-readme] The same mechanism extends to `Emit` versus `Abort`: events emitted
  before a failure survive only if the emit handler is outside the abort handler, which is the nesting
  morphir-scala's `QueryLogic` runner uses.[^morphir-query-logic] An interpreter that must report partial progress
  across a halt therefore sits **outside** `Abort.run`. RC6's 2–4-arity `ArrowEffect.handle`, which interleaved
  several effects in one loop, is gone at RC7, so nesting is the only public way to order handlers. The `.handle(...)`
  method on a computation is a left-to-right pipeline of nested handlers, not an interleaving
  handler.[^kyo-kernel-rewrite][^kyo-kernel-readme]

  ```mermaid
  flowchart LR
    subgraph keeps["Emit.run outside: events survive a halt"]
      EK["Emit.run"] -->|handles| AK["Abort.run"] -->|handles| CK["computation"]
    end
    subgraph loses["Abort.run outside: events die with the halt"]
      AL["Abort.run"] -->|handles| EL["Emit.run"] -->|handles| CL["computation"]
    end
  ```

  **Figure 1:** the outer handler survives the inner one's short-circuit; put the handler whose output must
  outlive a halt outside the halting handler.
- Short-circuiting effects must not provide `Isolate` instances. Kyo's own documentation says automatic isolation
  derivation over an effect that can halt produces order-dependent results. Provide isolation for the `Var`/`Emit`
  parts of a row only.[^kyo-isolate]
- Continuations are single-shot unless you ask for more. A `handleCont` clause applies its continuation at most
  once; a second application is refused at the first region it re-enters, as a use after release. A clause that
  resumes more than once uses `handleContRepeated`, as `Choice.run` does.[^kyo-arrow-effect][^kyo-choice] If the
  interpreted code performs `Sync`, each resumption re-runs those side effects. Keep an executor handler on
  `handleCont` or the loop family unless re-execution (retry, speculation) is the point.
- `Tag` erasure bites union-parameterised effects. Kyo mints `Tag[Abort[E]]` for arbitrary `E` by casting
  `Tag[Abort[Any]]`, and recovers precision at runtime via `ConcreteTag`. A too-wide tag swallows occurrences meant
  for an outer handler, because handler dispatch is subtype-checking on tags.[^kyo-abort]
- Pending computations get a full `Tag` at RC7. A compile probe derived `Tag[Unit < Emit[Chunk[Option[Int]]]]`, and
  `Tag[Emit[Chunk[Option[Unit < Emit[Chunk[Option[A]]]]]]]` under an abstract `A: Tag`, on RC5, RC6 and RC7. All
  three compiled. On RC5 and RC6 the tag dropped the arguments of `<`, so the tag for an `Int` source equalled the
  tag for a `String` source (`=:=` answered `true`), and an `Emit` of pending sources could not be told apart by
  element type. On RC7 the arguments are kept and the two tags differ. morphir-scala boxed pending values
  (`FluxEngine.Boxed`, added on RC5) on the stated grounds of a `Tag` macro `MatchError`; that error did not
  reproduce on RC5 or RC6 in the shape `FluxEngine` used. The box is gone with the move to a plain step
  type.[^morphir-flux-engine]
- Stack safety comes from periodic re-suspension. Fusion runs on the caller's stack under a per-thread budget (the
  safepoint, default `maxStackDepth` of 256).[^kyo-kernel-contributing] When the budget runs out, the evaluator
  takes the chain over, so a deep synchronous chain pays its depth in heap.[^kyo-kernel-rewrite][^kyo-kernel-readme]
  A handler body's own
  non-Kyo recursion is unprotected; use `Loop.*` for explicit iteration. RC6's safepoint also checked thread
  identity, so a computation resumed on another thread took the suspension path. That check is gone at RC7: a
  computation is an immutable value that one thread can build and another can run. A continuation still cannot
  cross, because `Region.NoEscape` refuses the `Isolate` a crossing needs.[^kyo-kernel-rewrite][^kyo-region]
- Kyo's house conventions for handlers: one canonical `runWith(...)(f)` from which the public `run`/`runTuple`
  variants derive; `inline` on suspend sites and kernel entry points, while public runners such as `Abort.run` are
  not inline; effect types as `trait`, everything else `abstract class` or `final class`.[^kyo-contributing]

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

[^kyo-rc7]: Kyo 1.0.0-RC7 source tree.
[^kyo-kernel-rewrite]: Kyo commit `645a6095a`, "New kernel" (getkyo/kyo#1988): the breaking-change, safepoint and fusion notes in its message.
[^kyo-arrow-effect]: `kyo-kernel/shared/src/main/scala/kyo/kernel/ArrowEffect.scala` at 1.0.0-RC7.
[^kyo-region]: `kyo-kernel/shared/src/main/scala/kyo/kernel/Region.scala` scaladoc at 1.0.0-RC7.
[^kyo-loop]: `kyo-kernel/shared/src/main/scala/kyo/kernel/Loop.scala` at 1.0.0-RC7.
[^kyo-abort]: `kyo-prelude/shared/src/main/scala/kyo/Abort.scala` at 1.0.0-RC7.
[^kyo-is-fatal]: `kyo-data/shared/src/main/scala/kyo/IsFatal.scala` at 1.0.0-RC7.
[^kyo-emit]: `kyo-prelude/shared/src/main/scala/kyo/Emit.scala` at 1.0.0-RC7.
[^kyo-var]: `kyo-prelude/shared/src/main/scala/kyo/Var.scala` at 1.0.0-RC7.
[^kyo-llm]: `kyo-ai/shared/src/main/scala/kyo/LLM.scala` at 1.0.0-RC7.
[^kyo-stream]: `kyo-prelude/shared/src/main/scala/kyo/Stream.scala` at 1.0.0-RC7; `take` at RC6 for the drain shape.
[^kyo-choice]: `kyo-prelude/shared/src/main/scala/kyo/Choice.scala` at 1.0.0-RC7.
[^kyo-isolate]: `kyo-kernel/shared/src/main/scala/kyo/kernel/Isolate.scala` scaladoc at 1.0.0-RC7.
[^kyo-readme]: Kyo `README.md` at 1.0.0-RC7, the worked `Var` and `Abort` ordering example.
[^kyo-kernel-readme]: `kyo-kernel/README.md` at 1.0.0-RC7: the `handle` pipeline and iteration sections.
[^kyo-kernel-contributing]: `kyo-kernel/CONTRIBUTING.md` at 1.0.0-RC7, the `Safepoint.period` row (default `maxStackDepth`, 256).
[^kyo-contributing]: Kyo `CONTRIBUTING.md` at 1.0.0-RC7, "Effect Implementation Reference" and "Inline Guidelines".
[^morphir-elm-parse]: `morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala` and `ParseEffectSpec.scala`.
[^morphir-flux-engine]: `morphir/knowledge/logic/src/morphir/knowledge/logic/core/FluxEngine.scala`, its scaladoc on the RC7 move.
[^morphir-query-logic]: `morphir/langkit/trees/src/morphir/langkit/trees/query/QueryLogic.scala`.
