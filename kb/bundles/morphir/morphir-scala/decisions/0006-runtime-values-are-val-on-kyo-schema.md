---
type: Decision Record
title: Runtime values are one serializable Val built on kyo-schema
description: "The new runtime has a single value type, datamodel.Val, whose structured case is kyo-schema's Structure.Value and whose closures serialize."
state: Accepted
decided: 2026-07-29
tags: [runtime, kyo, kyo-schema, datamodel, serialization]
status: stable
---

# 0006 — Runtime values are one serializable `Val` built on kyo-schema

The new runtime has exactly one value type, `org.finos.morphir.datamodel.Val`:

```scala
enum Val derives Schema:
  case Structured(value: Structure.Value)
  case Closure(params: Chunk[Name], body: Expr, env: Map[Name, Val])
  case Partial(fqn: FQName, arity: Int, applied: Chunk[Val])
```

The structured case is kyo-schema's `Structure.Value` rather than a bespoke ADT. Closures are a variant of the value
type, not an escape hatch outside it, and the whole thing round-trips through JSON.

## Why

The classic runtime has three incompatible value representations — `Concept`, `Data` and `RTValue` — plus the
conversion dance between them (`resultAndConceptToData` and friends). Every operation has to know which one it holds.
Collapsing them into one type removes that entire category of bug, and it is only possible because closures are
brought inside rather than left out.

**Closures must serialize, and that is the load-bearing constraint.** `RTValue` holds Scala functions and mutable
collections, so a paused program can never be written to bytes. That single fact is why snapshot, restore, resume,
stateful incremental evaluation and deterministic replay are all unreachable in the classic runtime — not a missing
feature in any of them, but a property of the value type underneath. `Val.Closure` holds IR plus an environment of
`Val`s, so a paused program is data. This was proven before the rest of the design was committed to: a closure
round-trips through JSON in `ValSpec`.

**`Structure.Value` rather than our own ADT** because it brings Json/Protobuf/Yaml/Ion/MsgPack codecs, plus `Focus`,
`Path`, `Compare`, `Modify`, `Changeset` and `Builder`, none of which we would otherwise write. Its shape is *not*
embedded per node, which matters for size.

The name was chosen against `Datum` and `MValue`. `Val` is the programming-language-implementation name for a value
domain that *contains closures*; `Datum` is the data-interchange name for one that cannot. Since closures are in, the
data-interchange name would be a lie.

## Consequences

`Structure.Value.Record` is purely structural, not nominal — a value encoded as a `Customer` decodes into an
unrelated `Widget` of the same shape. Type identity therefore has to come from elsewhere; see
[0007](/decisions/0007-runtime-reuses-the-code-model-type-language.md).

`Val.Closure` binds a `Chunk[Name]` while `Expr.Lambda` binds a `Pattern`, so a destructuring lambda such as
`\(x, y) -> x + y` has no representable closure today. Either the evaluator desugars it to a fresh name plus a
synthetic `PatternMatch`, or `Closure` widens to `Chunk[Pattern]`. This is unresolved and must be settled before any
evaluator commits to a wire format for `Val`, because changing `Closure`'s shape after `Val` ships is breaking.

Whether `Val.Partial` needs to distinguish a partially-applied native from a partially-applied closure is likewise
open, deferred to whenever application semantics are designed.
