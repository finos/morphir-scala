---
type: Reference
title: Scala 3 and Kyo implementation notes
description: Record versioned Scala 3 and Kyo implementation techniques that materially support typed language-tooling designs.
tags: [scala-3, kyo, implementation]
status: draft
stale_after: 2026-10-29
implementation_baselines:
  scala: 3.8.4
  kyo:
    version: 1.0.0-RC5
    ref: 55d919dc0269a28fd936bc8ebe7a8cd07463ac30
sources:
  - id: project-dependencies
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/mill-build/src/millbuild/deps.scala
    title: morphir-scala dependency versions
  - id: scala-reference
    resource: https://docs.scala-lang.org/scala3/reference/
    title: Scala 3.8.4 Reference
  - id: kyo-rc5
    resource: https://github.com/getkyo/kyo/tree/55d919dc0269a28fd936bc8ebe7a8cd07463ac30
    title: Kyo 1.0.0-RC5
  - id: kyo-pending
    resource: https://github.com/getkyo/kyo/blob/55d919dc0269a28fd936bc8ebe7a8cd07463ac30/kyo-kernel/shared/src/main/scala/kyo/kernel/Pending.scala
    title: Kyo pending type at 1.0.0-RC5
  - id: morphir-stage
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/morphir/langkit/elm/compiler/api/src/morphir/langkit/elm/compiler/Stage.scala
    title: morphir-scala typed Stage
  - id: morphir-kyo-visitor
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/morphir/langkit/trees/src/morphir/langkit/trees/query/KyoQueryVisitor.scala
    title: morphir-scala KyoQueryVisitor
  - id: morphir-elm-parse
    resource: https://github.com/finos/morphir-scala/blob/d2abf88838da641fb7944c8d1569c9068eebdf4c/morphir/langkit/elm/core/src/morphir/langkit/elm/ElmParse.scala
    title: morphir-scala ElmParse effect
---

# Scala 3 and Kyo implementation notes

Scala 3 provides direct language support for closed tree models, contextual structural capabilities, lightweight
domain types, and capability composition.[^scala-reference] At the pinned Kyo baseline, those types can also describe
effectful traversal and pipeline stages whose handlers remain separate from their definitions.

This is an implementation companion to the bundle's general concepts. It is intentionally not required reading for
their architectural arguments.

## Baseline and freshness

| Component | Baseline used here | Source of truth |
| --- | --- | --- |
| Scala | `3.8.4` | `ScalaVersions.scala3` in `mill-build/src/millbuild/deps.scala`[^project-dependencies] |
| Kyo | `1.0.0-RC5` | `Versions.kyo-case-app` in the same file[^project-dependencies] |
| Kyo source | commit `55d919dc0269a28fd936bc8ebe7a8cd07463ac30` | annotated tag `v1.0.0-RC5`[^kyo-rc5] |

The Kyo claims below apply to that commit. If the project Kyo version changes, treat them as provisional immediately,
even before `stale_after`. Recheck signatures and semantics against the new tag, update the pinned reference, then
move the baseline and review date. The date is a backstop for upstream churn, not a substitute for version matching.

## Scala 3 features with concrete value

### Enums and exhaustive matching

Scala 3 enums can express a closed algebraic data type. Pattern matching then keeps node dispatch close to the model:

```scala
enum Expr:
  case Ref(name: String)
  case IntLiteral(value: Int)
  case Add(left: Expr, right: Expr)

def nodeCount(expr: Expr): Int =
  expr match
    case Expr.Ref(_)          => 1
    case Expr.IntLiteral(_)   => 1
    case Expr.Add(left, right) => 1 + nodeCount(left) + nodeCount(right)
```

**Value:** adding a new enum case makes exhaustive matches review points. This is useful for typed AST, IR, and
visitor dispatch code where silently ignoring a new node kind would be incorrect. Exhaustivity does not apply to an
open plugin registry or a string-valued interchange node.

### Givens and `using` for structural capabilities

Contextual instances let a model participate in generic algorithms without inheriting from a framework node:

```scala
trait TreeView[T]:
  def children(node: T): IndexedSeq[T]

def size[T](root: T)(using view: TreeView[T]): Int =
  1 + view.children(root).map(size(_)).sum

given TreeView[Expr] with
  def children(node: Expr): IndexedSeq[Expr] =
    node match
      case Expr.Add(left, right) => IndexedSeq(left, right)
      case _                     => IndexedSeq.empty
```

**Value:** CST, AST, and IR types retain their own inheritance and invariants while generic queries request only the
capability they need. Separate givens can provide structural and position views independently. This is the mechanism
used by morphir-scala's `QueryableTree`.

### Extension methods

An extension can place generic operations next to the value at the call site:

```scala
extension [T](root: T)(using view: TreeView[T])
  def treeSize: Int = size(root)
```

**Value:** callers get discoverable syntax without modifying third-party tree classes or creating wrapper nodes. The
implementation remains a normal function selected through a given.

### Opaque types for identities and coordinate units

```scala
opaque type NodeId = String
opaque type ByteOffset = Int
opaque type Utf16Offset = Int
```

**Value:** values with identical runtime representations cannot be mixed accidentally outside their defining scope.
That matters for node identity and source positions, where confusing byte, code-point, and UTF-16 offsets can produce
valid-looking but incorrect locations.

### Variance for stage boundaries

Morphir-scala declares `Stage[-I, +O, S]`: input is contravariant and output covariant.[^morphir-stage]

**Value:** a stage able to accept a broader input can be used where a narrower input is expected, while a stage
producing a narrower output can satisfy a broader output requirement. Effect type `S` remains explicit rather than
being hidden in inheritance.

### Intersection types for capability accumulation

Scala 3 intersection types provide the syntax used by Kyo effect rows and the current stage composition:

```scala
trait Stage[-Input, +Output, Effect]:
  def andThen[Next, NextEffect](
      next: Stage[Output, Next, NextEffect]
  ): Stage[Input, Next, Effect & NextEffect]
```

**Value:** composition retains both stages' required capabilities in the result type. The intersection records
membership, not handler order or execution order.

## Kyo features at `1.0.0-RC5`

### The pending type keeps requirements in signatures

At the pinned tag, Kyo defines `A < S` as an opaque type representing a computation that produces `A` while effects
`S` remain to be handled. `map` and `flatMap` accumulate rows using `S & S2`.[^kyo-pending]

```scala
import kyo.*

def inspect[S](tree: Expr)(
    onNode: Expr => Unit < S
)(using Frame): Unit < S =
  onNode(tree).flatMap(_ =>
    tree match
      case Expr.Add(left, right) =>
        inspect(left)(onNode).flatMap(_ => inspect(right)(onNode))
      case _ => ()
  )
```

**Value:** the tree stays pure while a caller chooses whether inspection emits facts, collects state, aborts, logs,
or performs another handled effect. Morphir-scala's `KyoQueryVisitor` uses this shape for pre-order visits and
folds.[^morphir-kyo-visitor]

### Handlers separate policy from stage logic

The effect row says which operations may occur; handlers decide how those operations are interpreted. At this
baseline, Kyo's effects provide their own `run` operations, and handler order can affect the resulting value shape
even though the intersection type itself is unordered.[^kyo-pending]

**Value:** a parser can report a diagnostic as an operation while a CLI, test driver, or editor interpreter decides
whether to collect it, stop, or continue with a placeholder. This separation is useful only when consumers genuinely
need different policies; a pure function returning a value is simpler when they do not.

### A custom effect can name a domain protocol

Morphir-scala's `ElmParse` is an `ArrowEffect` with three domain operations: obtain parse options, report a
diagnostic, and halt. Its shipped handler collects reports and controls whether a tree survives.[^morphir-elm-parse]

**Value:** stage signatures name one domain capability instead of threading options, diagnostic accumulators, and
early-return plumbing independently. The protocol remains Elm-specific; it does not establish that every buildkit
operation needs a new custom effect.

### Effectful traversal composes without effectful nodes

`KyoQueryVisitor` accepts a callback returning `Unit < S` or `A < S` and recursively threads the same row while the
query AST remains an ordinary immutable model.[^morphir-kyo-visitor]

**Value:** observation policy is attached to the traversal operation rather than stored inside AST nodes. Pure
visitors remain available for callers that do not need effects.

### Concurrency does not replace graph semantics

Kyo supplies asynchronous and concurrent capabilities at this baseline, but those APIs do not decide buildkit's node
identity, readiness, join, skip, cancellation, or deterministic ordering contracts.

**Value:** Kyo can implement a parallel executor after those graph semantics are defined. Its availability is not
evidence that graph construction should expose scheduler primitives or that the first executor must be parallel.

## Sparse callout map

| General concept | Scala 3 value | Kyo value |
| --- | --- | --- |
| [Syntax trees and IRs](/syntax-trees-and-intermediate-representations.md) | Enums, case classes, opaque coordinate types | Usually none in the node model |
| [Traversal and rewriting](/tree-traversal-visitors-cursors-and-rewriting.md) | Exhaustive matching, immutable cursor values | Effect-row-polymorphic callbacks |
| [Structural interoperability](/structural-tree-interoperability.md) | Givens, `using`, extensions | Usually none for pure projection |
| [Transformation pipelines](/transformation-pipelines.md) | Variance and intersection types | Pending computations, handlers, domain effects |
| [Morphir guidance](/morphir-toolchain-guidance.md) | Typed boundaries and immutable definitions | Interpreter implementation and eventual concurrency |

The absence of a Kyo value in a row is deliberate. Pure data and pure transformations should remain independent of an
effect library when Kyo adds no required capability.

[^project-dependencies]: morphir-scala dependency versions at commit `d2abf888`.
[^scala-reference]: Scala 3.8.4 Reference.
[^kyo-rc5]: Kyo `v1.0.0-RC5` at commit `55d919dc`.
[^kyo-pending]: Kyo pending type at `1.0.0-RC5`.
[^morphir-stage]: morphir-scala typed Stage.
[^morphir-kyo-visitor]: morphir-scala KyoQueryVisitor.
[^morphir-elm-parse]: morphir-scala ElmParse effect.
