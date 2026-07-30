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

## Attribution implementation value

The recommended attribution prototype keeps these Scala and Kyo techniques outside Morphir IR nodes. They implement
the typed index and production boundary described in
[typed attribution guidance for morphir-scala](/typed-attribution-guidance-for-morphir-scala.md); they do not define
node identity, graph semantics, merge policy, or Morphir's intrinsic types.

### Scala 3: small types at the caller boundary

The following illustrative block is deliberately smaller than a storage implementation. It shows where Scala 3
adds concrete value: opaque identifier domains prevent accidental mixing; an enum closes preservation outcomes;
contravariant key scopes allow a broader key to serve a narrower node; separate key classes encode cardinality;
givens associate codecs and RDF vocabulary terms without adding fields to nodes; and extensions make lookup
discoverable.[^scala-reference]

```scala
object AttributionImplementation:
  opaque type SnapshotId = String
  object SnapshotId:
    def apply(value: String): SnapshotId =
      require(value.nonEmpty, "snapshot id must be non-empty")
      value

  opaque type LocalNodeId = Long
  object LocalNodeId:
    def apply(value: Long): LocalNodeId =
      require(value >= 0, "local node id must be non-negative")
      value

  opaque type LayerId = String
  object LayerId:
    def apply(value: String): LayerId =
      require(value.nonEmpty, "layer id must be non-empty")
      value

  opaque type ProducerId = String
  object ProducerId:
    def apply(value: String): ProducerId =
      require(value.nonEmpty, "producer id must be non-empty")
      value

  sealed trait NodeKind
  sealed trait Expression extends NodeKind
  sealed trait Declaration extends NodeKind

  enum RuntimeNodeKind:
    case Expression, Declaration

  final case class NodeRef[+S <: NodeKind](
      snapshot: SnapshotId,
      local: LocalNodeId
  )

  final case class AttributionContext(
      snapshot: SnapshotId,
      layer: LayerId,
      producer: ProducerId
  )

  enum PreservationOutcome:
    case Preserve, Recompute, Invalidate, Remap

  final case class KeyId(
      namespace: String,
      name: String,
      version: Int
  ):
    require(namespace.nonEmpty, "key namespace must be non-empty")
    require(name.nonEmpty, "key name must be non-empty")
    require(version > 0, "key version must be positive")

  trait ValueSchema[A]:
    def schemaId: String
    def encode(value: A): Either[String, String]
    def decode(value: String): Either[String, A]

  trait Applicability[-S <: NodeKind]:
    def applicabilityId: String
    def accepts(actual: RuntimeNodeKind): Boolean

  trait OptionalKey[-S <: NodeKind, A]:
    def id: KeyId
    def schema: ValueSchema[A]
    def applicability: Applicability[S]

  trait ManyKey[-S <: NodeKind, A]:
    def id: KeyId
    def schema: ValueSchema[A]
    def applicability: Applicability[S]

  final case class MorphirType(name: String)
  final case class Tag(value: String)

  object ExpressionApplicability extends Applicability[Expression]:
    val applicabilityId = "morphir.node-kind/expression/v1"
    def accepts(actual: RuntimeNodeKind): Boolean =
      actual == RuntimeNodeKind.Expression

  object DeclarationApplicability extends Applicability[Declaration]:
    val applicabilityId = "morphir.node-kind/declaration/v1"
    def accepts(actual: RuntimeNodeKind): Boolean =
      actual == RuntimeNodeKind.Declaration

  object InferredType extends OptionalKey[Expression, MorphirType]:
    val id =
      KeyId("org.finos.morphir.analysis", "inferred-type", 1)
    val schema = summon[ValueSchema[MorphirType]]
    val applicability = ExpressionApplicability

  object Tags extends ManyKey[Declaration, Tag]:
    val id = KeyId("org.finos.morphir.user", "tags", 1)
    val schema = summon[ValueSchema[Tag]]
    val applicability = DeclarationApplicability

  given ValueSchema[MorphirType] with
    val schemaId = "morphir.type-name/v1"
    def encode(value: MorphirType): Either[String, String] =
      if value.name.nonEmpty then Right(value.name)
      else Left("type name must be non-empty")
    def decode(value: String): Either[String, MorphirType] =
      if value.nonEmpty then Right(MorphirType(value))
      else Left("type name must be non-empty")

  given ValueSchema[Tag] with
    val schemaId = "morphir.user-tag/v1"
    def encode(value: Tag): Either[String, String] =
      if value.value.nonEmpty then Right(value.value)
      else Left("tag must be non-empty")
    def decode(value: String): Either[String, Tag] =
      if value.nonEmpty then Right(Tag(value))
      else Left("tag must be non-empty")

  trait Vocabulary[K]:
    def predicateIri(key: K): String

  given Vocabulary[InferredType.type] with
    def predicateIri(key: InferredType.type): String =
      "https://example.org/morphir/vocab#inferredType"

  given Vocabulary[Tags.type] with
    def predicateIri(key: Tags.type): String =
      "https://example.org/morphir/vocab#tag"

  enum LookupError:
    case SnapshotMismatch(
        expected: SnapshotId,
        actual: SnapshotId
    )
    case UnknownNode(snapshot: SnapshotId, local: LocalNodeId)
    case InapplicableKey(id: KeyId, actual: RuntimeNodeKind)
    case UnknownKey(id: KeyId)
    case InvalidStoredValue(id: KeyId, details: String)

  enum RegistrationError:
    case KeyCollision(id: KeyId)
    case SchemaCollision(
        id: KeyId,
        registered: String,
        proposed: String
    )
    case ApplicabilityCollision(
        id: KeyId,
        registered: String,
        proposed: String
    )

  trait KeyRegistry:
    def register[S <: NodeKind, A](
        key: OptionalKey[S, A]
    ): Either[RegistrationError, KeyRegistry]

    def register[S <: NodeKind, A](
        key: ManyKey[S, A]
    ): Either[RegistrationError, KeyRegistry]

  trait SnapshotMembership:
    def snapshot: SnapshotId
    def kindOf[S <: NodeKind](
        node: NodeRef[S]
    ): Either[LookupError, RuntimeNodeKind]

  trait AttributionView:
    def context: AttributionContext
    def membership: SnapshotMembership

    def get[S <: NodeKind, A](
        node: NodeRef[S],
        key: OptionalKey[S, A]
    ): Either[LookupError, Option[A]]

    def values[S <: NodeKind, A](
        node: NodeRef[S],
        key: ManyKey[S, A]
    ): Either[LookupError, Vector[A]]

  extension (view: AttributionView)
    def inferredType(
        node: NodeRef[Expression]
    ): Either[LookupError, Option[MorphirType]] =
      view.get(node, InferredType)

    def tags(
        node: NodeRef[Declaration]
    ): Either[LookupError, Vector[Tag]] =
      view.values(node, Tags)
```

The schema givens do not make external bytes trustworthy; import code still runs `decode`. The open key traits
support downstream extensions, but registration must reject a reused `KeyId` unless its exact schema, cardinality,
and `applicabilityId` agree. `SchemaCollision` and `ApplicabilityCollision` make those dynamic failures explicit.
An erased implementation cannot recover `A` from `ClassTag`; it must create and decode packed entries through the
exact registered `ValueSchema[A]` or retain a closed-registry equality proof. Compile-time safety covers the public
caller API, while external data receives runtime validation. Likewise, the vocabulary givens make projection
explicit but do not make the illustrative IRIs a Morphir standard.

An `AttributionView` is scoped to its `AttributionContext`: one snapshot, layer, and producer. Its implementation
includes all three identifiers in the logical lookup address and returns `SnapshotMismatch` for a reference from
another snapshot. Before accessing storage, `get` and `values` use `SnapshotMembership.kindOf`: a missing
snapshot-local reference returns `UnknownNode`, while a runtime kind rejected by the registered applicability
witness returns `InapplicableKey`. These checks protect the dynamic import and stale-reference boundary even though
ordinary Scala call sites remain cast-free. A resolved multi-layer view is a separate operation with a named
deterministic merge policy, not an implicit behavior of these lookup extensions.

Match types are useful only when they remove work at a public call site—for example, deriving a result type from a
single cardinality descriptor. With separate `OptionalKey` and `ManyKey`, overloads already yield `Option[A]` and
`Vector[A]` directly, so an additional match-type algebra would be clever machinery without caller value. Prefer
ordinary types until a measured API burden justifies more.

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

### Attribution fact production stays effect-polymorphic

Morphir-scala's pinned `Stage` returns a Kyo pending computation, and `KyoQueryVisitor` accepts effect-polymorphic
callbacks for visits and folds.[^morphir-stage][^morphir-kyo-visitor] The following proposed use applies those
demonstrated shapes to attribution. It intentionally uses only `A < S`, `Frame`, and an effect-polymorphic callback
from the pinned Kyo API rather than speculating about an `Emit` or environment API:

```scala
object AttributionProduction:
  import kyo.*
  import AttributionImplementation.*

  final case class AttributionFact(
      node: NodeRef[? <: NodeKind],
      keyId: KeyId,
      encodedValue: String,
      context: AttributionContext
  )

  def recordInferredType[S](
      node: NodeRef[Expression],
      inferred: MorphirType
  )(
      emit: AttributionFact => Unit < S
  )(using
      context: AttributionContext,
      frame: Frame
  ): Either[String, Unit] < S =
    InferredType.schema.encode(inferred) match
      case Left(error) => Left(error)
      case Right(encoded) =>
        emit(
          AttributionFact(
            node,
            InferredType.id,
            encoded,
            context
          )
        ).map(Right(_))
```

The `AttributionContext` is scoped by an ordinary Scala `using` value at the stage or run boundary. That is enough
to keep snapshot, producer, and layer out of every method argument and out of IR nodes; it makes no claim about a
Kyo environment effect at RC5. A caller can supply callbacks whose handlers collect facts, validate and index them,
serialize selected facts, or discard them. A future direct `Emit`-style implementation is an interchangeable
handler choice only after its exact pinned API has been verified.

This callback shape is a **proposed application** of demonstrated pinned capabilities, not an existing
morphir-scala attribution implementation. Kyo's pending type and `flatMap`/`map` composition are present at the
pinned commit,[^kyo-pending] while the attribution fact, context, key registry, and handler policies above are
design candidates.

Collection must preserve a specified fact order or canonicalize before indexing and serialization. Layer merge,
single-value conflict, duplicate handling, and invalidation semantics must be deterministic before concurrency is
introduced. Kyo may execute production and handling after those rules exist; it must not define node identity, graph
semantics, merge policy, preservation outcomes, or Morphir IR types.

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
| [Typed attribution guidance](/typed-attribution-guidance-for-morphir-scala.md) | Opaque IDs, typed cardinality, codecs, vocabularies, extensions | Effect-polymorphic fact-production callbacks and handlers |
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
