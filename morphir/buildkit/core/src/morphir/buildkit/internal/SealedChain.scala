package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/**
 * One element of a sealed (validated) pipeline plan, mirroring [[DefElem]]: a stage paired structurally with its
 * assigned [[morphir.buildkit.NodeId]], a fork whose two sides each hold a fully sealed sub-chain, or a fan-out whose
 * child chain is fully sealed on its own, independent id namespace.
 */
private[buildkit] enum SealedElem[-I, +O, S]:
  // `Nothing` bridges the `Stage[I, O, E, S]` arity change (bead-tracked follow-up: threading `E` through
  // `SealedElem` is a later task); every stage wrapped here is still infallible or carries its abort inside the
  // untyped `S` row.
  case StageNode(id: NodeId, stage: Stage[I, O, Nothing, S])
  case ParNode[I2, O1, O2, Z, S1, S2](
      left: SealedChain[I2, O1, S1],
      right: SealedChain[I2, O2, S2],
      zip: (O1, O2) => Z
  ) extends SealedElem[I2, Z, S1 & S2]

  /**
   * A fan-out node: its own id, structurally paired with its child chain, already fully sealed on that child's own,
   * independent id namespace. At execution ([[morphir.buildkit.SealedPipeline]]) this node's id brackets the whole
   * per-element loop with its own `Entered`/`Exited` events — the child chain runs once per element of the incoming
   * `Chunk[A]`, each run's own events qualified with that element's index, and the bracket still fires with zero
   * elements (`Entered` immediately followed by `Exited`, no child events between).
   */
  case FanOutNode[A, B, S2](
      id: NodeId,
      each: SealedChain[A, B, S2]
  ) extends SealedElem[Chunk[A], Chunk[B], S2]

  /**
   * A branch node: its own id, a plain predicate on the incoming value, and both arms fully sealed. Unlike
   * `FanOutNode`'s child, both arms share this node's own flattened id namespace (see
   * [[morphir.buildkit.internal.DefElem.BranchElem]]), since only one of them ever executes. At execution the node's
   * own id brackets the whole decision with `Entered`/`Exited`: the taken arm runs normally, and every static node
   * reachable through the untaken arm — found via that arm's own [[nodeIds]], so a nested fan-out contributes only its
   * own id, unexpanded — emits `Skipped`.
   */
  case BranchNode[I2, O2, S1, S2](
      id: NodeId,
      pred: I2 => Boolean,
      ifTrue: SealedChain[I2, O2, S1],
      ifFalse: SealedChain[I2, O2, S2]
  ) extends SealedElem[I2, O2, S1 & S2]

  /**
   * Node ids, in definition order: one per leaf stage, recursing through both sides of a `ParNode`. A `FanOutNode`
   * contributes only its own id — its child chain's ids live in a separate, nested namespace. A `BranchNode`
   * contributes its own id followed by both arms' own ids, flattened — the same namespace its arms already share at
   * seal time.
   */
  def nodeIds: Chunk[NodeId] =
    this match
      case StageNode(id, _)                   => Chunk(id)
      case ParNode(left, right, _)            => left.nodeIds ++ right.nodeIds
      case FanOutNode(id, _)                  => Chunk(id)
      case BranchNode(id, _, ifTrue, ifFalse) => Chunk(id) ++ ifTrue.nodeIds ++ ifFalse.nodeIds

  /** Render this element: a stage renders as its own description; a fork renders as `par(left, right)`. */
  def describe: String =
    this match
      case StageNode(_, stage)               => stage.describe
      case ParNode(left, right, _)           => s"par(${left.describe}, ${right.describe})"
      case FanOutNode(_, each)               => s"fanOut(${each.describe})"
      case BranchNode(_, _, ifTrue, ifFalse) => s"branch(${ifTrue.describe}, ${ifFalse.describe})"
end SealedElem

/**
 * A typed non-empty cons-list of sealed elements, mirroring [[NodeChain]]'s GADT shape but pairing each stage with its
 * assigned id structurally, rather than through a parallel array indexed by position. Built once, by
 * [[Sealing.sealChain]], from a [[NodeChain]] and its assigned ids; the executor walks it with no index arithmetic, and
 * an id can never desynchronize from the node it names.
 */
private[buildkit] enum SealedChain[-I, +O, S]:
  case Single[I2, O2, S2](elem: SealedElem[I2, O2, S2]) extends SealedChain[I2, O2, S2]
  case Append[I2, M, O2, S1, S2](
      init: SealedChain[I2, M, S1],
      last: SealedElem[M, O2, S2]
  ) extends SealedChain[I2, O2, S1 & S2]

  /** Node ids, in definition order. */
  def nodeIds: Chunk[NodeId] =
    this match
      case Single(elem)       => elem.nodeIds
      case Append(init, last) => init.nodeIds ++ last.nodeIds

  /**
   * Render the chain: elements joined with `andThen`, forks rendered as `par(left, right)`. Shared by
   * [[morphir.buildkit.SealedPipeline#describe]] so it cannot drift from how nodes are actually walked at execution.
   */
  def describe: String =
    this match
      case Single(elem)       => elem.describe
      case Append(init, last) => s"${init.describe} andThen ${last.describe}"
end SealedChain
