package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/** A node paired structurally with its assigned [[morphir.buildkit.NodeId]], produced by sealing. */
private[buildkit] final case class SealedNode[-I, +O, S](id: NodeId, node: PipelineNode[I, O, S])

/**
 * A typed non-empty cons-list of sealed nodes, mirroring [[NodeChain]]'s GADT shape but pairing each node with its
 * assigned id structurally, rather than through a parallel array indexed by position. Built once, by [[Sealing.seal]],
 * from a [[NodeChain]] and its assigned ids; the executor walks it with no index arithmetic, and an id can never
 * desynchronize from the node it names.
 */
private[buildkit] enum SealedChain[-I, +O, S]:
  case Single[I2, O2, S2](node: SealedNode[I2, O2, S2]) extends SealedChain[I2, O2, S2]
  case Append[I2, M, O2, S1, S2](
      init: SealedChain[I2, M, S1],
      last: SealedNode[M, O2, S2]
  ) extends SealedChain[I2, O2, S1 & S2]

  /** Node ids, in definition order. */
  def nodeIds: Chunk[NodeId] =
    this match
      case Single(node)       => Chunk(node.id)
      case Append(init, last) => init.nodeIds.append(last.id)

  /**
   * Render the chain: stage descriptions joined with `andThen`. Shared by [[morphir.buildkit.SealedPipeline#describe]]
   * so it cannot drift from how nodes are actually walked at execution.
   */
  def describe: String =
    this match
      case Single(node)       => node.node.stage.describe
      case Append(init, last) => s"${init.describe} andThen ${last.node.stage.describe}"
end SealedChain
