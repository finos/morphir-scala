package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/** One node of a pipeline definition: an optional explicit id and the stage it runs. */
private[buildkit] final case class PipelineNode[-I, +O, S](explicitId: Maybe[String], stage: Stage[I, O, S])

/**
 * A typed non-empty cons-list of pipeline nodes, appended at the right — the same existential-middle technique as
 * `Stage.AndThen`. `Append`'s middle type `M` is carried as a case type parameter so walking the chain stays fully
 * typed with no casts.
 */
private[buildkit] enum NodeChain[-I, +O, S]:
  case Single[I2, O2, S2](node: PipelineNode[I2, O2, S2]) extends NodeChain[I2, O2, S2]
  case Append[I2, M, O2, S1, S2](
      init: NodeChain[I2, M, S1],
      last: PipelineNode[M, O2, S2]
  ) extends NodeChain[I2, O2, S1 & S2]

  /** Nodes in execution order, erased to what sealing needs: explicit id and stage metadata. */
  def summaries: Chunk[(Maybe[String], Maybe[StageMeta], String)] =
    this match
      case Single(node)       => Chunk((node.explicitId, node.stage.meta, node.stage.describe))
      case Append(init, last) => init.summaries.append((last.explicitId, last.stage.meta, last.stage.describe))

  def size: Int =
    this match
      case Single(_)       => 1
      case Append(init, _) => init.size + 1

  /**
   * Render the chain: stage descriptions joined with `andThen`. Shared by [[morphir.buildkit.PipelineDef#describe]] and
   * [[morphir.buildkit.SealedPipeline#describe]] so the two cannot drift — the expression can't live on the public
   * `Pipeline` trait without naming this internal type.
   */
  def describe: String =
    summaries.map((_, _, description) => description).mkString(" andThen ")
end NodeChain

private[buildkit] object Sealing:
  /** Lowercase; runs of non-alphanumerics become single `-`; trimmed. `Absent` when nothing survives. */
  def slugify(label: String): Maybe[String] =
    val slug = label.toLowerCase.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")
    if slug.isEmpty then Absent else Present(slug)
