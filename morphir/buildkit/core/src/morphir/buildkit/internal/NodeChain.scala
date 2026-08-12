package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/**
 * One element of a pipeline definition: either a single stage, or a fork into two peer chains that both receive the
 * same input and whose results are paired by `zip`. `par` is the first non-stage element; later tasks (fanOut, branch)
 * add cases here.
 *
 * `ParElem`'s type parameters follow the same existential-middle technique as [[Stage.AndThen]]: `I2` is the shared
 * input to both branches, `Z` is the zipped result exposed as this element's `O`, and `S1 & S2` is the combined effect
 * row. `zip` is captured monomorphically at `par` call time (from a `Zippable` instance), so this GADT never carries
 * the typeclass itself.
 */
private[buildkit] enum DefElem[-I, +O, S]:
  case StageElem(explicitId: Maybe[String], stage: Stage[I, O, S])
  case ParElem[I2, O1, O2, Z, S1, S2](
      left: NodeChain[I2, O1, S1],
      right: NodeChain[I2, O2, S2],
      zip: (O1, O2) => Z
  ) extends DefElem[I2, Z, S1 & S2]

  /** Number of leaf stages this element expands to, walked in definition order. A `ParElem` counts both sides. */
  def size: Int =
    this match
      case StageElem(_, _)         => 1
      case ParElem(left, right, _) => left.size + right.size

  /** Leaf summaries, in definition order: one triple per stage, recursing through both sides of a `ParElem`. */
  def summaries: Chunk[(Maybe[String], Maybe[StageMeta], String)] =
    this match
      case StageElem(explicitId, stage) => Chunk((explicitId, stage.meta, stage.describe))
      case ParElem(left, right, _)      => left.summaries ++ right.summaries

  /** Render this element: a stage renders as its own description; a fork renders as `par(left, right)`. */
  def describe: String =
    this match
      case StageElem(_, stage)     => stage.describe
      case ParElem(left, right, _) => s"par(${left.describe}, ${right.describe})"
end DefElem

/**
 * A typed non-empty cons-list of pipeline elements, appended at the right — the same existential-middle technique as
 * `Stage.AndThen`. `Append`'s middle type `M` is carried as a case type parameter so walking the chain stays fully
 * typed with no casts.
 */
private[buildkit] enum NodeChain[-I, +O, S]:
  case Single[I2, O2, S2](elem: DefElem[I2, O2, S2]) extends NodeChain[I2, O2, S2]
  case Append[I2, M, O2, S1, S2](
      init: NodeChain[I2, M, S1],
      last: DefElem[M, O2, S2]
  ) extends NodeChain[I2, O2, S1 & S2]

  /** Number of leaf stages in this chain, walked in definition order. */
  def size: Int =
    this match
      case Single(elem)       => elem.size
      case Append(init, last) => init.size + last.size

  /** Leaf summaries, erased to what sealing needs: explicit id and stage metadata, in definition order. */
  def summaries: Chunk[(Maybe[String], Maybe[StageMeta], String)] =
    this match
      case Single(elem)       => elem.summaries
      case Append(init, last) => init.summaries ++ last.summaries

  /**
   * Render the chain: elements joined with `andThen`, forks rendered as `par(left, right)`. Shared by
   * [[morphir.buildkit.PipelineDef#describe]] and [[morphir.buildkit.SealedPipeline#describe]] so the two cannot drift
   * — the expression can't live on the public `Pipeline` trait without naming this internal type.
   */
  def describe: String =
    this match
      case Single(elem)       => elem.describe
      case Append(init, last) => s"${init.describe} andThen ${last.describe}"
end NodeChain

private[buildkit] object Sealing:
  /**
   * Lowercase (ASCII only — folding through the JVM default locale is unsafe: Turkish locales map `I` to `ı`, not `i`,
   * and js/native runtimes can differ again); runs of non-alphanumerics become single `-`; trimmed. `Absent` when
   * nothing survives.
   */
  def slugify(label: String): Maybe[String] =
    val lowered = label.map(c => if c >= 'A' && c <= 'Z' then (c + 32).toChar else c)
    val slug    = lowered.replaceAll("[^a-z0-9]+", "-").stripPrefix("-").stripSuffix("-")
    if slug.isEmpty then Absent else Present(slug)

  /**
   * Pairs `chain` with `ids` (one per leaf stage, in definition order) into a [[SealedChain]], recursing through both
   * sides of a `ParElem`/`ParNode`. `ids` must have exactly `chain.size` entries; each element consumes the prefix
   * matching its own `size`.
   */
  def seal[I, O, S](chain: NodeChain[I, O, S], ids: Chunk[NodeId]): SealedChain[I, O, S] =
    def loopChain[I2, O2, S2](c: NodeChain[I2, O2, S2], slice: Chunk[NodeId]): SealedChain[I2, O2, S2] =
      c match
        case NodeChain.Single(elem) =>
          SealedChain.Single(loopElem(elem, slice))
        case NodeChain.Append(init, last) =>
          val initSize = init.size
          SealedChain.Append(loopChain(init, slice.take(initSize)), loopElem(last, slice.drop(initSize)))

    def loopElem[I2, O2, S2](e: DefElem[I2, O2, S2], slice: Chunk[NodeId]): SealedElem[I2, O2, S2] =
      e match
        case DefElem.StageElem(_, stage) =>
          SealedElem.StageNode(slice(0), stage)
        case DefElem.ParElem(left, right, zip) =>
          val leftSize = left.size
          SealedElem.ParNode(loopChain(left, slice.take(leftSize)), loopChain(right, slice.drop(leftSize)), zip)

    loopChain(chain, ids)
end Sealing
