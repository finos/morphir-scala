package morphir.buildkit.internal

import kyo.*
import morphir.buildkit.*

/**
 * One element of a pipeline definition: a single stage, a fork into two peer chains that both receive the same input
 * and whose results are paired by `zip`, or a fan-out that runs a whole child chain once per element of a `Chunk`.
 * `par` and `fanOut` are the non-stage elements so far; a later task (branch) adds a case here.
 *
 * `ParElem`'s type parameters follow the same existential-middle technique as [[Stage.AndThen]]: `I2` is the shared
 * input to both branches, `Z` is the zipped result exposed as this element's `O`, and `S1 & S2` is the combined effect
 * row. `zip` is captured monomorphically at `par` call time (from a `Zippable` instance), so this GADT never carries
 * the typeclass itself.
 *
 * `FanOutElem`'s own type parameters (`A`, `B`, `S2`) name the child chain's input/output/effect row directly, rather
 * than the enclosing element's `I`/`O`/`S`: the enclosing element's `I` is fixed to `Chunk[A]` and `O` to `Chunk[B]` by
 * the `extends` clause, mirroring how `ParElem` fixes its own `I`/`O` from its case type parameters.
 */
private[buildkit] enum DefElem[-I, +O, S]:
  case StageElem(explicitId: Maybe[String], stage: Stage[I, O, S])
  case ParElem[I2, O1, O2, Z, S1, S2](
      left: NodeChain[I2, O1, S1],
      right: NodeChain[I2, O2, S2],
      zip: (O1, O2) => Z
  ) extends DefElem[I2, Z, S1 & S2]
  case FanOutElem[A, B, S2](
      explicitId: Maybe[String],
      each: NodeChain[A, B, S2]
  ) extends DefElem[Chunk[A], Chunk[B], S2]

  /**
   * Number of leaf slots this element expands to, walked in definition order: `1` for a stage or a fan-out (a fan-out's
   * own id-space is nested, not flattened into the enclosing chain's), both sides summed for a `ParElem`.
   */
  def size: Int =
    this match
      case StageElem(_, _)         => 1
      case ParElem(left, right, _) => left.size + right.size
      case FanOutElem(_, _)        => 1

  /**
   * Leaf summaries, in definition order: one triple per stage, recursing through both sides of a `ParElem`. A fan-out
   * contributes exactly one triple for itself — no label, since a pipeline (unlike a `Stage`) carries no `StageMeta` —
   * and does not recurse into its child chain, which is sealed independently.
   */
  def summaries: Chunk[(Maybe[String], Maybe[StageMeta], String)] =
    this match
      case StageElem(explicitId, stage) => Chunk((explicitId, stage.meta, stage.describe))
      case ParElem(left, right, _)      => left.summaries ++ right.summaries
      case FanOutElem(explicitId, _)    => Chunk((explicitId, Absent, describe))

  /** Render this element: a stage renders as its own description; a fork renders as `par(left, right)`. */
  def describe: String =
    this match
      case StageElem(_, stage)     => stage.describe
      case ParElem(left, right, _) => s"par(${left.describe}, ${right.describe})"
      case FanOutElem(_, each)     => s"fanOut(${each.describe})"
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
   * Assign ids to `chain`'s own elements and validate the result into a [[SealedChain]], accumulating every failure: an
   * invalid explicit id, a duplicate id among this level's own nodes, or — for a `FanOutElem` — its child chain failing
   * to seal.
   *
   * A `ParElem`'s two sides share this level's id namespace, so duplicates across sides are caught here (unchanged from
   * before `fanOut` existed). A `FanOutElem`'s child chain does not share it: its ids are validated by a nested call to
   * this same function, so they may repeat ids used elsewhere in the enclosing chain. When that nested call fails, its
   * errors re-enter this level's aggregate with this node's own assigned id segment prefixed onto each
   * [[SealError.DuplicateNodeId]] — an [[SealError.InvalidSegment]] names the raw string that failed validation, which
   * has no path to prefix, so it passes through unchanged.
   */
  def sealChain[I, O, S](chain: NodeChain[I, O, S]): Result[SealErrors, SealedChain[I, O, S]] =
    val summaries                                  = chain.summaries
    val assigned: Chunk[Result[SealError, NodeId]] =
      summaries.zipWithIndex.map { case ((explicit, meta, _), index) =>
        explicit match
          case Present(value) => NodeId.segment(value)
          case Absent         =>
            val slug: Maybe[String] = meta.map(_.label).flatMap(slugify)
            slug match
              case Present(value) => Result.succeed(NodeId.unsafe(Chunk(value)))
              case Absent         => Result.succeed(NodeId.unsafe(Chunk(s"node-$index")))
      }
    val segmentErrors = assigned.collect { case Result.Failure(error) => error }
    val ids           = assigned.collect { case Result.Success(id) => id }
    val duplicates    =
      ids
        .groupBy(_.render)
        .toSeq
        .collect { case (rendered, group) if group.size > 1 => (rendered, group) }
        .sortBy(_._1)
        .map((_, group) => SealError.DuplicateNodeId(group.head))

    if segmentErrors.nonEmpty || duplicates.nonEmpty then
      Result.fail(SealErrors.unsafe(segmentErrors ++ Chunk.from(duplicates)))
    else pairChain(chain, ids)

  /**
   * Pairs `chain` with `ids` (one per own-level element, in definition order) into a [[SealedChain]], recursing through
   * both sides of a `ParElem` and, for a `FanOutElem`, sealing its child chain independently. `ids` must have exactly
   * `chain.size` entries — guaranteed by [[sealChain]], the only caller — each element consuming the slice matching its
   * own `size`.
   */
  private def pairChain[I2, O2, S2](
      c: NodeChain[I2, O2, S2],
      slice: Chunk[NodeId]
  ): Result[SealErrors, SealedChain[I2, O2, S2]] =
    c match
      case NodeChain.Single(elem) =>
        pairElem(elem, slice).map(SealedChain.Single(_))
      case NodeChain.Append(init, last) =>
        val initSize = init.size
        combine(pairChain(init, slice.take(initSize)), pairElem(last, slice.drop(initSize)))(SealedChain.Append(_, _))

  private def pairElem[I2, O2, S2](
      e: DefElem[I2, O2, S2],
      slice: Chunk[NodeId]
  ): Result[SealErrors, SealedElem[I2, O2, S2]] =
    e match
      case DefElem.StageElem(_, stage) =>
        Result.succeed(SealedElem.StageNode(slice(0), stage))
      case DefElem.ParElem(left, right, zip) =>
        val leftSize = left.size
        combine(pairChain(left, slice.take(leftSize)), pairChain(right, slice.drop(leftSize)))(
          SealedElem.ParNode(_, _, zip)
        )
      case DefElem.FanOutElem(_, each) =>
        val ownId = slice(0)
        sealChain(each) match
          case Result.Success(sealedEach) => Result.succeed(SealedElem.FanOutNode(ownId, sealedEach))
          case Result.Failure(errors)     =>
            Result.fail(SealErrors.unsafe(errors.errors.map(prefixNested(_, ownId))))
          case Result.Panic(ex) => Result.panic(ex)

  /**
   * Combine two seal results, accumulating errors from both sides when either (or both) fail. Neither side of this
   * recursion can actually panic — `pairChain`/`pairElem` only ever construct `Success` or `Failure` — but `Result` has
   * a third `Panic` arm, so a nested match on both sides, rather than a wildcard fallback, is what lets the
   * exhaustiveness check (required under `-Werror`) confirm that without hiding a real panic behind a manufactured
   * `SealError`.
   */
  private def combine[A, B, C](
      a: Result[SealErrors, A],
      b: Result[SealErrors, B]
  )(f: (A, B) => C): Result[SealErrors, C] =
    a match
      case Result.Panic(ex)   => Result.panic(ex)
      case Result.Failure(ae) =>
        b match
          case Result.Panic(ex)   => Result.panic(ex)
          case Result.Failure(be) => Result.fail(SealErrors.unsafe(ae.errors ++ be.errors))
          case Result.Success(_)  => Result.fail(ae)
      case Result.Success(av) =>
        b match
          case Result.Panic(ex)   => Result.panic(ex)
          case Result.Failure(be) => Result.fail(be)
          case Result.Success(bv) => Result.succeed(f(av, bv))

  /** Qualify a nested fan-out child's seal error with the fan-out node's own assigned id segment. */
  private def prefixNested(error: SealError, parentId: NodeId): SealError =
    error match
      case SealError.DuplicateNodeId(id)     => SealError.DuplicateNodeId(id.prefixed(parentId.render))
      case invalid: SealError.InvalidSegment => invalid
end Sealing
