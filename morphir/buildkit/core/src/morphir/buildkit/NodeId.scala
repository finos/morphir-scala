package morphir.buildkit

import kyo.*

/**
 * The identity of a node in a sealed pipeline: a non-empty path of validated segments.
 *
 * This slice always produces single-segment ids. The path structure exists now so that when pipeline nesting arrives, a
 * child plan's ids gain the parent node's segment as a prefix — the same shape as the executor's provenance path —
 * without retrofitting paths onto flat strings.
 */
opaque type NodeId = Chunk[String]

object NodeId:

  /** Validate `value` as a single-segment id: non-blank, no `/`. */
  def segment(value: String): Result[SealError, NodeId] =
    if value.isBlank then Result.fail(SealError.InvalidSegment(value, "blank"))
    else if value.contains("/") then Result.fail(SealError.InvalidSegment(value, "contains '/'"))
    else Result.succeed(Chunk(value))

  /** Trusted constructor for segments the sealer has already produced or validated. */
  private[buildkit] def unsafe(segments: Chunk[String]): NodeId = segments

  extension (id: NodeId)
    /** The path rendered with `/` separators. */
    def render: String = id.mkString("/")

    /** The underlying segments, outermost first. */
    def segments: Chunk[String] = id
end NodeId

/**
 * Compile-time validated literal node ids: `nodeId"parse"`.
 *
 * Literal-only by design — interpolated arguments are rejected at compile time; a dynamically computed id goes through
 * `NodeId.segment` and participates in seal-time error accumulation instead. Follows the repository convention of
 * literal interpolators for validated identifiers.
 */
extension (inline sc: StringContext)
  inline def nodeId(inline args: Any*): NodeId = ${ NodeIdMacros.nodeIdImpl('sc, 'args) }

private[buildkit] object NodeIdMacros:
  import scala.quoted.*

  def nodeIdImpl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[NodeId] =
    import quotes.reflect.report
    args match
      case Varargs(Seq()) => ()
      case _ => report.errorAndAbort("nodeId accepts no interpolated arguments; use NodeId.segment for dynamic ids")
    val parts   = sc.valueOrAbort.parts
    val literal = parts.mkString
    if literal.isBlank then report.errorAndAbort(s"invalid node id segment '$literal': blank")
    if literal.contains("/") then report.errorAndAbort(s"invalid node id segment '$literal': contains '/'")
    '{ NodeId.unsafe(Chunk(${ Expr(literal) })) }
end NodeIdMacros
