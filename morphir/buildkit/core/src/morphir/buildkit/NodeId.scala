package morphir.buildkit

import kyo.*

/**
 * The identity of a node in a sealed pipeline: a non-empty path of validated segments.
 *
 * This slice always produces single-segment ids. The path structure exists now so that when pipeline nesting arrives, a
 * child plan's ids gain the parent node's segment as a prefix, growing in the same direction as the executor's
 * provenance path — outermost first. The two are not identical in shape today: provenance is a `Chunk[StageMeta]` that
 * only grows for labelled nodes, while an id path is expected to grow for every nesting level regardless of labelling.
 * They are intended to align once nesting lands, not claimed to match now.
 */
opaque type NodeId = Chunk[String]

object NodeId:

  /**
   * Validate `value` as a single-segment id: non-blank, not `.` or `..`, and free of `/`, `\` and control characters —
   * reserved so a future path-valued id cannot collide with filesystem or URL path conventions.
   */
  def segment(value: String): Result[SealError, NodeId] =
    if value.isBlank then Result.fail(SealError.InvalidSegment(value, "blank"))
    else if value == "." then Result.fail(SealError.InvalidSegment(value, "is '.'"))
    else if value == ".." then Result.fail(SealError.InvalidSegment(value, "is '..'"))
    else if value.contains("/") then Result.fail(SealError.InvalidSegment(value, "contains '/'"))
    else if value.contains("\\") then Result.fail(SealError.InvalidSegment(value, "contains '\\'"))
    else if value.exists(Character.isISOControl) then
      Result.fail(SealError.InvalidSegment(value, "contains a control character"))
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
    if literal == "." then report.errorAndAbort(s"invalid node id segment '$literal': is '.'")
    if literal == ".." then report.errorAndAbort(s"invalid node id segment '$literal': is '..'")
    if literal.contains("/") then report.errorAndAbort(s"invalid node id segment '$literal': contains '/'")
    if literal.contains("\\") then report.errorAndAbort(s"invalid node id segment '$literal': contains '\\'")
    if literal.exists(Character.isISOControl) then
      report.errorAndAbort(s"invalid node id segment '$literal': contains a control character")
    '{ NodeId.unsafe(Chunk(${ Expr(literal) })) }
end NodeIdMacros
