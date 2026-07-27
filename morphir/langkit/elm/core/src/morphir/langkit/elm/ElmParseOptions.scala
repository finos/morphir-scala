package morphir.langkit.elm

import morphir.langkit.elm.parser.OperatorTable

/**
 * What to do about a construct Elm rejects.
 *
 * Every field of [[ElmParseOptions]] that takes one of these defaults to [[Reject]], so the parser's out-of-the-box
 * behaviour is Elm's. [[Accept]] exists for tooling that would rather have a tree than a diagnostic — an editor
 * highlighting a half-typed expression, a formatter working on a file that does not compile yet — and each site
 * documents the shape it produces instead.
 */
enum Leniency derives CanEqual:
  case Reject, Accept

/**
 * How to parse Elm.
 *
 * The default, [[ElmParseOptions.elm]], is canonical Elm: what `elm/compiler` accepts, and nothing else. Departures are
 * opt-in and named, so a caller relying on one says so at the call site rather than inheriting it by accident.
 *
 * @param operators
 *   fixities the parser knows before it reads the module. Defaults to [[OperatorTable.wellKnown]] — the official Elm
 *   packages that declare operators. A caller that resolves imports can supply a fuller table.
 * @param unknownOperator
 *   what to do with an operator no fixity in scope names. [[Leniency.Accept]] assumes [[OperatorTable.unknownFixity]]
 *   (precedence 9, left-associative) rather than failing.
 * @param operatorChainConflict
 *   what to do when adjacent operators of equal precedence cannot be grouped — a non-associative operator chained, or a
 *   left- and a right-associative operator mixed. [[Leniency.Accept]] groups them to the left.
 * @param tupleArity
 *   what to do with a tuple of more than three entries, which Elm rejects. [[Leniency.Accept]] keeps the tuple as
 *   parsed and reports it as advisory.
 */
final case class ElmParseOptions(
    operators: OperatorTable = OperatorTable.wellKnown,
    unknownOperator: Leniency = Leniency.Reject,
    operatorChainConflict: Leniency = Leniency.Reject,
    tupleArity: Leniency = Leniency.Reject
) derives CanEqual:

  def withOperators(table: OperatorTable): ElmParseOptions           = copy(operators = table)
  def withUnknownOperator(leniency: Leniency): ElmParseOptions       = copy(unknownOperator = leniency)
  def withOperatorChainConflict(leniency: Leniency): ElmParseOptions = copy(operatorChainConflict = leniency)
  def withTupleArity(leniency: Leniency): ElmParseOptions            = copy(tupleArity = leniency)

object ElmParseOptions:

  /** Canonical Elm semantics. The default everywhere. */
  val elm: ElmParseOptions = ElmParseOptions()

  /**
   * Best-effort parsing for tooling: never fail over something that only affects how a tree is grouped.
   *
   * An operator of unknown fixity binds tightest and to the left, and a chain that Elm would refuse to group is grouped
   * to the left. The resulting tree is a guess, and is not a basis for compiling.
   */
  val lenient: ElmParseOptions = ElmParseOptions(
    unknownOperator = Leniency.Accept,
    operatorChainConflict = Leniency.Accept,
    tupleArity = Leniency.Accept
  )
