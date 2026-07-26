package morphir.langkit.trees.query

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName

/**
 * Serializes a [[Query]] back to a canonical, minimal S-expression string.
 *
 * The output uses single spaces to separate tokens and omits all comments and redundant whitespace. It is guaranteed to
 * round-trip through [[QueryParser.parse]]:
 * {{{
 *   QueryParser.parse(QueryPrinter.print(q)).isSuccess
 * }}}
 *
 * Canonical ordering within a node:
 *   - Negated field constraints appear before positive field patterns.
 *   - Positive field patterns appear in the order given by `NodePattern.fieldPatterns`.
 *   - Unfielded child patterns (with any interspersed anchors and quantifiers) follow all field patterns.
 *   - Predicates are appended after the root pattern in their original order.
 */
object QueryPrinter:

  /** Render `query` as a canonical S-expression string. */
  def print(query: Query): String =
    val patternParts = query.root match
      case MultiPattern(patterns) => patterns.map(printPattern)
      case p                      => List(printPattern(p))
    val predicateParts = query.predicates.map(printPredicate)
    (patternParts ++ predicateParts).mkString(" ")

  // --- Patterns ------------------------------------------------------------

  private def printPattern(p: Pattern): String = p match
    case WildcardPattern(cap) =>
      "_" + capSuffix(cap)

    case AlternationPattern(patterns, cap) =>
      val inner = patterns.map(printPattern).mkString(" ")
      s"[$inner]${capSuffix(cap)}"

    case MultiPattern(patterns) =>
      patterns.map(printPattern).mkString(" ")

    case NodePattern(nt, fieldPatterns, childPatterns, cap, adjacentAnchors, negatedFields, quantifiers) =>
      val ntStr        = NodeTypeName.unwrap(nt)
      val negatedParts = negatedFields.toList.map(FieldName.unwrap).sorted.map(fn => s"!$fn")
      val fieldParts   = fieldPatterns.map(fp => s"${FieldName.unwrap(fp.name)}: ${printPattern(fp.pattern)}")
      val childParts   = childPatterns.zipWithIndex.flatMap { (child, i) =>
        val withQuantifier = quantifiers.get(i) match
          case Some(QuantifierKind.Optional)   => printPattern(child) + "?"
          case Some(QuantifierKind.ZeroOrMore) => printPattern(child) + "*"
          case Some(QuantifierKind.OneOrMore)  => printPattern(child) + "+"
          case None                            => printPattern(child)
        // A `.` anchor at index i means "child i and child i+1 must be adjacent siblings".
        if adjacentAnchors.contains(i) then List(withQuantifier, ".") else List(withQuantifier)
      }
      val members = negatedParts ++ fieldParts ++ childParts
      val body    = if members.isEmpty then "" else s" ${members.mkString(" ")}"
      s"($ntStr$body)${capSuffix(cap)}"

  // --- Predicates ----------------------------------------------------------

  private def printPredicate(p: Predicate): String = p match
    case EqPredicate(left, right)      => s"(#eq? ${printArg(left)} ${printArg(right)})"
    case MatchPredicate(arg, regex)    => s"(#match? ${printArg(arg)} ${printRegex(regex)})"
    case NotEqPredicate(left, right)   => s"(#not-eq? ${printArg(left)} ${printArg(right)})"
    case NotMatchPredicate(arg, regex) => s"(#not-match? ${printArg(arg)} ${printRegex(regex)})"

  // --- Helpers -------------------------------------------------------------

  private def capSuffix(cap: Option[CaptureName]): String =
    cap.fold("")(c => s" @${CaptureName.unwrap(c)}")

  private def printArg(arg: PredicateArg): String = arg match
    case CaptureRef(name) => s"@${CaptureName.unwrap(name)}"
    case StringArg(value) => s""""${escapeString(value)}""""

  private def printRegex(rx: RegexPattern): String =
    s""""${escapeString(rx.source)}""""

  /**
   * Escape double-quote and backslash so the output is a valid query string literal.
   *
   * Note: [[QueryParser]] does not support escape sequences in string literals. Consequently, any `StringArg` or
   * `RegexPattern` produced by `QueryParser.parse` will never contain a `"` character. The escaping here is defensive
   * for programmatically constructed queries; callers should ensure that string values do not contain literal `"` if a
   * round-trip through `QueryParser` is required.
   */
  private def escapeString(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")
