package morphir.langkit.trees.query

import scala.annotation.tailrec

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName

/** Root of the query AST. A parsed query is one pattern plus zero or more predicates that constrain captured nodes. */
final case class Query(root: Pattern, predicates: List[Predicate]) derives CanEqual:

  /**
   * Every capture name bound anywhere in the root pattern.
   *
   * Useful for validation (e.g. confirming every `@foo` referenced by a predicate is actually bound by the pattern).
   */
  def captureNames: Set[CaptureName] =
    @tailrec
    def go(stack: List[Pattern], acc: Set[CaptureName]): Set[CaptureName] = stack match
      case Nil       => acc
      case p :: rest =>
        val withOwn = p.capture.fold(acc)(acc + _)
        p match
          case NodePattern(_, fields, children, _, _, _, _) =>
            go(fields.map(_.pattern) ::: children ::: rest, withOwn)
          case MultiPattern(patterns) =>
            go(patterns ::: rest, withOwn)
          case AlternationPattern(patterns, _) =>
            go(patterns ::: rest, withOwn)
          case _: WildcardPattern =>
            go(rest, withOwn)
    go(List(root), Set.empty)

/**
 * A tree pattern: either a node pattern with an optional list of named sub-patterns, or a wildcard that matches any
 * node.
 */
sealed trait Pattern derives CanEqual:
  /** Optional capture name bound to the node that matches this pattern. */
  def capture: Option[CaptureName]

/**
 * Matches a single node whose `nodeType` equals `nodeType`, subject to optional field, child, and negated-field
 * constraints.
 *
 * @param nodeType
 *   The expected node kind — must equal `QueryableTree[T].nodeType(node)` for the match to succeed.
 * @param fieldPatterns
 *   Named sub-tree constraints. Each entry must match at least one node returned by
 *   `QueryableTree[T].fields(node)(fp.name)`.
 * @param childPatterns
 *   Ordered unfielded child constraints. Children are matched in the order they appear in `children(node)`.
 * @param capture
 *   If present, the matched node is bound to this name in the resulting `Match.captures`.
 * @param adjacentChildAnchors
 *   Set of child-pattern indices `i` for which the matched child at index `i` and the matched child at index `i + 1`
 *   must be immediately adjacent siblings in the parent node (no intervening siblings). Written `(A) . (B)` in query
 *   syntax.
 * @param negatedFields
 *   Set of field names that must be absent (or empty) on the matched node. Written `!fieldName` in query syntax.
 * @param childQuantifiers
 *   Per-child-pattern quantifiers (`?`, `*`, `+`). A missing entry means the child pattern must match exactly once.
 */
final case class NodePattern(
    nodeType: NodeTypeName,
    fieldPatterns: List[FieldPattern],
    childPatterns: List[Pattern],
    capture: Option[CaptureName],
    adjacentChildAnchors: Set[Int] = Set.empty,
    negatedFields: Set[FieldName] = Set.empty,
    childQuantifiers: Map[Int, QuantifierKind] = Map.empty
) extends Pattern derives CanEqual

/**
 * Combines several top-level patterns into one query, each matched independently against the entire tree.
 *
 * Matches from each constituent pattern are yielded in pattern order (all matches for pattern 0, then all for pattern
 * 1, …). `capture` is always `None`; top-level patterns bind captures individually.
 */
final case class MultiPattern(patterns: List[Pattern]) extends Pattern derives CanEqual:
  val capture: Option[CaptureName] = None

/**
 * Matches the first branch in `patterns` that structurally matches a candidate node.
 *
 * Written `[(A) (B) …]` in query syntax. An optional `capture` binds the matched node regardless of which branch
 * succeeded.
 */
final case class AlternationPattern(patterns: List[Pattern], capture: Option[CaptureName]) extends Pattern
    derives CanEqual

/**
 * Matches any node unconditionally.
 *
 * Written `_` or `(_)` in query syntax; optionally followed by `@name` to capture the matched node.
 */
final case class WildcardPattern(capture: Option[CaptureName]) extends Pattern derives CanEqual

/**
 * Repetition quantifier for an unfielded child pattern inside a [[NodePattern]].
 *
 * Written immediately after the child pattern in query syntax: `?` for optional, `*` for zero-or-more, `+` for
 * one-or-more.
 */
enum QuantifierKind derives CanEqual:
  /** The child pattern may match zero or one child. */
  case Optional

  /** The child pattern may match zero or more children. */
  case ZeroOrMore

  /** The child pattern must match one or more children. */
  case OneOrMore

/**
 * A named sub-pattern attached to a parent [[NodePattern]].
 *
 * Written `fieldName: pattern` in query syntax.
 */
final case class FieldPattern(name: FieldName, pattern: Pattern) derives CanEqual

/**
 * A predicate clause that constrains captured nodes after structural matching.
 *
 * Predicates are evaluated after all structural patterns have matched. A match is only included in the result if every
 * predicate in the query returns `true`.
 */
sealed trait Predicate derives CanEqual

/**
 * Requires the text of `left` and `right` (either captures or string literals) to be equal.
 *
 * Written `(#eq? @left @right)` or `(#eq? @left "literal")` in query syntax.
 */
final case class EqPredicate(left: PredicateArg, right: PredicateArg) extends Predicate derives CanEqual

/**
 * Requires the text of the captured node referred to by `arg` to match the compiled `regex`.
 *
 * Written `(#match? @capture "regex")` in query syntax.
 */
final case class MatchPredicate(arg: PredicateArg, regex: RegexPattern) extends Predicate derives CanEqual

/**
 * Requires the text of `left` and `right` to be unequal.
 *
 * Written `(#not-eq? @left @right)` or `(#not-eq? @left "literal")` in query syntax.
 */
final case class NotEqPredicate(left: PredicateArg, right: PredicateArg) extends Predicate derives CanEqual

/**
 * Requires the text of the captured node referred to by `arg` to *not* match the compiled `regex`.
 *
 * Written `(#not-match? @capture "regex")` in query syntax.
 */
final case class NotMatchPredicate(arg: PredicateArg, regex: RegexPattern) extends Predicate derives CanEqual

/** Argument supplied to a predicate: a reference to a capture or a string literal. */
sealed trait PredicateArg derives CanEqual

/** Refers to a previously captured node by name; the predicate resolves its text via `QueryableTree[T].text`. */
final case class CaptureRef(name: CaptureName) extends PredicateArg derives CanEqual

/** A constant string value supplied directly in the query source. */
final case class StringArg(value: String) extends PredicateArg derives CanEqual

/**
 * One successful match: the node that matched the root pattern plus every capture binding collected along the way.
 *
 * @param root
 *   The node that matched the outermost pattern.
 * @param captures
 *   All `@name` bindings collected during structural matching, keyed by [[CaptureName]].
 */
final case class Match[T](root: T, captures: Map[CaptureName, T]) derives CanEqual
