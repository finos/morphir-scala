package morphir.langkit.trees.query

import kyo.*

/**
 * Effect-tracked sibling of the pure [[QueryVisitor]] for the query-AST [[QueryNode]] tree.
 *
 * Mirrors the pre-order traversal of `QueryVisitor.foldLeft` / `QueryVisitor.collect`, but threads each callback
 * through the Kyo effect row `S` so callers can interleave `Abort`, `Var`, `Emit`, etc. with the walk.
 *
 * `visit` and `fold` accept either a [[Query]] (walk starts from the synthetic [[QueryNode.Root]]) or a [[Pattern]]
 * (walk starts at the corresponding [[QueryNode.PatternNode]]).
 */
object KyoQueryVisitor:

  /** Visit every node reachable from `query` in pre-order, threading effects through `S`. */
  def visit[S](query: Query)(f: QueryNode => Unit < S)(using frame: Frame): Unit < S =
    traverse(QueryNode.Root(query))(f)

  /** Visit every node reachable from `pattern` in pre-order, threading effects through `S`. */
  def visit[S](pattern: Pattern)(f: QueryNode => Unit < S)(using frame: Frame): Unit < S =
    traverse(QueryNode.PatternNode(pattern))(f)

  /** Visit every node reachable from `node` in pre-order, threading effects through `S`. */
  def visit[S](node: QueryNode)(f: QueryNode => Unit < S)(using frame: Frame): Unit < S =
    traverse(node)(f)

  /** Pre-order left fold over every node reachable from `query`. */
  def fold[A, S](query: Query, zero: A)(f: (A, QueryNode) => A < S)(using frame: Frame): A < S =
    traverseFold(QueryNode.Root(query), zero)(f)

  /** Pre-order left fold over every node reachable from `pattern`. */
  def fold[A, S](pattern: Pattern, zero: A)(f: (A, QueryNode) => A < S)(using frame: Frame): A < S =
    traverseFold(QueryNode.PatternNode(pattern), zero)(f)

  /** Pre-order left fold over every node reachable from `node`. */
  def fold[A, S](node: QueryNode, zero: A)(f: (A, QueryNode) => A < S)(using frame: Frame): A < S =
    traverseFold(node, zero)(f)

  private def traverse[S](node: QueryNode)(f: QueryNode => Unit < S)(using frame: Frame): Unit < S =
    f(node).flatMap(_ => visitChildren(QueryVisitor.children(node))(f))

  private def traverseFold[A, S](node: QueryNode, zero: A)(f: (A, QueryNode) => A < S)(using frame: Frame): A < S =
    f(zero, node).flatMap(acc1 => foldChildren(QueryVisitor.children(node), acc1)(f))

  private def visitChildren[S](children: List[QueryNode])(f: QueryNode => Unit < S)(using frame: Frame): Unit < S =
    children match
      case Nil       => ()
      case h :: rest => traverse(h)(f).flatMap(_ => visitChildren(rest)(f))

  private def foldChildren[A, S](children: List[QueryNode], acc: A)(f: (A, QueryNode) => A < S)(using
      frame: Frame
  ): A < S =
    children match
      case Nil       => acc
      case h :: rest => traverseFold(h, acc)(f).flatMap(next => foldChildren(rest, next)(f))
