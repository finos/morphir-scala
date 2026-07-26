package morphir.langkit.itest

import morphir.langkit.trees.CaptureName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree
import morphir.langkit.trees.query.Match

/**
 * Type-erased view over a query match, usable by BDD steps regardless of whether the tree under test is a CST or an
 * AST.
 */
final case class MatchView(
    rootNodeType: String,
    rootText: Option[String],
    captures: Map[String, CapturedNode]
)

object MatchView:

  def from[T](m: Match[T])(using qt: QueryableTree[T]): MatchView =
    def view(node: T): CapturedNode =
      CapturedNode(NodeTypeName.unwrap(qt.nodeType(node)), qt.text(node), qt.children(node).size)
    MatchView(
      rootNodeType = NodeTypeName.unwrap(qt.nodeType(m.root)),
      rootText = qt.text(m.root),
      captures = m.captures.map((name, node) => CaptureName.unwrap(name) -> view(node))
    )

/**
 * A captured node reduced to its nodeType, optional text, and direct child count. Enough to write all v1 capture
 * assertions without exposing the tree-specific type.
 */
final case class CapturedNode(nodeType: String, text: Option[String], childCount: Int)
