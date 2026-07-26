package morphir.langkit.elm.ast

/**
 * A zipper-style cursor for navigating the AST. Tracks the current node and its position within the tree, allowing
 * movement to parent, children, and siblings without re-traversing from the root.
 */
final case class AstCursor private (
    node: AstNode,
    parentCrumbs: List[AstCursor.Crumb]
):

  import AstCursor.*

  /** The direct children of the current node. */
  def children: List[AstNode] = AstVisitor.children(node)

  /** Move to the first child, if any. */
  def firstChild: Option[AstCursor] =
    val kids = children
    kids match
      case head :: tail =>
        val crumb = Crumb(node, Nil, tail, parentCrumbs)
        Some(AstCursor(head, crumb :: parentCrumbs))
      case Nil => None

  /** Move to the last child, if any. */
  def lastChild: Option[AstCursor] =
    val kids = children
    kids match
      case Nil => None
      case _   =>
        val reversed = kids.reverse
        val crumb    = Crumb(node, reversed.tail.reverse, Nil, parentCrumbs)
        Some(AstCursor(reversed.head, crumb :: parentCrumbs))

  /** Move to the next sibling, if any. */
  def nextSibling: Option[AstCursor] = parentCrumbs match
    case Crumb(parent, lefts, right :: rights, grandCrumbs) :: _ =>
      val crumb = Crumb(parent, lefts :+ node, rights, grandCrumbs)
      Some(AstCursor(right, crumb :: grandCrumbs))
    case _ => None

  /** Move to the previous sibling, if any. */
  def previousSibling: Option[AstCursor] = parentCrumbs match
    case Crumb(parent, lefts, rights, grandCrumbs) :: _ if lefts.nonEmpty =>
      val prev  = lefts.last
      val crumb = Crumb(parent, lefts.init, node :: rights, grandCrumbs)
      Some(AstCursor(prev, crumb :: grandCrumbs))
    case _ => None

  /** Move to the parent node, if any. */
  def parent: Option[AstCursor] = parentCrumbs match
    case Crumb(parentNode, _, _, grandCrumbs) :: _ =>
      Some(AstCursor(parentNode, grandCrumbs))
    case Nil => None

  /** True if this cursor is at the root of the tree. */
  def isRoot: Boolean = parentCrumbs.isEmpty

  /** True if this cursor has no children. */
  def isLeaf: Boolean = children.isEmpty

  /** Return the root node by walking up from the current position. */
  def root: AstCursor =
    parent match
      case Some(p) => p.root
      case None    => this

  /** Depth of this cursor in the tree (0 = root). */
  def depth: Int = parentCrumbs.length

  /** Walk the tree depth-first, pre-order, starting from this cursor. */
  def preOrder: LazyList[AstCursor] =
    def go(cursor: AstCursor): LazyList[AstCursor] =
      cursor #:: cursor.firstChild.map(goDown).getOrElse(goRight(cursor))
    def goDown(cursor: AstCursor): LazyList[AstCursor] =
      cursor #:: cursor.firstChild.map(goDown).getOrElse(goRight(cursor))
    def goRight(cursor: AstCursor): LazyList[AstCursor] =
      cursor.nextSibling match
        case Some(sibling) => goDown(sibling)
        case None          =>
          cursor.parent match
            case Some(p) => goRight(p)
            case None    => LazyList.empty
    go(this)

object AstCursor:

  /** A breadcrumb recording the parent node and the siblings to the left and right. */
  case class Crumb(
      parent: AstNode,
      leftSiblings: List[AstNode],
      rightSiblings: List[AstNode],
      parentCrumbs: List[Crumb]
  )

  /** Create a cursor rooted at the given node. */
  def apply(root: AstNode): AstCursor = AstCursor(root, Nil)
