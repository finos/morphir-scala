package morphir.langkit.trees.query

final case class QueryCursor private (
    node: QueryNode,
    parentCrumbs: List[QueryCursor.Crumb]
):

  import QueryCursor.*

  def children: List[QueryNode] =
    QueryVisitor.children(node)

  def firstChild: Option[QueryCursor] =
    children match
      case head :: tail =>
        val crumb = Crumb(node, Nil, tail, parentCrumbs)
        Some(QueryCursor(head, crumb :: parentCrumbs))
      case Nil => None

  def lastChild: Option[QueryCursor] =
    val kids = children
    kids match
      case Nil => None
      case _   =>
        val reversed = kids.reverse
        val crumb    = Crumb(node, reversed.tail.reverse, Nil, parentCrumbs)
        Some(QueryCursor(reversed.head, crumb :: parentCrumbs))

  def nextSibling: Option[QueryCursor] = parentCrumbs match
    case Crumb(parent, lefts, right :: rights, grandCrumbs) :: _ =>
      val crumb = Crumb(parent, lefts :+ node, rights, grandCrumbs)
      Some(QueryCursor(right, crumb :: grandCrumbs))
    case _ => None

  def previousSibling: Option[QueryCursor] = parentCrumbs match
    case Crumb(parent, lefts, rights, grandCrumbs) :: _ if lefts.nonEmpty =>
      val prev  = lefts.last
      val crumb = Crumb(parent, lefts.init, node :: rights, grandCrumbs)
      Some(QueryCursor(prev, crumb :: grandCrumbs))
    case _ => None

  def parent: Option[QueryCursor] = parentCrumbs match
    case Crumb(parentNode, _, _, grandCrumbs) :: _ =>
      Some(QueryCursor(parentNode, grandCrumbs))
    case Nil => None

  def isRoot: Boolean = parentCrumbs.isEmpty

  def isLeaf: Boolean = children.isEmpty

  def root: QueryCursor =
    parent match
      case Some(value) => value.root
      case None        => this

  def depth: Int = parentCrumbs.length

  def preOrder: LazyList[QueryCursor] =
    def go(cursor: QueryCursor): LazyList[QueryCursor] =
      cursor #:: cursor.firstChild.map(goDown).getOrElse(goRight(cursor))
    def goDown(cursor: QueryCursor): LazyList[QueryCursor] =
      cursor #:: cursor.firstChild.map(goDown).getOrElse(goRight(cursor))
    def goRight(cursor: QueryCursor): LazyList[QueryCursor] =
      cursor.nextSibling match
        case Some(sibling) => goDown(sibling)
        case None          =>
          cursor.parent match
            case Some(p) => goRight(p)
            case None    => LazyList.empty
    go(this)

object QueryCursor:

  case class Crumb(
      parent: QueryNode,
      leftSiblings: List[QueryNode],
      rightSiblings: List[QueryNode],
      parentCrumbs: List[Crumb]
  )

  def apply(query: Query): QueryCursor =
    QueryCursor(QueryNode.Root(query), Nil)
