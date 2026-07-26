package morphir.langkit.trees.unist

import morphir.langkit.trees.FieldName
import morphir.langkit.trees.NodeTypeName
import morphir.langkit.trees.QueryableTree

trait UnistProjection[T]:
  def span(t: T): Option[UnistSpan]

object UnistProjection:

  def project[T](tree: T, source: Option[String] = None)(using
      queryable: QueryableTree[T],
      projection: UnistProjection[T]
  ): UnistNode =
    val children          = queryable.children(tree).toIndexedSeq
    val projectedChildren = children.map(child => project(child, source))
    val data              = UnistData(
      fields = fieldIndexes(queryable.fields(tree), children),
      childCount = children.size
    )

    UnistNode(
      `type` = NodeTypeName.unwrap(queryable.nodeType(tree)),
      value = queryable.text(tree),
      position = source.flatMap(text => projection.span(tree).map(span => position(text, span))),
      data = data,
      children = projectedChildren
    )

  private def fieldIndexes[T](
      fields: Map[FieldName, Seq[T]],
      children: IndexedSeq[T]
  ): Map[String, IndexedSeq[Int]] =
    fields.toSeq
      .sortBy((field, _) => FieldName.unwrap(field))
      .map { (field, fieldChildren) =>
        val usedIndexes = scala.collection.mutable.Set.empty[Int]
        FieldName.unwrap(field) -> fieldChildren.toIndexedSeq.flatMap(fieldChild =>
          val identityMatch = firstUnusedMatch(children, usedIndexes, fieldChild, sameReference)
          val matchedIndex  =
            identityMatch.orElse(firstUnusedMatch(children, usedIndexes, fieldChild, sameNode))
          matchedIndex.foreach(usedIndexes += _)
          matchedIndex
        )
      }
      .toMap

  private def firstUnusedMatch[T](
      children: IndexedSeq[T],
      usedIndexes: scala.collection.Set[Int],
      fieldChild: T,
      matches: (T, T) => Boolean
  ): Option[Int] =
    children.zipWithIndex.collectFirst {
      case (child, index) if !usedIndexes.contains(index) && matches(child, fieldChild) => index
    }

  private def sameReference[T](left: T, right: T): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  private def sameNode[T](left: T, right: T): Boolean =
    java.util.Objects.equals(left, right)

  private def position(source: String, span: UnistSpan): UnistPosition =
    UnistPosition(
      start = pointAt(source, span.start),
      end = pointAt(source, span.end)
    )

  private def pointAt(source: String, offset: Int): UnistPoint =
    val boundedOffset = offset.max(0).min(source.length)
    var line          = 1
    var column        = 1
    var index         = 0

    while index < boundedOffset do
      if source.charAt(index) == '\n' then
        line += 1
        column = 1
      else column += 1
      index += 1

    UnistPoint(line = line, column = column, offset = Some(boundedOffset))
