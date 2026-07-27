package morphir.langkit.trees.unist

final case class UnistPoint(
    line: Int,
    column: Int,
    offset: Option[Int] = None
) derives CanEqual

final case class UnistPosition(
    start: UnistPoint,
    end: UnistPoint
) derives CanEqual

final case class UnistData(
    fields: Map[String, IndexedSeq[Int]] = Map.empty,
    childCount: Int = 0
) derives CanEqual

object UnistData:
  val empty: UnistData = UnistData()

final case class UnistNode(
    `type`: String,
    value: Option[String] = None,
    position: Option[UnistPosition] = None,
    data: UnistData = UnistData.empty,
    children: IndexedSeq[UnistNode] = IndexedSeq.empty
) derives CanEqual

final case class UnistSpan(start: Int, end: Int) derives CanEqual

object UnistSpan:

  def fromOffsetLength(offset: Int, length: Int): UnistSpan =
    UnistSpan(offset, offset + length)
