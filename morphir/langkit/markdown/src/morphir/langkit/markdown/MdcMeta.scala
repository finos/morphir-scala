package morphir.langkit.markdown

import kyo.*
import morphir.langkit.core.Span

/**
 * A typed key into [[MdcMeta.data]].
 *
 * Minting a key demands a [[Schema]], so every value that can enter a node's data is provably serializable — what makes
 * a future Unist `data` projection total and lets annotated trees persist. Identity is the name *and* the value type:
 * two libraries minting `MetaKey[Int]("weight")` and `MetaKey[String]("weight")` hold different keys, which is what
 * makes [[MdcMeta.get]]'s cast safe by construction.
 */
final class MetaKey[A](val name: String)(using val schema: Schema[A], val tag: Tag[A]):
  override def equals(other: Any): Boolean = other match
    case that: MetaKey[?] => name == that.name && tag =:= that.tag
    case _                => false
  override def hashCode: Int    = name.hashCode
  override def toString: String = s"MetaKey($name: ${tag.show})"

object MetaKey:
  def apply[A](name: String)(using Schema[A], Tag[A]): MetaKey[A] = new MetaKey[A](name)

/**
 * What a node knows about itself beyond its content: unist's `position` and `data` pair.
 *
 * `span` is Present exactly when the node came from a parse (the positioning invariant); a generated node carries
 * Absent. `data` is open, typed annotation — style hints the writer reads, or anything a consumer mints a [[MetaKey]]
 * for. [[MdcNode.unpositioned]] strips the span and keeps the data: position is derived provenance, data is content the
 * author attached.
 */
final case class MdcMeta(
    span: Maybe[Span] = Absent,
    data: Map[MetaKey[?], Any] = Map.empty
) derives CanEqual:

  def get[A](key: MetaKey[A]): Maybe[A] = data.get(key) match
    case Some(value) => Present(value.asInstanceOf[A]) // safe: key identity carries the type
    case None        => Absent

  def updated[A](key: MetaKey[A], value: A): MdcMeta = copy(data = data.updated(key, value))

object MdcMeta:
  val empty: MdcMeta          = MdcMeta()
  def at(span: Span): MdcMeta = MdcMeta(Present(span))
