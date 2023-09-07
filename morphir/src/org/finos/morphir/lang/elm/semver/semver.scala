package org.finos.morphir.lang.elm.semver
import enumeratum.values._
import zio.Chunk
import zio.prelude._

sealed abstract class Strictness(val value: Int) extends IntEnumEntry

object Strictness extends IntEnum[Strictness] {
  case object Exact  extends Strictness(0)
  case object Safe   extends Strictness(1)
  case object Unsafe extends Strictness(2)
  val values: IndexedSeq[Strictness] = findValues
}

sealed abstract class Relation(val value: Int) extends IntEnumEntry
object Relation extends IntEnum[Relation] {
  case object Superset    extends Relation(0)
  case object Subset      extends Relation(1)
  case object Overlapping extends Relation(2)
  case object Disjoint    extends Relation(3)
  case object Equal       extends Relation(4)
  val values: IndexedSeq[Relation] = findValues
}

sealed trait Interval extends Product with Serializable { self => }
object Interval {
  final case class Closed(version: Version) extends Interval
  final case class Open(version: Version)   extends Interval
  case object Unbounded                     extends Interval
}

final case class Range(lower: Interval, upper: Interval)
object Range {
  def apply(version: Version): Range =
    Range(Interval.Closed(version), Interval.Closed(version))
}

final case class Constraint(set: Set[Range])
object Constraint {
  val empty: Constraint = Constraint(Set.empty)
}
