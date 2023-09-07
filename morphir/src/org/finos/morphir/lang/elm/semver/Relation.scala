package org.finos.morphir.lang.elm.semver

sealed abstract class Relation(val value: Int)
object Relation {
  case object Superset    extends Relation(0)
  case object Subset      extends Relation(1)
  case object Overlapping extends Relation(2)
  case object Disjoint    extends Relation(3)
  case object Equal       extends Relation(4)
  val values: IndexedSeq[Relation] = IndexedSeq(Superset, Subset, Overlapping, Disjoint, Equal)
}
