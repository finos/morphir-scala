package org.finos.morphir.lang.elm.semver
import enumeratum.values._
import zio.Chunk
import zio.prelude._

sealed abstract class Strictness(val value: Int) extends IntEnumEntry

object Strictness extends IntEnum[Strictness] {
  case object Exact  extends Strictness(0)
  case object Safe   extends Strictness(1)
  case object Unsafe extends Strictness(2)
  val values = findValues
}

sealed abstract class Relation(val value: Int) extends IntEnumEntry
object Relation {
  case object Superset    extends Relation(0)
  case object Subset      extends Relation(1)
  case object Overlapping extends Relation(2)
  case object Disjoint    extends Relation(3)
  case object Equal       extends Relation(4)
}

final case class Version(major: Long, minor: Long, patch: Long) { self =>
  override def toString: String = s"$major.$minor.$patch"
}

object Version {
  def fromString(version: String): Validation[String, Version] = {
    val parts = version.split('.').zipWithIndex.collect { case (part, idx) =>
      part.toLongOption match {
        case None        => Validation.fail(s"Invalid version part($idx): $part")
        case Some(value) => Validation.succeed(value)
      }
    }

    Validation.validateAll(Chunk.fromIterable(parts)).flatMap {
      case Chunk(major, minor, patch) =>
        Validation.succeed(Version(major, minor, patch))
      case Chunk(major, minor) =>
        Validation.succeed(Version(major, minor, 0))
      case Chunk(major) =>
        Validation.succeed(Version(major, 0, 0))
      case _ => Validation.fail(s"Invalid version: $version")
    }
  }
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
