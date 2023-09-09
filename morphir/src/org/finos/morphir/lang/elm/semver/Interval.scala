package org.finos.morphir.lang.elm.semver

sealed trait Interval extends Product with Serializable { self => }
object Interval {
  final case class Closed(version: Version) extends Interval
  final case class Open(version: Version)   extends Interval
  case object Unbounded                     extends Interval
}
