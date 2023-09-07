package org.finos.morphir.lang.elm.semver

final case class Range(lower: Interval, upper: Interval)
object Range {
  def apply(version: Version): Range =
    Range(Interval.Closed(version), Interval.Closed(version))
}
