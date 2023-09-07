package org.finos.morphir.lang.elm.semver

final case class Constraint(set: Set[Range])

object Constraint {
  val empty: Constraint = Constraint(Set.empty)
}
