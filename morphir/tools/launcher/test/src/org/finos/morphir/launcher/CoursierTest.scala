package org.finos.morphir.launcher

import coursier.Dependency

case class CoursierTest() extends Coursier {
  var fetched: Vector[Dependency]             = Vector.empty
  override def fetch(deps: Dependency*): Unit = fetched ++= deps
}
