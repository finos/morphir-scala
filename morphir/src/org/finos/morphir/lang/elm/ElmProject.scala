package org.finos.morphir.lang.elm
import semver.Version

sealed trait ElmProject extends Product with Serializable
object ElmProject {
  final case class Application(sourceDirectories: Vector[String], elmVersion: Version) extends ElmProject
  final case class Package()                                                           extends ElmProject
}

final case class AppDependencies(direct: Map[String, Version], indirect: Map[String, Version])




