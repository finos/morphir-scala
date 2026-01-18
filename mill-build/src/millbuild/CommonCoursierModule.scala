package millbuild

import mill.*, scalalib.*, scalafmt.*

trait CommonCoursierModule extends CoursierModule {
  override def mapDependencies: Task[coursier.Dependency => coursier.Dependency] = Task.Anon {
    super.mapDependencies().andThen { dep =>
      Task.log.debug("****************************[START: Forced Versions]*****************************")
      val result = forcedVersions
        .find(t => t._1 == dep.module.organization.value && t._2 == dep.module.name.value)
        .map { forced =>
          @annotation.nowarn("msg=deprecated")
          val newDep = dep.withVersion(forced._3)
          Task.log.debug(s"Mapping ${dep} to ${newDep}")
          newDep
        }
        .getOrElse(dep)
      Task.log.debug("****************************[END: Forced Versions]*****************************")
      result
    }
  }

  val forcedVersions = Seq(
    ("org.apache.ant", "ant", "1.10.12"),
    ("commons-io", "commons-io", "2.11.0"),
    ("com.google.code.gson", "gson", "2.9.0"),
    ("com.google.protobuf", "protobuf-java", "3.21.2"),
    ("com.google.guava", "guava", "31.1-jre"),
    ("com.fasterxml.jackson.core", "jackson-core", "2.17.2"),
    ("com.fasterxml.jackson.core", "jackson-databind", "2.17.2"),
    ("org.jsoup", "jsoup", "1.15.3"),
    ("org.scala-lang", "scala3-library_3", "3.7.4"),
    ("org.scala-lang", "scala-library", "2.13.16")
  )
}
