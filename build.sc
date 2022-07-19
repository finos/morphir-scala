import $file.project.deps, deps.{Deps, ScalaVersions}
import $file.project.modules.dependencyCheck //, dependencyCheck.DependencyCheck
import $file.project.publishing
import $file.project.modules.shared, shared.{MorphirCrossScalaModule, MorphirTestModule, MorphirPublishModule}
import $ivy.`io.chris-kipp::mill-github-dependency-graph::0.1.1`
import mill._, scalalib._, scalafmt._
import Deps._

object morphir extends Module {

  object annotation extends mill.Cross[AnnotationModule](ScalaVersions.all: _*)
  class AnnotationModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    object test extends Tests with MorphirTestModule {}
  }

  object knowledge extends mill.Cross[KnowledgeModule](ScalaVersions.all: _*) {}
  class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
    def ivyDeps = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`)
    object test extends Tests with MorphirTestModule {}
  }
}


