import $file.project.deps, deps.{Deps, ScalaVersions}
import $file.project.modules.dependencyCheck //, dependencyCheck.DependencyCheck
import $file.project.publishing
import $file.project.modules.shared,
shared.{MorphirCrossScalaModule, MorphirScalaModule, MorphirTestModule, MorphirPublishModule}
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
    def ivyDeps    = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`)
    def moduleDeps = Seq(ld(crossScalaVersion))
    object test extends Tests with MorphirTestModule {}
  }

  object ld extends mill.Cross[LdModule](ScalaVersions.all: _*) {}
  class LdModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
    def ivyDeps = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`, io.lemonlabs.`scala-uri`)
    object test extends Tests with MorphirTestModule {}
  }

  object mscplugin extends MorphirScalaModule with MorphirPublishModule { self =>
    private val selectedScalaVersion = ScalaVersions.scala3x
    def scalaVersion                 = selectedScalaVersion
    def ivyDeps                      = self.compilerPluginDependencies(selectedScalaVersion)
    def crossFullScalaVersion        = true

    object test extends Tests with MorphirTestModule {}
    object itest extends Module {
      object basics extends MorphirScalaModule {
        def scalaVersion = selectedScalaVersion

        def morphirPluginJar = T.input(mscplugin.jar())

        override def scalacOptions = T {
          val pluginJarPath = morphirPluginJar().path
          super.scalacOptions() ++ Seq(s"-Xplugin:$pluginJarPath")
        }

        override def scalacPluginClasspath = T(super.scalacPluginClasspath() ++ Agg(morphirPluginJar()))
        // def scalacPluginIvyDeps = T {
        //   // TODO: try lefou's suggestion to
        //   // "... but you could instead just override the scalacPluginClasspath and add the mscplugin.jar directly"
        //   val _                    = mscplugin.publishLocal()()
        //   val morphirPluginVersion = mscplugin.publishVersion()
        //   Agg(ivy"org.finos.morphir:::morphir-mscplugin:$morphirPluginVersion")
        // }
      }
    }
  }
}
