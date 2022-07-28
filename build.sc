import $file.project.deps, deps.{Deps, ScalaVersions}
import $file.project.modules.dependencyCheck //, dependencyCheck.DependencyCheck
import $file.project.publishing
import $file.project.modules.shared,
shared.{MorphirCrossScalaModule, MorphirScalaModule, MorphirTestModule, MorphirPublishModule}
import $ivy.`io.chris-kipp::mill-github-dependency-graph::0.1.1`
import mill._, scalalib._, scalafmt._

import Deps._

object morphir extends Module {

  /**
   * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code
   * for other Scala versions. We may also directly cross-compile to additional Scla versions.
   */
  val morphirScalaVersion = ScalaVersions.scala3x

  object annotation extends mill.Cross[AnnotationModule](ScalaVersions.all: _*)
  class AnnotationModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    object test extends Tests with MorphirTestModule {}
  }

  object corelib extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def moduleDeps        = Seq(annotation(morphirScalaVersion))
    def morphirPluginJar  = T(mscplugin.assembly())

    override def scalacOptions = T {
      val pluginJarPath = morphirPluginJar().path
      super.scalacOptions() ++ Seq(s"-Xplugin:$pluginJarPath" /*, "--morphir"*/ )
    }

    override def scalacPluginClasspath = T(super.scalacPluginClasspath() ++ Agg(morphirPluginJar()))

    object test extends Tests with MorphirTestModule {}
  }

  object core extends mill.Cross[CoreModule](ScalaVersions.all: _*) {
    object codec extends MorphirScalaModule with MorphirPublishModule {

      def crossScalaVersion = ScalaVersions.scala3x

      def ivyDeps = Agg(io.bullet.`borer-core`(morphirScalaVersion), io.bullet.`borer-derivation`(morphirScalaVersion))
      def moduleDeps = Seq(core(morphirScalaVersion))

      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(core(morphirScalaVersion).test)
      }
    }
  }

  class CoreModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    def ivyDeps = Agg(com.lihaoyi.sourcecode, dev.zio.zio, dev.zio.`zio-prelude`, io.lemonlabs.`scala-uri`)
    object test extends Tests with MorphirTestModule {}
  }

  object knowledge extends mill.Cross[KnowledgeModule](ScalaVersions.all: _*) {}
  class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
    def ivyDeps    = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`)
    def moduleDeps = Seq(core(crossScalaVersion))
    object test extends Tests with MorphirTestModule {}
  }

  object mscplugin extends MorphirScalaModule with MorphirPublishModule { self =>
    def crossScalaVersion     = ScalaVersions.scala3x
    def scalaVersion          = morphirScalaVersion
    def ivyDeps               = self.compilerPluginDependencies(morphirScalaVersion)
    def moduleDeps            = Seq(morphir.core(morphirScalaVersion), morphir.core.codec)
    def crossFullScalaVersion = true

    object test extends Tests with MorphirTestModule {}
    object itest extends Module {
      object basics extends MorphirScalaModule {
        def crossScalaVersion = morphirScalaVersion

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

import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}
