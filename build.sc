import $file.project.deps, deps.{Deps, ScalaVersions}
import $file.project.modules.dependencyCheck //, dependencyCheck.DependencyCheck
import $file.project.modules.shared,
shared.{MorphirCrossScalaModule, MorphirScalaModule, MorphirTestModule, MorphirPublishModule}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`

import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._, scalalib._, scalafmt._
import mill.contrib.buildinfo.BuildInfo
import mill.define.Sources
import mill.modules.Jvm
import os.Path

import Deps._

/**
 * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
 * other Scala versions. We may also directly cross-compile to additional Scla versions.
 */
val morphirScalaVersion = ScalaVersions.scala3x
val docsScalaVersion    = ScalaVersions.scala213 //This really should match but need to figure it out

object morphir extends Module {
  val workspaceDir = build.millSourcePath

  object cli extends MorphirScalaModule with BuildInfo /* - Not Ready to publish yet - with MorphirPublishModule*/ {
    def crossScalaVersion    = morphirScalaVersion
    def buildInfoPackageName = Some("org.finos.morphir.cli")
    def buildInfoObjectName  = "MorphirCliBuildInfo"
    def buildInfoMembers = T {
      Map(
        "scalaVersion" -> scalaVersion(),
        "version"      -> VcsVersion.vcsState().format(),
        "product"      -> "morphir",
        "summary"      -> "Morphir CLI",
        "description"  -> packageDescription
      )
    }

    def ivyDeps            = Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-cli`, Deps.dev.zio.`zio-json`)
    def packageDescription = "A command line interface for Morphir"
    object test extends Tests with MorphirTestModule {}
  }

  object corelib extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def moduleDeps        = Seq(interop(crossScalaVersion))
    def morphirPluginJar  = T(mscplugin.assembly())

    override def scalacOptions = T {
      val pluginJarPath = morphirPluginJar().path
      super.scalacOptions() ++ Seq(s"-Xplugin:$pluginJarPath" /*, "--morphir"*/ )
    }

    override def scalacPluginClasspath = T(super.scalacPluginClasspath() ++ Agg(morphirPluginJar()))

    object test extends Tests with MorphirTestModule {}
  }

  object flowz extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def ivyDeps           = Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-json`)
    object test extends Tests with MorphirTestModule {}
  }

  object formats extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def ivyDeps           = Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-json`)
    def moduleDeps        = Seq(core, zio.json)

    object test extends Tests with MorphirTestModule {}

    object core extends MorphirScalaModule with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion
      object test extends Tests with MorphirTestModule {}
    }

    object zio extends Module {
      object json extends MorphirScalaModule with MorphirPublishModule {
        def scalacOptions     = super.scalacOptions() ++ Seq("-Yretain-trees")
        def crossScalaVersion = morphirScalaVersion
        def moduleDeps        = Seq(core, ir)
        def ivyDeps           = Agg(Deps.dev.zio.`zio-json`)
        object test extends Tests with MorphirTestModule {}
      }
    }
  }

  object internal extends Module {
    object codec extends MorphirScalaModule with MorphirPublishModule {

      def crossScalaVersion = morphirScalaVersion

      def ivyDeps = Agg(io.bullet.`borer-core`(morphirScalaVersion), io.bullet.`borer-derivation`(morphirScalaVersion))
      def moduleDeps = Seq(core(morphirScalaVersion))

      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(core(morphirScalaVersion).test)
      }
    }

    object core extends mill.Cross[CoreModule](ScalaVersions.all: _*) {}

    class CoreModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(com.lihaoyi.sourcecode, dev.zio.zio, dev.zio.`zio-prelude`, io.lemonlabs.`scala-uri`)
      object test extends Tests with MorphirTestModule {}
    }

    object util extends MorphirScalaModule with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion
      object test extends Tests with MorphirTestModule {}
    }
  }

  object interop extends mill.Cross[InteropModule](ScalaVersions.all: _*)
  class InteropModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    object test extends Tests with MorphirTestModule {}
  }

  object ir extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def moduleDeps        = Seq(formats.core)
    object test extends Tests with MorphirTestModule {}
  }

  object knowledge extends mill.Cross[KnowledgeModule](ScalaVersions.all: _*) {}
  class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
    def ivyDeps    = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`)
    def moduleDeps = Seq(internal.core(crossScalaVersion))
    object test extends Tests with MorphirTestModule {}
  }

  object mir extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = morphirScalaVersion
    def moduleDeps        = Seq(internal.util)
    def scalacOptions     = super.scalacOptions()
    object test extends Tests with MorphirTestModule
  }

  object mscplugin extends MorphirScalaModule with MorphirPublishModule { self =>
    def crossScalaVersion = ScalaVersions.scala3x
    def scalaVersion      = morphirScalaVersion
    def ivyDeps           = self.compilerPluginDependencies(morphirScalaVersion)
    def moduleDeps =
      Seq(morphir.internal.core(morphirScalaVersion), morphir.internal.codec, morphir.mir, morphir.internal.util)
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

  object site extends Docusaurus2Module with MDocModule {
    override def scalaMdocVersion: T[String] = T("2.3.3")
    override def scalaVersion                = T(docsScalaVersion)
    // MD Sources that must be compiled with Scala MDoc
    override def mdocSources = T.sources(workspaceDir / "docs")
    // MD Sources that are just plain MD files
    override def docusaurusSources = T.sources(workspaceDir / "website")

    override def watchedMDocsDestination: T[Option[Path]] = T(Some(docusaurusBuild().path / "docs"))
    override def compiledMdocs: Sources                   = T.sources(mdoc().path)
  }

  object testing extends Module {
    object compiler extends Module {
      object interface extends JavaModule with MorphirPublishModule {}
    }
  }

  object tools extends MorphirScalaModule with MorphirPublishModule {
    def crossScalaVersion = ScalaVersions.scala3x
    def moduleDeps        = Seq(morphir.ir, morphir.formats.core, morphir.formats.zio.json)
    object test extends Tests with MorphirTestModule {}
  }
}

import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}
