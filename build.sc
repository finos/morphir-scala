import mill.api.Result.Aborted
import mill.api.Result
import mill.api.Result.Failure
import mill.api.Result.Skipped
import mill.api.Result.Success
import $file.project.deps, deps.{Deps, ScalaVersions}
import $file.project.modules.dependencyCheck //, dependencyCheck.DependencyCheck
import $file.project.modules.shared, shared.{
  MorphirCrossScalaModule,
  MorphirScalaJSModule,
  MorphirScalaModule,
  MorphirScalaNativeModule,
  MorphirTestModule,
  MorphirPublishModule
}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import com.github.lolgab.mill.crossplatform._
import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._, scalalib._, scalafmt._
import mill.contrib.buildinfo.BuildInfo
import mill.define.Sources
import mill.modules.Jvm
import mill.scalalib.publish.PublishInfo
import os.Path

import Deps._

/**
 * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
 * other Scala versions. We may also directly cross-compile to additional Scla versions.
 */
val morphirScalaVersion: String = ScalaVersions.scala3x
val docsScalaVersion: String    = ScalaVersions.scala213 //This really should match but need to figure it out

object morphir extends MorphirScalaModule with MorphirPublishModule {
  val crossScalaVersion = morphirScalaVersion
  val workspaceDir      = build.millSourcePath
  def moduleDeps        = Seq(core(crossScalaVersion).jvm, concepts, lang)

  object test extends Tests with MorphirTestModule {
    def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
  }

  object contrib extends Module {
    object knowledge extends mill.Cross[KnowledgeModule](ScalaVersions.all: _*) {}
    class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
      def ivyDeps    = Agg(com.lihaoyi.sourcecode, dev.zio.`zio-streams`)
      def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion))
      object test extends Tests with MorphirTestModule {}
    }
  }

  object concepts extends MorphirScalaModule with MorphirPublishModule {
    val crossScalaVersion = morphirScalaVersion

    def enableNative = crossScalaVersion.startsWith("2.")

    def ivyDeps    = Agg(com.beachape.enumeratum, dev.zio.`zio-parser`, org.typelevel.`paiges-core`)
    def moduleDeps = Seq(core(crossScalaVersion).jvm)

    object test extends Tests with MorphirTestModule {
      def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
    }
  }

  object core extends Cross[CoreModule](ScalaVersions.all: _*)
  class CoreModule(val crossScalaVersion: String) extends CrossPlatform { module =>
    def enableNative = false //crossScalaVersion.startsWith("2.")

    def moduleDeps = Seq(`core-macros`(crossScalaVersion))
    trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {

      def ivyDeps = Agg(
        com.beachape.enumeratum,
        com.lihaoyi.castor,
        com.lihaoyi.pprint,
        com.lihaoyi.`upickle`,
        org.typelevel.`paiges-core`,
      ) ++ (if (crossScalaVersion.startsWith("2."))
              Agg(
                org.`scala-lang`.`scala-reflect`(crossScalaVersion),
                org.`scala-lang`.`scala-compiler`(crossScalaVersion)
              )
            else Agg.empty)

      def scalacOptions = T {
        // val additionalOptions = if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
        val additionalOptions =
          if (crossScalaVersion.startsWith("2.13")) Seq("-language:experimental.macros") else Seq.empty
        super.scalacOptions() ++ additionalOptions
      }

      //def moduleDeps = Seq(macros)

      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
      }
    }

    object jvm extends Shared                           {}
    object js  extends Shared with MorphirScalaJSModule {}

    object native extends Shared with MorphirScalaNativeModule {}
  }

  object `core-macros` extends Cross[CoreMacrosModule](ScalaVersions.all: _*)
  class CoreMacrosModule(val crossScalaVersion:String) extends CrossPlatform {
    def enableNative = false
    trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = T {
        (if (crossScalaVersion.startsWith("2."))
          Agg(
            org.`scala-lang`.`scala-reflect`(crossScalaVersion),
            org.`scala-lang`.`scala-compiler`(crossScalaVersion)
          )
        else Agg.empty)
      }

      def scalacOptions = T {
        // val additionalOptions = if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
        val additionalOptions =
          if (crossScalaVersion.startsWith("2.13")) Seq("-language:experimental.macros") else Seq.empty
        super.scalacOptions() ++ additionalOptions
      }

      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
      }
    }

    object jvm extends Shared
    object js extends Shared with MorphirScalaJSModule
    object native extends Shared with MorphirScalaNativeModule
  }

  object lang extends MorphirScalaModule with MorphirPublishModule {
    val crossScalaVersion = morphirScalaVersion

    def moduleDeps = Seq(core(crossScalaVersion).jvm, concepts, vfile(crossScalaVersion))
    def ivyDeps    = Agg(com.lihaoyi.pprint, dev.zio.`zio-parser`, com.lihaoyi.upickle, org.typelevel.`paiges-core`)

    object test extends Tests with MorphirTestModule {
      def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
    }
  }

  object lib extends Module {
    object core extends MorphirScalaModule with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion
      def moduleDeps        = Seq(interop(crossScalaVersion))
      def morphirPluginJar  = T(morphir.tools.msc.plugin.assembly())

      override def scalacOptions = T {
        val pluginJarPath = morphirPluginJar().path
        super.scalacOptions() ++ Seq(s"-Xplugin:$pluginJarPath" /*, "--morphir"*/ )
      }

      override def scalacPluginClasspath = T(super.scalacPluginClasspath() ++ Agg(morphirPluginJar()))

      object test extends Tests with MorphirTestModule {}
    }

    object interop extends mill.Cross[InteropModule](ScalaVersions.all: _*)
    class InteropModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      object test extends Tests with MorphirTestModule {}
    }
  }

  object site extends Docusaurus2Module with MDocModule {
    override def scalaMdocVersion: T[String] = T("2.3.6")
    override def scalaVersion                = T(docsScalaVersion)
    // MD Sources that must be compiled with Scala MDoc
    override def mdocSources = T.sources(workspaceDir / "docs")
    // MD Sources that are just plain MD files
    override def docusaurusSources = T.sources(workspaceDir / "website")

    override def watchedMDocsDestination: T[Option[Path]] = T(Some(docusaurusBuild().path / "docs"))
    override def compiledMdocs: Sources                   = T.sources(mdoc().path)
    object test extends Tests with MorphirTestModule {}
  }

  object runtime
  class RuntimeModule(val crossScalaVersion: String) extends CrossPlatform { module =>
    def enableNative = false
    def moduleDeps   = Seq(morphir.core(crossScalaVersion))
    trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {
      object test extends Tests with MorphirTestModule {}
    }

    object jvm    extends Shared                               {}
    object js     extends Shared with MorphirScalaJSModule     {}
    object native extends Shared with MorphirScalaNativeModule {}
  }

  object testing extends mill.Cross[TestingModule](ScalaVersions.all: _*) {
    object compiler extends Module {
      object interface extends JavaModule with MorphirPublishModule {
        object test extends Tests with TestModule.Junit4
      }
    }
  }

  class TestingModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    def ivyDeps = Agg(dev.zio.zio, dev.zio.`zio-test`)
    object test extends Tests with MorphirTestModule
  }

  object toolkit extends Module {
    object codec extends MorphirScalaModule with MorphirPublishModule {

      object zio extends Module {
        object json extends mill.Cross[JsonModule](ScalaVersions.all: _*) {}
        class JsonModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
          def ivyDeps    = Agg(dev.zio.`zio-json`)
          def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion))
          object test extends Tests with MorphirTestModule {
            def moduleDeps = super.moduleDeps ++ Seq(
              morphir.testing(crossScalaVersion),
              morphir.toolkit.core.testing(crossScalaVersion)
            )
            def ivyDeps = T(super.ivyDeps() ++ Agg(dev.zio.`zio-json-golden`))
          }
        }
      }

      def crossScalaVersion = morphirScalaVersion

      def ivyDeps = Agg(io.bullet.`borer-core`(morphirScalaVersion), io.bullet.`borer-derivation`(morphirScalaVersion))
      def moduleDeps = Seq(morphir.toolkit.core(morphirScalaVersion))

      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(core(morphirScalaVersion).test)
      }
    }

    object core extends mill.Cross[CoreModule](ScalaVersions.all: _*) {
      object testing extends mill.Cross[TestingModule](ScalaVersions.all: _*)
      class TestingModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
        def ivyDeps    = Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-magnolia`)
        def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion), morphir.testing(crossScalaVersion))
        object test extends Tests with MorphirTestModule {}
      }
    }
    class CoreModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(
        com.lihaoyi.sourcecode,
        dev.zio.zio,
        dev.zio.`zio-prelude`,
        io.lemonlabs.`scala-uri`,
        com.lihaoyi.pprint,
        org.typelevel.`paiges-core`
      )
      def moduleDeps = Seq(morphir.lib.interop(crossScalaVersion))
      object test extends Tests with MorphirTestModule {
        def moduleDeps =
          super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion), morphir.toolkit.core.testing(crossScalaVersion))
      }
    }

    object mir extends MorphirScalaModule with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion
      def moduleDeps        = Seq(morphir.toolkit.util(crossScalaVersion))
      def scalacOptions     = super.scalacOptions()
      object test extends Tests with MorphirTestModule
    }

    object util extends mill.Cross[UtilModule](ScalaVersions.all: _*)

    class UtilModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(dev.zio.`izumi-reflect`)
      object test extends Tests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion))
      }
    }

  }

  object tools extends Module {

    object cli extends MorphirScalaModule with BuildInfo with MorphirPublishModule {
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

      def ivyDeps = Agg(
        dev.zio.zio,
        dev.zio.`zio-cli`,
        dev.zio.`zio-json`,
        dev.zio.`zio-process`
      )
      def packageDescription = "A command line interface for Morphir"
      object test extends Tests with MorphirTestModule {}
    }

    object frontend extends Module {
      object lang extends Module {
        object scala extends MorphirScalaModule with BuildInfo with MorphirPublishModule {
          def crossScalaVersion = morphirScalaVersion

          def buildinfopackagename = Some("org.finos.morphir.frontend.lang")

          def buildInfoObjectName = "ScalaFrontendBuildInfo"

          def buildInfoMembers = T {
            Map(
              "scalaVersion" -> scalaVersion(),
              "version"      -> VcsVersion.vcsState().format(),
              "product"      -> "morphir",
              "summary"      -> "Morphir Frontend - Scala",
              "description"  -> packageDescription
            )
          }

          def ivyDeps = Agg(
            org.`scala-lang`.`scala3-tasty-inspector`(crossScalaVersion)
          )
          def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion))

          object test extends Tests with MorphirTestModule {}
        }
      }
    }

    object launcher extends MorphirScalaModule with BuildInfo with MorphirPublishModule {
      def crossScalaVersion    = ScalaVersions.scala213 // Coursier not available for Scala 3
      def ivyDeps              = Agg(com.lihaoyi.mainargs, com.lihaoyi.`os-lib`, io.`get-coursier`.coursier)
      def buildInfoPackageName = Some("org.finos.morphir.launcher")
      def buildInfoMembers = T {
        val maybeLastTaggedVersion = VcsVersion.vcsState().lastTag.map(_.stripPrefix("v"))
        Map("version" -> maybeLastTaggedVersion.getOrElse("0.0.0"))
      }
      object test extends Tests with MorphirTestModule {}

      // also publish the assembly jar
      override def extraPublish: T[Seq[PublishInfo]] = T {
        Seq(
          PublishInfo(file = assembly(), classifier = Some("assembly"), ivyConfig = "compile")
        )
      }
    }

    object msc extends Module {
      object plugin extends MorphirScalaModule with MorphirPublishModule { self =>
        def crossScalaVersion = ScalaVersions.scala3x
        def scalaVersion      = morphirScalaVersion
        def ivyDeps           = self.compilerPluginDependencies(morphirScalaVersion)
        def moduleDeps =
          Seq(
            morphir.toolkit.core(morphirScalaVersion),
            morphir.toolkit.codec,
            morphir.toolkit.mir,
            morphir.toolkit.util(crossScalaVersion)
          )
        def crossFullScalaVersion = true

        object test extends Tests with MorphirTestModule {}
        object itest extends Module {
          object basics extends MorphirScalaModule {
            def crossScalaVersion = morphirScalaVersion
            def moduleDeps        = Seq(morphir.lib.interop(crossScalaVersion))
            def morphirPluginJar  = T(morphir.tools.msc.plugin.assembly())

            override def scalacOptions = T {
              val pluginJarPath = morphirPluginJar().path
              super.scalacOptions() ++ Seq(s"-Xplugin:$pluginJarPath" /*, "--morphir"*/ )
            }

            override def scalacPluginClasspath = T(super.scalacPluginClasspath() ++ Agg(morphirPluginJar()))
            // def scalacPluginIvyDeps = T {
            //   // TODO: try lefou's suggestion to
            //   // "... but you could instead just override the scalacPluginClasspath and add the morphir.tools.msc.plugin.jar directly"
            //   val _                    = morphir.tools.msc.plugin.publishLocal()()
            //   val morphirPluginVersion = morphir.tools.msc.plugin.publishVersion()
            //   Agg(ivy"org.finos.morphir:::morphir-morphir.tools.msc.plugin:$morphirPluginVersion")
            // }
            object test extends Tests with MorphirTestModule {}
          }
        }
      }
    }
  }

  object vfile extends mill.Cross[VFileModule](ScalaVersions.all: _*)

  class VFileModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    def ivyDeps = Agg(
      com.lihaoyi.sourcecode,
      com.lihaoyi.geny,
      com.lihaoyi.pprint,
      org.typelevel.`paiges-core`
    )

    def moduleDeps = Seq(morphir.toolkit.util(crossScalaVersion))

    object test extends Tests with MorphirTestModule {
      def moduleDeps = super.moduleDeps ++ Seq(
        morphir.testing(crossScalaVersion)
      )
    }
  }
}

import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}

def scalaBuild(evaluator: Evaluator, scalaVersionFilter: String, target: String) = T.command {
  import mill.util.{PrintLogger, Watched}
  import mill.define.SelectMode
  import ujson.Value
  val newTargets = mill.main.MainModule
    .evaluateTasksNamed(
      evaluator.withBaseLogger(
        // When using `show`, redirect all stdout of the evaluated tasks so the
        // printed JSON is the only thing printed to stdout.
        evaluator.baseLogger match {
          case PrintLogger(c1, d, c2, c3, _, i, e, in, de, uc) =>
            PrintLogger(c1, d, c2, c3, e, i, e, in, de, uc)
          case l => l
        }
      ),
      Seq("__.scalaVersion"),
      SelectMode.Separated
    ) { res: Seq[(Any, Option[(String, ujson.Value)])] =>
      val nameAndJson         = res.flatMap(_._2)
      val output: ujson.Value = ujson.Obj.from(nameAndJson)
      val res0 = output.obj.collect {
        case (taskName, ujson.Str(scalaVersion)) if scalaVersion.startsWith(scalaVersionFilter) =>
          taskName.replace(".scalaVersion", s".$target")
      }.toList
      // T.log.outputStream.println(res0)
      res0
    }
    .map { res: Watched[Option[List[String]]] =>
      res.value.getOrElse(List.empty)
    }

  val finalResult = newTargets.asSuccess match {
    case Some(Result.Success(Nil)) =>
      Result.Failure("No targets found where scalaVersion starts with " + scalaVersionFilter)
    case Some(Result.Success(targets)) =>
      // T.log.outputStream.println(targets)
      mill.main.MainModule.evaluateTasksNamed(evaluator, targets, SelectMode.Multi) {
        res: Seq[(Any, Option[(String, ujson.Value)])] =>
          val nameAndJson         = res.flatMap(_._2)
          val output: ujson.Value = ujson.Obj.from(nameAndJson)
          // T.log.outputStream.println(output)
          output
      }
    case None => Result.Failure("Failed to get targets")
  }
  finalResult
}
