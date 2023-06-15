import $meta._
import $ivy.`io.chris-kipp::mill-ci-release::0.1.9`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $file.project.deps, deps.{Deps, ScalaVersions, Versions => Vers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import io.kipp.mill.ci.release.CiReleaseModule
import millbuild._
import millbuild.crossplatform._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._


import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}

trait MorphirPublishModule extends CiReleaseModule with JavaModule {
  import mill.scalalib.publish._
  def packageDescription: String = s"The $artifactName package"

  def pomSettings = PomSettings(
    description = packageDescription,
    organization = "org.finos.morphir",
    url = "https://github.com/finos/morphir-scala",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("finos", "morphir-scala"),
    developers = Seq(
      Developer("DamianReeves", "Damian Reeves", "https://github.com/damianreeves")
    )
  )
}

object morphir extends Cross[MorphirModule](ScalaVersions.all)
trait MorphirModule extends Cross.Module[String] with CrossPlatform { morphir =>
  val workspaceDir = millbuild.build.millSourcePath

  trait MorphirCommonModule extends CrossPlatformScalaModule with CrossValue with CommonScalaModule {
    def semanticDbVersion       = T.input(Vers.semanticDb(partialVersion()))
    def compilerPluginDependencies(selectedScalaVersion: String) =
      Agg.when(selectedScalaVersion.startsWith("3.")) {
        Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
      } 
  }

  trait MorphirJVMModule extends MorphirCommonModule {
    def platform = Platform.JVM
  }

  trait MorphirJSModule extends MorphirCommonModule with ScalaJSModule {
    def platform = Platform.JS
    def scalaJSVersion = ScalaVersions.scalaJSVersion 
  }

  trait MorphirNativeModule extends MorphirCommonModule with ScalaNativeModule {
    def platform = Platform.Native
    def scalaNativeVersion = ScalaVersions.scalaNativeVersion
  }

  object datamodel extends Module {
    object jvm extends MorphirJVMModule with MorphirPublishModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }
    object js extends MorphirJSModule with MorphirPublishModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }
    object native extends MorphirNativeModule with MorphirPublishModule {
      object test extends ScalaTests with TestModule.Munit {
        def ivyDeps = Agg(Deps.org.scalameta.munit)
      }
    }

    object json extends Module {
      object zio extends Module {
        object jvm extends MorphirJVMModule with MorphirPublishModule {

          def ivyDeps = Agg(Deps.dev.zio.`zio-json`)
          def moduleDeps = Seq(datamodel.jvm)

          object test extends ScalaTests with TestModule.Munit {
            def ivyDeps: T[Agg[Dep]] = Agg(
              Deps.org.scalameta.munit, 
              Deps.org.scalameta.`munit-scalacheck`
            )
          }
        }

        object js extends MorphirJSModule with MorphirPublishModule {

          def ivyDeps = Agg(Deps.dev.zio.`zio-json`)
          def moduleDeps = Seq(datamodel.js)

          object test extends ScalaTests with TestModule.Munit {
            def ivyDeps: T[Agg[Dep]] = Agg(
              Deps.org.scalameta.munit, 
              Deps.org.scalameta.`munit-scalacheck`
            )
          }
        }
      }
    }
  }

  object lib extends Module {
    object jvm extends MorphirJVMModule {

    }

    object interop extends Module {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        object test extends ScalaTests with TestModule.Munit {
          def ivyDeps = Agg(Deps.org.scalameta.munit)
        }       
      }
      object jvm extends Shared with MorphirJVMModule {

      }
      object js extends Shared with MorphirJSModule {

      }
      object native extends Shared with  MorphirNativeModule {

      }
    }
  }

  object testing extends Module {
    trait Shared extends MorphirCommonModule with MorphirPublishModule {
       def ivyDeps = T {
        Agg(Deps.co.fs2.`fs2-io`, Deps.com.lihaoyi.sourcecode, Deps.dev.zio.zio,  Deps.dev.zio.`zio-test`) ++ Agg.when(platform.isNotNative)(Deps.dev.zio.`zio-json`)
       }
    }


    object jvm extends Shared with MorphirJVMModule {
      
    }
    object js extends Shared with MorphirJSModule {

    }
    object native extends Shared with MorphirNativeModule

    object munit extends Module {
      object jvm extends MorphirJVMModule
      object js extends MorphirJSModule
      object native extends MorphirNativeModule
    }

    object zio extends Module {
      object jvm extends MorphirJVMModule
      object js extends MorphirJSModule
      object native extends MorphirNativeModule
    }
  }

  object toolkit extends Module {
    object core extends Module {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = Agg(
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.zio,
          Deps.dev.zio.`zio-prelude`,
          Deps.io.lemonlabs.`scala-uri`,
          Deps.com.lihaoyi.pprint,
          Deps.org.typelevel.`paiges-core`
        )
      }
      object jvm extends Shared with MorphirJVMModule {
         def moduleDeps = Seq(datamodel.jvm, lib.interop.jvm)
      }
      object js extends Shared with MorphirJSModule {
         def moduleDeps = Seq(datamodel.js, lib.interop.js)
      }
    }

    object util extends Module {
      trait Shared extends MorphirCommonModule with MorphirPublishModule {
        def ivyDeps = Agg(Deps.dev.zio.`izumi-reflect`)
      }
      object jvm extends Shared with MorphirJVMModule with MorphirPublishModule
    }
  }

  object vfile extends Module {
    trait Shared extends MorphirCommonModule {
      def ivyDeps = Agg(
        Deps.com.lihaoyi.sourcecode,
        Deps.com.lihaoyi.geny,
        Deps.com.lihaoyi.pprint,
        Deps.org.typelevel.`paiges-core`
      )
    }

    object jvm extends Shared with MorphirJVMModule with MorphirPublishModule {
      def moduleDeps = Seq(morphir.toolkit.util.jvm)

    }  
  }

}
