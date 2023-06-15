import $meta._
import $ivy.`io.chris-kipp::mill-ci-release::0.1.9`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $file.project.deps, deps.{Deps, ScalaVersions, Versions => Vers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import io.kipp.mill.ci.release.CiReleaseModule
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

trait CommonCoursierModule extends CoursierModule {
  override def mapDependencies: Task[coursier.Dependency => coursier.Dependency] = T.task {
    super.mapDependencies().andThen { dep =>
      forcedVersions
        .find(t => t._1 == dep.module.organization.value && t._2 == dep.module.name.value)
        .map { forced =>
          val newDep = dep.withVersion(forced._3)
          T.log.debug(s"Mapping ${dep} to ${newDep}")
          newDep
        }
        .getOrElse(dep)
    }
  }

  val forcedVersions = Seq(
    ("org.apache.ant", "ant", "1.10.12"),
    ("commons-io", "commons-io", "2.11.0"),
    ("com.google.code.gson", "gson", "2.9.0"),
    ("com.google.protobuf", "protobuf-java", "3.21.2"),
    ("com.google.guava", "guava", "31.1-jre"),
    ("org.jsoup", "jsoup", "1.15.3"),
    ("junit", "junit", "4.13.1")
  )
}

object morphir extends Cross[MorphirModule](ScalaVersions.all)
trait MorphirModule extends Cross.Module[String] {
  val workspaceDir = build.millSourcePath

  trait MorphirCommonModule extends CrossPlatformScalaModule with CrossValue {

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

  object testing extends Module {
    object jvm extends MorphirJVMModule {
    }
    object js extends MorphirJSModule {

    }
    object native extends MorphirNativeModule

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

}
