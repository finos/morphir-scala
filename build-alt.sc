import $ivy.`io.chris-kipp::mill-ci-release::0.1.9`
import $ivy.`com.github.lolgab::mill-crossplatform::0.2.3`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $file.project.deps, deps.{Deps, ScalaVersions, Versions => Vers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import io.kipp.mill.ci.release.CiReleaseModule
import mill.api.Result._
import mill.api.Result
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._
import com.github.lolgab.mill.crossplatform._
import de.tobiasroeser.mill.vcs.version.VcsVersion
import mill._, scalalib._, scalafmt._
import mill.contrib.buildinfo.BuildInfo
import mill.define.Sources
import mill.modules.Jvm
import mill.scalalib.publish.PublishInfo
import os.Path
import java.util.Properties

import Deps._

/**
 * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
 * other Scala versions. We may also directly cross-compile to additional Scla versions.
 */
val morphirScalaVersion: String = ScalaVersions.scala3x
val docsScalaVersion: String    = ScalaVersions.scala213 //This really should match but need to figure it out

object morphir extends Cross[MorphirModule](ScalaVersions.all)
trait MorphirModule extends CrossModuleBase { morphirModule =>
  val workspaceDir = build.millSourcePath

  object contrib extends Module {
    object knowledge extends mill.Cross[KnowledgeModule](ScalaVersions.all) {}
    class KnowledgeModule(val crossScalaVersion: String) extends MorphirCrossScalaModule {
      def ivyDeps    = Agg(Deps.com.lihaoyi.sourcecode, Deps.dev.zio.`zio-streams`)
      def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion))
      object test extends ScalaTests with MorphirTestModule {}
    }
  }

  object core extends Cross[CoreModule](ScalaVersions.all: _*)
  class CoreModule(val crossScalaVersion: String) extends CrossPlatform { module =>
    def enableNative = false // crossScalaVersion.startsWith("2.")
    def enableJS     = crossScalaVersion.startsWith("3.")

    def moduleDeps = Seq(`core-macros`(crossScalaVersion))
    trait Shared extends MorphirCrossScalaModule with CrossPlatformCrossScalaModule with MorphirPublishModule {

      def ivyDeps = Agg(
        Deps.com.beachape.enumeratum,
        Deps.com.lihaoyi.castor,
        Deps.com.lihaoyi.pprint,
        Deps.com.lihaoyi.`upickle`,
        Deps.com.outr.scribe,
        Deps.org.typelevel.`paiges-core`
      )

      def compileIvyDeps = super.compileIvyDeps() ++ (if (crossScalaVersion.startsWith("2."))
                                                        Agg(
                                                          Deps.org.`scala-lang`.`scala-reflect`(crossScalaVersion),
                                                          Deps.org.`scala-lang`.`scala-compiler`(crossScalaVersion)
                                                        )
                                                      else Agg.empty)

      def scalacOptions = T {
        // val additionalOptions = if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
        val additionalOptions =
          if (crossScalaVersion.startsWith("2.13")) Seq("-language:experimental.macros") else Seq.empty
        super.scalacOptions() ++ additionalOptions
      }

      // def moduleDeps = Seq(macros)

    }

    object jvm extends Shared {
      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).jvm)
      }
    }
    object js extends Shared with MorphirScalaJSModule {

      object test extends ScalaJSTests with MorphirTestModule {
        // def scalacOptions = super.scalacOptions() ++ outer.scalacOptions()
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).js)
      }
    }

    object native extends Shared with MorphirScalaNativeModule {}
  }

  object `core-macros` extends Cross[CoreMacrosModule](ScalaVersions.all: _*)
  class CoreMacrosModule(val crossScalaVersion: String) extends CrossPlatform {
    def enableNative = false
    def enableJS     = crossScalaVersion.startsWith("3.")
    trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = super.ivyDeps() ++ {
        (if (crossScalaVersion.startsWith("2."))
           Agg(
             Deps.org.`scala-lang`.`scala-reflect`(crossScalaVersion),
             Deps.org.`scala-lang`.`scala-compiler`(crossScalaVersion)
           )
         else Agg.empty)
      }

      def scalacOptions = T {
        // val additionalOptions = if (crossScalaVersion.startsWith("2.13")) Seq("-Ymacro-annotations") else Seq.empty
        val additionalOptions =
          if (crossScalaVersion.startsWith("2.13")) Seq("-language:experimental.macros") else Seq.empty
        super.scalacOptions() ++ additionalOptions
      }
    }

    object jvm extends Shared {
      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).jvm)
      }
    }
    object js extends Shared with MorphirScalaJSModule {

      object test extends SclaJSTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).js)
      }
    }
    object native extends Shared with MorphirScalaNativeModule {

      object test extends ScalaNativeTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).native)
      }
    }
  }

  object datamodel extends MorphirCrossPlatform { module =>
    trait Shared extends MorphirCrossPlatformScalaModule with MorphirPublishModule {}
    object jvm extends Shared {
      object test extends ScalaTests with TestModule.Munit {
        override def ivyDeps: T[Agg[Dep]] = Agg(Deps.org.scalameta.munit)
      }
    }
    object js extends Shared with MorphirScalaJSModule {

      override def ivyDeps: T[Agg[Dep]] = Agg(Deps.org.scalameta.munit)
    }
    object native extends Shared with MorphirScalaNativeModule {

      override def ivyDeps: T[Agg[Dep]] = Agg(Deps.org.scalameta.munit)
    }

    object json extends Module {
      object zio extends MorphirCrossPlatform {
        trait Shared extends MorphirCrossPlatformScalaModule with MorphirPublishModule
        object jvm   extends Shared
      }
    }
  }

  object `datamodel-json` extends Module {
    object zio extends Cross[DatamodelJsonZioModule](ScalaVersions.all: _*)
    case class DatamodelJsonZioModule(val crossScalaVersion: String) extends CrossPlatform { module =>
      def enableNative = false
      def moduleDeps   = Seq(morphir.datamodel(crossScalaVersion))

      trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {
        def ivyDeps = Agg(Deps.dev.zio.`zio-json`)

      }
      object jvm extends Shared {
        object test extends ScalaTests with TestModule.Munit {
          override def ivyDeps: T[Agg[Dep]] = Agg(org.scalameta.munit, org.scalameta.`munit-scalacheck`)
        }
      }

      object js extends Shared with MorphirScalaJSModule {

        object test extends ScalaJSTests with TestModule.Munit {
          override def ivyDeps: T[Agg[Dep]] = Agg(org.scalameta.munit, org.scalameta.`munit-scalacheck`)
        }
      }

      object native extends Shared with MorphirScalaNativeModule {
        object test extends ScalaNativeTests with TestModule.Munit {
          override def ivyDeps: T[Agg[Dep]] = Agg(org.scalameta.munit, org.scalameta.`munit-scalacheck`)
        }
      }
    }
  }

  object lib extends Module {
    object core extends MorphirScalaModule with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion
      def moduleDeps        = Seq(interop(crossScalaVersion))
      object test extends ScalaTests with MorphirTestModule {}
    }

    object interop extends mill.Cross[InteropModule](ScalaVersions.all: _*)
    class InteropModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      object test extends ScalaTests with MorphirTestModule {}
    }
  }

  object site extends Docusaurus2Module with MDocModule {
    override def scalaMdocVersion: T[String] = T("2.3.7")
    override def scalaVersion                = T(docsScalaVersion)
    // MD Sources that must be compiled with Scala MDoc
    override def mdocSources = T.sources(workspaceDir / "docs")
    // MD Sources that are just plain MD files
    override def docusaurusSources = T.sources(workspaceDir / "website")

    override def watchedMDocsDestination: T[Option[Path]] = T(Some(docusaurusBuild().path / "docs"))
    override def compiledMdocs: Sources                   = T.sources(mdoc().path)
    object test extends ScalaTests with MorphirTestModule {}
  }

  object runtime extends Cross[RuntimeModule](ScalaVersions.all: _*)
  class RuntimeModule(val crossScalaVersion: String) extends CrossPlatform { module =>
    def enableNative = false
    def enableJS     = crossScalaVersion.startsWith("3.")
    def moduleDeps   = Seq(morphir.core(crossScalaVersion))
    trait Shared extends CrossPlatformCrossScalaModule with MorphirCrossScalaModule with MorphirPublishModule {}

    object jvm extends Shared {
      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).jvm)
      }
    }
    object js extends Shared with MorphirScalaJSModule { outer =>
      object test extends ScalaJSTests with MorphirTestModule {
        def scalacOptions = super.scalacOptions() ++ outer.scalacOptions()
        def moduleDeps    = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).js)
      }
    }
    object native extends Shared with MorphirScalaNativeModule {
      object test extends ScalaNativeTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).native)
      }
    }
  }

  // TODO: Make testing.shared, testing.zio, testing.munit to help with transition towards munit
  object testing extends MorphirCrossPlatform {
    object compiler extends Module {
      object interface extends JavaModule with MorphirPublishModule {
        object test extends JavaModuleTests with TestModule.Junit4
      }
    }

    def enableNative = crossValue.startsWith("3.")
    trait Shared extends MorphirCrossPlatformScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(Deps.co.fs2.`fs2-io`, Deps.com.lihaoyi.sourcecode, Deps.dev.zio.zio, Deps.dev.zio.`zio-test`)
    }

    object jvm extends Shared {

      object test extends ScalaTests with MorphirTestModule {
        // def moduleDeps = super.moduleDeps ++ Seq(testing.jvm)
        def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-json`)
      }
    }

    object js extends Shared with MorphirScalaJSModule {

      object test extends ScalaJSTests with MorphirTestModule {
        // def moduleDeps = super.moduleDeps ++ Seq(testing.js)
        def ivyDeps = super.ivyDeps() ++ Agg(Deps.dev.zio.`zio-json`)
      }
    }
    object native extends Shared with MorphirScalaNativeModule {

      object test extends ScalaNativeTests with MorphirTestModule {
        // def moduleDeps = super.moduleDeps ++ Seq(testing.native)
      }
    }
  }

  object toolkit extends Module {
    object codec extends MorphirScalaModule with MorphirPublishModule {

      object zio extends Module {
        object json extends mill.Cross[JsonModule](ScalaVersions.all: _*) {}
        class JsonModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
          def ivyDeps    = Agg(dev.zio.`zio-json`)
          def moduleDeps = Seq(morphir.toolkit.core(crossScalaVersion))
          object test extends ScalaTests with MorphirTestModule {
            def moduleDeps = super.moduleDeps ++ Seq(
              morphir.testing(crossScalaVersion).jvm,
              morphir.toolkit.core.testing(crossScalaVersion)
            )
            def ivyDeps = T(
              super.ivyDeps() ++ Agg(
                dev.zio.`zio-json-golden`,
                ivy"io.github.deblockt:json-diff:0.0.6",
                dev.zio.`zio-process`
              )
            )
          }
        }
      }

      def crossScalaVersion = morphirScalaVersion

      def ivyDeps =
        Agg(Deps.io.bullet.`borer-core`(morphirScalaVersion), Deps.io.bullet.`borer-derivation`(morphirScalaVersion))
      def moduleDeps = Seq(morphir.toolkit.core(morphirScalaVersion))

      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(core(morphirScalaVersion).test)
      }
    }

    object core extends mill.Cross[CoreModule](ScalaVersions.all: _*) {
      object testing extends mill.Cross[TestingModule](ScalaVersions.all: _*)
      class TestingModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
        def ivyDeps = Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-magnolia`)
        def moduleDeps = Seq(
          morphir.datamodel(crossScalaVersion).jvm,
          morphir.toolkit.core(crossScalaVersion),
          morphir.testing(crossScalaVersion).jvm
        )
        object test extends ScalaTests with MorphirTestModule {}
      }
    }
    class CoreModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(
        Deps.com.lihaoyi.sourcecode,
        Deps.dev.zio.zio,
        Deps.dev.zio.`zio-prelude`,
        Deps.io.lemonlabs.`scala-uri`,
        Deps.com.lihaoyi.pprint,
        Deps.org.typelevel.`paiges-core`
      )
      def moduleDeps = Seq(morphir.datamodel(crossScalaVersion).jvm, morphir.lib.interop(crossScalaVersion))
      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps =
          super.moduleDeps ++ Seq(
            morphir.testing(crossScalaVersion).jvm,
            morphir.toolkit.core.testing(crossScalaVersion)
          )
      }
    }

    object util extends mill.Cross[UtilModule](ScalaVersions.all: _*)

    class UtilModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
      def ivyDeps = Agg(dev.zio.`izumi-reflect`)
      object test extends ScalaTests with MorphirTestModule {
        def moduleDeps = super.moduleDeps ++ Seq(morphir.testing(crossScalaVersion).jvm)
      }
    }

  }

  object tools extends Module {

    object cli extends MorphirScalaModule with BuildInfo with MorphirPublishModule {
      def crossScalaVersion = morphirScalaVersion

      def buildInfoPackageName = Some("org.finos.morphir.cli")

      def buildInfoObjectName = "MorphirCliBuildInfo"

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

      object test extends ScalaTests with MorphirTestModule {}
    }

    object launcher extends MorphirScalaModule with BuildInfo with MorphirPublishModule {
      def crossScalaVersion = ScalaVersions.scala213 // Coursier not available for Scala 3

      def ivyDeps = Agg(Deps.com.lihaoyi.mainargs, Deps.com.lihaoyi.`os-lib`, Deps.io.`get-coursier`.coursier)

      def buildInfoPackageName = Some("org.finos.morphir.launcher")

      def buildInfoMembers = T {
        val maybeLastTaggedVersion = VcsVersion.vcsState().lastTag.map(_.stripPrefix("v"))
        Map("version" -> maybeLastTaggedVersion.getOrElse("0.0.0"))
      }

      object test extends ScalaTests with MorphirTestModule {}

      // also publish the assembly jar
      override def extraPublish: T[Seq[PublishInfo]] = T {
        Seq(
          PublishInfo(file = assembly(), classifier = Some("assembly"), ivyConfig = "compile")
        )
      }
    }
  }

  object vfile extends mill.Cross[VFileModule](ScalaVersions.all: _*)

  class VFileModule(val crossScalaVersion: String) extends MorphirCrossScalaModule with MorphirPublishModule {
    def ivyDeps = Agg(
      Deps.com.lihaoyi.sourcecode,
      Deps.com.lihaoyi.geny,
      Deps.com.lihaoyi.pprint,
      Deps.org.typelevel.`paiges-core`
    )

    def moduleDeps = Seq(morphir.toolkit.util(crossScalaVersion))

    object test extends ScalaTests with MorphirTestModule {
      def moduleDeps = super.moduleDeps ++ Seq(
        morphir.testing(crossScalaVersion).jvm
      )
    }
  }

  trait MorphirCrossPlatform extends CrossPlatform { morphirCrossPlatform =>
    override def crossValue = morphirModule.crossValue
    trait MorphirCrossPlatformScalaModule extends CrossPlatformScalaModule with MorphirScalaModule {
      override def crossValue = morphirCrossPlatform.crossValue
    }
  }

  // trait MorphirCrossScalaModule extends CommonCrossModule

  trait MorphirScalaModule extends CommonScalaModule with CrossScalaModule {}

  // trait MorphirScalaTestModule extends CommonTestModule with CommonScalaModule {}

  trait MorphirTestModule extends CommonTestModule {}

  // trait CommonCrossModule extends CrossModuleBase with CommonScalaModule {
  //   //override def scalaVersion: T[String] = T(crossScalaVersion)
  // }

  trait CommonScalaModule extends CrossScalaModule with CommonCoursierModule with ScalafmtModule { self =>
    def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))

    def partialVersion(version: String): Option[(Int, Int)] = {
      val partial = version.split('.').take(2)
      for {
        major    <- partial.headOption
        majorInt <- major.toIntOption
        minor    <- partial.lastOption
        minorInt <- minor.toIntOption
      } yield (majorInt, minorInt)
    }

    def partialVersion: T[Option[(Int, Int)]] = T {
      partialVersion(scalaVersion())
    }

    def optimize: T[Boolean] = T(false)
    def scalacOptions: Target[Seq[String]] = T {
      val options = scalacOptions(scalaVersion(), optimize())
      super.scalacOptions() ++ options ++ additionalScalacOptions()
    }

    def userBuildProperties = T.source(T.workspace / "build.user.properties")

    def additionalScalacOptions = T {
      val propsPath = userBuildProperties().path
      if (os.exists(propsPath)) {
        try {
          val is = os.read.inputStream(propsPath)
          try {
            val props = new java.util.Properties()
            props.load(is)
            getAdditionalScalacOptions(props, partialVersion())
          } finally is.close()
        } catch {
          case e: Throwable =>
            println(s"Error reading $propsPath: ${e.getMessage}")
            Seq()
        }
      } else {
        Seq()
      }
    }

    def getAdditionalScalacOptions(props: Properties, partialVersion: Option[(Int, Int)]): Seq[String] = {
      val allProps =
        Option(props.getProperty("scalac.options.additional"))
          .map(str => str.split(' ').toSeq)
          .getOrElse(Seq.empty)
      partialVersion match {
        case None => allProps
        case Some((major, minor)) =>
          val majorProps =
            Option(props.getProperty(s"scalac.$major.x.options.additional"))
              .map(str => str.split(' ').toSeq)
              .getOrElse(Seq.empty)
          val majorMinorProps =
            Option(props.getProperty(s"scalac.$major.$minor.options.additional"))
              .map(str => str.split(" ").toSeq)
              .getOrElse(Seq.empty)
          allProps ++ majorProps ++ majorMinorProps
      }
    }
    def compilerPluginDependencies(selectedScalaVersion: String): Agg[Dep] =
      if (selectedScalaVersion.startsWith("3.")) {
        Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
      } else {
        Agg()
      }

    def scalacOptions(scalaVersion: String, optimize: Boolean) = {

      val commonOptions = Seq("-deprecation", "-language:implicitConversions")

      val versionParts = scalaVersion.split("\\.")
      val extraOptions = versionParts match {
        case Array("2", _, _) =>
          Seq("-language:existentials", "-Yrangepos", "-Xsource:3", "-Xfatal-warnings")
        case Array("3", _, _) =>
          Seq("-Xignore-scala2-macros", "-Yretain-trees")
        case _ =>
          Seq()
      }
      commonOptions ++ extraOptions
    }

    //  def compileIvyDeps = T{
    //    if(scalaVersion().startsWith("2.")) {
    //      super.compileIvyDeps() ++ Agg(
    //        com.github.ghik.`silencer-plugin`
    //      )
    //    } else {
    //      super.compileIvyDeps()
    //    }
    //  }
    //  def ivyDeps = T {
    //    if (scalaVersion().startsWith("2."))
    //      Agg(com.github.ghik.`silencer-lib`)
    //    else
    //      Agg()
    //  }
  }

  trait MorphirScalaJSModule extends ScalaJSModule { outer =>
    def scalaJSVersion = ScalaVersions.scalaJSVersion

    // trait Tests extends super.ScalaJSTests with MorphirTestModule {
    //   override def scalacOptions = outer.scalacOptions()
    // // override def moduleDeps = super.moduleDeps ++ Seq(outer)
    // }
  }

  trait MorphirScalaNativeModule extends ScalaNativeModule { outer =>
    def scalaNativeVersion = ScalaVersions.scalaNativeVersion

    // trait Tests extends super.ScalaNativeTests with MorphirTestModule {
    //   override def scalacOptions = outer.scalacOptions()
    //   // override def moduleDeps = super.moduleDeps ++ Seq(outer)
    // }
  }
}

//-----------------------------------------------------------------------------
// Build helpers
//-----------------------------------------------------------------------------

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

trait CommonTestModule extends TestModule with CommonCoursierModule {
  def ivyDeps       = super.ivyDeps() ++ Agg(Deps.dev.zio.zio, Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
  def testFramework = "zio.test.sbt.ZTestFramework"
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

import mill.eval.{Evaluator, EvaluatorPaths}
// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}
