import $meta._
import $ivy.`de.tototec::de.tobiasroeser.mill.integrationtest::0.7.1`
import $ivy.`de.tototec::de.tobiasroeser.mill.vcs.version::0.4.0`
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import $ivy.`com.lihaoyi::mill-contrib-scoverage:$MILL_VERSION`
import $ivy.`com.carlosedp::mill-aliases::0.5.0`
import $ivy.`com.github.lolgab::mill-mima::0.1.1`
import $file.project.deps, deps.{Deps, MillVersions, Versions => Vers, ScalaVersions => ScalaVers}
import $file.project.modules.docs, docs.{Docusaurus2Module, MDocModule}
import mill.testrunner.TestResult
import mill.scalalib.publish.PublishInfo
import mill.local.plugins.ci.release.ReleaseSetupModule
import com.carlosedp.aliases._
import de.tobiasroeser.mill.integrationtest._
import millbuild._
import millbuild.crossplatform._
import millbuild.jsruntime._
import org.finos.millmorphir._
import org.finos.millmorphir.elm._
import millbuild.settings._
import mill._, mill.scalalib._, mill.scalajslib._, mill.scalanativelib._, scalafmt._
import mill.scalajslib.api.ModuleKind
import mill.contrib.buildinfo.BuildInfo
import mill.contrib.scoverage.{ScoverageModule, ScoverageReport}
import com.github.lolgab.mill.mima._
import de.tobiasroeser.mill.vcs.version.VcsVersion
import scala.concurrent.duration.DurationInt

implicit val buildSettings: BuildSettings = interp.watchValue(MyBuild.cachedBuildSettings)

def resolvedBuildSettings = T.input { MyBuild.buildSettings() }

/**
 * The version of Scala natively supported by the toolchain. Morphir itself may provide backends that generate code for
 * other Scala versions. We may also directly cross-compile to additional Scla versions.
 */
val morphirScalaVersion: String = interp.watchValue(buildSettings.scala.defaultVersion)

val docsScalaVersion: String =
  interp.watchValue(buildSettings.scala.scala213Version) // This really should match but need to figure it out

import mill.eval.{Evaluator, EvaluatorPaths}

def bspInstall(jobs: Int = 1) = T.command {
  mill.bsp.BSP.install(jobs)
}

def idea(ev: Evaluator.AllBootstrapEvaluators) = T.command {
  mill.idea.GenIdea.idea(ev)
}

// With this we can now just do ./mill reformatAll __.sources
// instead of ./mill -w mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources
def reformatAll(evaluator: Evaluator, sources: mill.main.Tasks[Seq[PathRef]]) = T.command {
  ScalafmtModule.reformatAll(sources)()
}

def showBuildSettings() = T.command {
  MyBuild.showBuildSettings()
}

object scoverage extends ScoverageReport {
  // While the plugin doesn't seem to support multiple scala versions, that doesn't seem to impact the coverage
  // For overall coverage report, just specifying Scala 3
  def scalaVersion     = ScalaVers.scala3x
  def scoverageVersion = Vers.scoverage
}

trait MorphirPublishModule extends PublishModule with JavaModule with Mima {
  import mill.scalalib.publish._

  def publishVersion             = VcsVersion.vcsState().format()
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

object morphir extends Cross[MorphirCrossModule](buildSettings.scala.crossScalaVersions) {
  object build extends Module {
    object integration extends Module {
      object `mill-morphir-elm` extends Cross[morphirlibElmPlugin](MillVersions.all)
      trait morphirlibElmPlugin
          extends Cross.Module[String]
          with ScalaModule
          with ScalafmtModule
          with MorphirPublishModule {

        def millVersion = crossValue

        val pluginName = "mill-morphir-elm"

        def scalaVersion          = T { resolvedBuildSettings().mill.scalaVersion }
        override def artifactName = s"${pluginName}_mill${MillVersions.millBinaryVersion(millVersion)}"
        override def compileIvyDeps = super.compileIvyDeps() ++ Agg(
          ivy"com.lihaoyi::mill-scalalib:${millVersion}"
        )
        override def scalacOptions = Seq("-Ywarn-unused", "-deprecation")
        override def sources = T.sources {
          super.sources() ++ Seq(
            millSourcePath / s"src-mill${millVersion.split('.').take(2).mkString(".")}"
          ).map(PathRef(_))
        }
      }
      trait ItestCross extends MillIntegrationTestModule with Cross.Module[String]
    }
  }

  object main extends CommonScalaModule with MorphirPublishModule with BuildInfo {

    def buildInfoPackageName = "org.finos.morphir.cli"

    def buildInfoMembers = Seq(
      BuildInfo.Value("version", publishVersion()),
      BuildInfo.Value("scalaVersion", scalaVersion())
    )

    override def extraPublish: T[Seq[PublishInfo]] = T {
      Seq(PublishInfo(file = assembly(), classifier = Some("assembly"), ivyConfig = "compile"))
    }

    val mainScalaVersion = morphirScalaVersion

    def packageDescription =
      "The morphir-main package. This is the main entry point for the morphir tooling, including the morphir-cli."

    def scalaVersion = T { mainScalaVersion }
    def ivyDeps = Agg(
      Deps.co.fs2.`fs2-io`,
      Deps.com.lihaoyi.fansi,
      Deps.com.lihaoyi.pprint,
      Deps.com.lihaoyi.sourcecode,
      Deps.dev.zio.zio,
      Deps.dev.zio.`zio-interop-cats`,
      Deps.dev.zio.`zio-cli`,
      Deps.dev.zio.`zio-config`,
      Deps.dev.zio.config.magnolia,
      Deps.dev.zio.config.refined,
      Deps.dev.zio.config.typesafe
    )

    def moduleDeps =
      Seq(
        morphir(mainScalaVersion).jvm,
        morphir(mainScalaVersion).runtime.jvm,
        morphir(mainScalaVersion).tools.jvm
      )
  }

}
trait MorphirCrossModule extends Cross.Module[String] with CrossPlatform { morphir =>
  import DevMode._
  val workspaceDir = millbuild.build.millSourcePath

  trait MorphirCommonModule extends ScalaModule with CrossValue with CommonScalaModule {
    def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))

    def compilerPluginDependencies(selectedScalaVersion: String) =
      Agg.when(selectedScalaVersion.startsWith("3.")) {
        Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
      }
  }

  trait MorphirCommonCrossModule extends CrossPlatformScalaModule with CrossValue with CommonCrossScalaModule {
    def semanticDbVersion = T.input(Vers.semanticDb(partialVersion()))
    def compilerPluginDependencies(selectedScalaVersion: String) =
      Agg.when(selectedScalaVersion.startsWith("3.")) {
        Agg(Deps.org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
      }
  }

  trait MorphirJVMModule extends MorphirCommonCrossModule {
    def platform = Platform.JVM
  }

  trait MorphirJSModule extends MorphirCommonCrossModule with ScalaJSModule {
    import mill.scalajslib.api._
    def platform       = Platform.JS
    def scalaJSVersion = T { resolvedBuildSettings().js.version }
  }

  trait MorphirNativeModule extends MorphirCommonCrossModule {
    def platform           = Platform.Native
    def scalaNativeVersion = T { resolvedBuildSettings().native.version }
  }

  trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
    def ivyDeps = super.ivyDeps() ++ Agg(
      Deps.org.`scala-lang`.modules.`scala-collection-contrib`,
      Deps.com.beachape.enumeratum,
      Deps.com.lihaoyi.fansi,
      Deps.com.lihaoyi.geny,
      Deps.com.lihaoyi.sourcecode,
      Deps.com.lihaoyi.pprint,
      Deps.dev.zio.`izumi-reflect`,
      Deps.dev.zio.zio,
      Deps.dev.zio.`zio-json`,
      Deps.dev.zio.`zio-prelude`,
      Deps.org.typelevel.spire
    ) ++ Agg.when(isScala3())(
      Deps.com.softwaremill.magnolia_3.magnolia
    ) ++ Agg.when(isScala2())(
      Deps.com.softwaremill.magnolia_2.magnolia
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

    def platformSpecificModuleDeps = Seq(extensibility)
  }

  object jvm extends Shared with MorphirJVMModule
  object js  extends Shared with MorphirJSModule

  object contrib extends Module {
    object knowledge extends CrossPlatform with CrossValue {
      def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.") && !devMode
      trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
        def ivyDeps = Agg(
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.`zio-streams`
        )
        def platformSpecificModuleDeps = Seq(morphir)
      }

      object jvm extends Shared with MorphirJVMModule with ScoverageModule {
        def scoverageVersion = Vers.scoverage

        object test extends ScoverageTests with TestModule.ZioTest {
          def ivyDeps = Agg(
            Deps.dev.zio.zio,
            Deps.dev.zio.`zio-streams`,
            Deps.dev.zio.`zio-test`,
            Deps.dev.zio.`zio-test-sbt`
          )
          def moduleDeps = super.moduleDeps ++ Seq(testing.zio.jvm)
        }
      }

      object js extends Shared with MorphirJSModule {
        object test extends ScalaJSTests with TestModule.ZioTest {
          def ivyDeps = Agg(
            Deps.dev.zio.zio,
            Deps.dev.zio.`zio-streams`,
            Deps.dev.zio.`zio-test`,
            Deps.dev.zio.`zio-test-sbt`
          )
          def moduleDeps = super.moduleDeps ++ Seq(testing.zio.js)
        }
      }
    }
  }

  object extensibility extends CrossPlatform with CrossValue {

    trait Shared extends MorphirCommonCrossModule with MorphirPublishModule with BuildInfo {

      def buildInfoPackageName = "org.finos.morphir.extensibility"

      def buildInfoMembers = Seq(
        BuildInfo.Value("version", publishVersion()),
        BuildInfo.Value("scalaVersion", scalaVersion()),
        BuildInfo.Value("platform", platform.toString)
      )
    }

    object jvm extends Shared with MorphirJVMModule
    object js  extends Shared with MorphirJSModule
  }

  object interop extends Module {
    object zio extends Module {
      object json extends CrossPlatform with CrossValue {
        trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
          def ivyDeps                    = Agg(Deps.dev.zio.`zio-json`)
          def platformSpecificModuleDeps = Seq(morphir)
        }

        object jvm extends Shared with MorphirJVMModule with ScoverageModule {
          def scoverageVersion = Vers.scoverage

          object test extends ScoverageTests with TestModule.ZioTest {
            def ivyDeps: T[Agg[Dep]] = Agg(
              Deps.dev.zio.`zio-json-golden`,
              ivy"io.github.deblockt:json-diff:1.1.0",
              Deps.dev.zio.`zio-process`
            )

            def moduleDeps = super.moduleDeps ++ Agg(morphir.testing.generators.jvm, testing.zio.jvm)
          }
        }

        object js extends Shared with MorphirJSModule
      }
    }

    object borer extends CrossPlatform with CrossValue {
      trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
        def ivyDeps = Agg(
          Deps.io.bullet.`borer-core`(crossScalaVersion),
          Deps.io.bullet.`borer-derivation`(crossScalaVersion)
        )

        def platformSpecificModuleDeps = Seq(morphir)
      }

      object jvm extends Shared with MorphirJVMModule with ScoverageModule {
        def scoverageVersion = Vers.scoverage

        object test extends ScoverageTests with TestModule.ZioTest {
          def ivyDeps: T[Agg[Dep]] = Agg(
            Deps.io.bullet.`borer-core`(crossScalaVersion),
            Deps.io.bullet.`borer-derivation`(crossScalaVersion)
          )

          def moduleDeps = super.moduleDeps ++ Agg(morphir.testing.generators.jvm, testing.zio.jvm)
        }
      }

      object js extends Shared with MorphirJSModule
    }
  }
  object lib extends Module {

    object interop extends CrossPlatform {
      trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {}
      object jvm   extends Shared with MorphirJVMModule
      object js    extends Shared with MorphirJSModule
    }
  }

  object runtime extends CrossPlatform with CrossValue {

    def enableNative(module: Module): Boolean = crossValue.startsWith("2.13.") && !devMode
    trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
      def ivyDeps                    = Agg(Deps.org.typelevel.`scalac-compat-annotation`)
      def platformSpecificModuleDeps = Seq(morphir, morphir.interop.zio.json)
    }

    trait RuntimeTests extends TestModule.ZioTest {
      def morphirTestSources = T.sources {
        examples.`morphir-elm-projects`.`evaluator-tests`.distOutputDirs()
      }

      def morphirTestSourceFiles = T {
        Lib.findSourceFiles(morphirTestSources(), Seq("json")).collect {
          case path if path.last.startsWith("morphir-") => PathRef(path)
        }
      }

      override protected def testTask(
          args: Task[Seq[String]],
          globSelectors: Task[Seq[String]]
      ): Task[(String, Seq[TestResult])] = T.task {
        val _ = morphirTestSourceFiles()
        super.testTask(args, globSelectors)()
      }
    }

    object jvm extends Shared with MorphirJVMModule with ScoverageModule {
      def scoverageVersion = Vers.scoverage

      object test extends ScoverageTests with RuntimeTests {
        def ivyDeps = Agg(
          Deps.com.lihaoyi.`os-lib`,
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.`zio-test`,
          Deps.dev.zio.`zio-test-sbt`
        )
        def moduleDeps = super.moduleDeps ++ Agg(testing.zio.jvm)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaJSTests with RuntimeTests {
        def ivyDeps    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.zio.js)
        def moduleKind = ModuleKind.CommonJSModule
      }
    }
  }

  object testing extends Module {
    object generators extends CrossPlatform with CrossValue {
      trait Shared extends MorphirCommonCrossModule {
        def ivyDeps = Agg(Deps.dev.zio.`zio-test`) ++ Agg.when(!platform.isNative)(Deps.dev.zio.`zio-test-magnolia`)
        def platformSpecificModuleDeps = Seq(morphir)

      }
      object jvm extends Shared with MorphirJVMModule
      object js  extends Shared with MorphirJSModule
    }

    object zio extends CrossPlatform {
      trait Shared extends MorphirCommonCrossModule {
        def ivyDeps = Agg(
          ivy"io.github.cquiroz::scala-java-time::2.6.0",
          Deps.dev.zio.`zio-json`,
          Deps.dev.zio.`zio-prelude`,
          Deps.dev.zio.`zio-test`,
          Deps.dev.zio.`zio-test-sbt`
        )
      }
      object jvm extends Shared with MorphirJVMModule
      object js  extends Shared with MorphirJSModule
    }
  }

  object tests extends CrossPlatform {

    trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.com.lihaoyi.sourcecode,
        Deps.io.github.cquiroz.`scala-java-time`,
        Deps.io.github.cquiroz.`scala-java-time-tzdb`
      )
      def platformSpecificModuleDeps = Seq(extensibility, morphir, runtime, tools)
    }

    object jvm extends Shared with MorphirJVMModule with ScoverageModule {
      def scoverageVersion = Vers.scoverage

      object test extends ScoverageTests with TestModule.ZioTest {

        def ivyDeps = Agg(
          Deps.com.lihaoyi.`os-lib`,
          Deps.com.lihaoyi.sourcecode,
          Deps.dev.zio.`zio-test`,
          Deps.dev.zio.`zio-test-sbt`
        )

        def moduleDeps = super.moduleDeps ++ Agg(testing.generators.jvm, testing.zio.jvm)
      }
    }

    object js extends Shared with MorphirJSModule {
      object test extends ScalaJSTests with TestModule.ZioTest {
        def ivyDeps    = Agg(Deps.dev.zio.`zio-test`, Deps.dev.zio.`zio-test-sbt`)
        def moduleDeps = super.moduleDeps ++ Agg(testing.generators.js, testing.zio.js)
      }
    }
  }

  object tools extends CrossPlatform {
    trait Shared extends MorphirCommonCrossModule with MorphirPublishModule {
      def ivyDeps = Agg(
        Deps.co.fs2.`fs2-io`,
        Deps.com.lihaoyi.sourcecode,
        Deps.dev.zio.zio,
        Deps.dev.zio.`zio-prelude`,
        Deps.dev.zio.`zio-streams`,
        Deps.com.lihaoyi.pprint,
        Deps.org.typelevel.cats.core,
        Deps.org.typelevel.spire,
        Deps.org.typelevel.`paiges-core`
      ) ++ Agg.when(!platform.isNative)(Deps.dev.zio.`zio-interop-cats`)

      def platformSpecificModuleDeps = Seq(morphir, morphir.interop.zio.json)
    }

    object jvm extends Shared with MorphirJVMModule {
      def ivyDeps = super.ivyDeps() ++ Agg(
        Deps.dev.zio.`zio-nio`,
        Deps.dev.zio.`zio-process`
      )
    }
    object js extends Shared with MorphirJSModule {
      def docSources = T.sources {
        Lib.findSourceFiles(super.docSources(), Seq("tasty"))
          .map(PathRef(_))
        // .filterNot(_.path.last.contains("ProcessIOPlat"))
      }
    }
  }
}

// The following modules are morphir-elm modules that have been setup to be built using mill
// Morphir Elm Projects/Modules:
object examples extends Module {
  object `morphir-elm-projects` extends Module {
    object finance extends MorphirElmModule

    object `evaluator-tests` extends MorphirElmModule
    object `unit-test-framework` extends Module {
      object `example-project` extends MorphirElmModule {
        def morphirModuleDeps = Seq(`morphir-elm`.sdks.`morphir-unit-test`)
      }

      object `example-project-tests` extends MorphirElmModule {
        def morphirModuleDeps = Seq(
          `morphir-elm`.sdks.`morphir-unit-test`,
          `example-project`
        )
      }
      object `example-project-tests-incomplete` extends MorphirElmModule {
        def morphirModuleDeps = Seq(
          `morphir-elm`.sdks.`morphir-unit-test`,
          `example-project`
        )
      }
      object `example-project-tests-passing` extends MorphirElmModule {
        def morphirModuleDeps = Seq(
          `morphir-elm`.sdks.`morphir-unit-test`,
          `example-project`
        )
      }
    }
  }
}

object `morphir-elm` extends Module {
  object sdks extends Module {
    object `morphir-unit-test` extends MorphirElmModule
  }
}

object ci extends Module {

  def publishSonatype(tasks: mill.main.Tasks[PublishModule.PublishData]) = T.command {
    // ReleaseSetupModule.setupGpg()()
    publishSonatype0(
      data = define.Target.sequence(tasks.value)(),
      log = T.ctx().log
    )
  }

  def publishSonatype0(
      data: Seq[PublishModule.PublishData],
      log: mill.api.Logger
  ): Unit = {

    val credentials = sys.env("SONATYPE_USERNAME") + ":" + sys.env("SONATYPE_PASSWORD")
    val pgpPassword = sys.env("PGP_PASSPHRASE")
    val timeout     = 20.minutes // SONATYPE CAN BE SUPER SLOW

    val artifacts = data.map {
      case PublishModule.PublishData(a, s) =>
        (s.map { case (p, f) => (p.path, f) }, a)
    }

    val isRelease = {
      val versions = artifacts.map(_._2.version).toSet
      val set      = versions.map(!_.endsWith("-SNAPSHOT"))
      assert(
        set.size == 1,
        s"Found both snapshot and non-snapshot versions: ${versions.toVector.sorted.mkString(", ")}"
      )
      set.head
    }
    val publisher = new scalalib.publish.SonatypePublisher(
      uri = "https://s01.oss.sonatype.org/service/local",
      snapshotUri = "https://s01.oss.sonatype.org/content/repositories/snapshots",
      credentials = credentials,
      signed = true,
      // format: off
      gpgArgs = Seq(
        "--detach-sign",
        "--batch=true",
        "--yes",
        "--pinentry-mode", "loopback",
        "--passphrase", pgpPassword,
        "--armor",
        "--use-agent"
      ),
      // format: on
      readTimeout = timeout.toMillis.toInt,
      connectTimeout = timeout.toMillis.toInt,
      log = log,
      awaitTimeout = timeout.toMillis.toInt,
      stagingRelease = isRelease
    )

    publisher.publishAll(isRelease, artifacts: _*)
  }

}

// The following section contains aliases used to simplify build tasks

object MyAliases extends Aliases {
  def fmt           = alias("mill.scalalib.scalafmt.ScalafmtModule/reformatAll __.sources")
  def checkfmt      = alias("mill.scalalib.scalafmt.ScalafmtModule/checkFormatAll __.sources")
  def deps          = alias("mill.scalalib.Dependency/showUpdates")
  def testall       = alias("__.test")
  def testJVM       = alias("morphir.__.jvm.__.test")
  def testJVMCached = alias("morphir.__.jvm.__.testCached")
  def compileall    = alias("__.compile")
  def comptestall   = alias("__.compile", "__.test")
}
