import mill.define.{Target, Task}
import $ivy.`io.chris-kipp::mill-ci-release::0.1.1`
import $file.^.deps, deps.{Deps, ScalaVersions, Versions => Vers}
import $file.dependencyCheck, dependencyCheck.DependencyCheckModule
import mill._, mill.scalalib._, mill.scalajslib._, scalafmt._
import mill.scalalib.bsp.ScalaMetalsSupport
import io.kipp.mill.ci.release.CiReleaseModule
import Deps._
import java.util.Properties

def commitHash = T {
  os.proc("git", "rev-parse", "HEAD").call().out.text.trim
}

lazy val latestTaggedVersion = os
  .proc("git", "describe", "--abbrev=0", "--tags", "--match", "v*")
  .call()
  .out
  .trim
lazy val buildVersion = {
  val gitHead = os.proc("git", "rev-parse", "HEAD").call().out.trim
  val maybeExactTag = scala.util.Try {
    os.proc("git", "describe", "--exact-match", "--tags", "--always", gitHead)
      .call()
      .out
      .trim
      .stripPrefix("v")
  }
  maybeExactTag.toOption.getOrElse {
    val commitsSinceTaggedVersion =
      os.proc('git, "rev-list", gitHead, "--not", latestTaggedVersion, "--count")
        .call()
        .out
        .trim
        .toInt
    val gitHash = os.proc("git", "rev-parse", "--short", "HEAD").call().out.trim
    s"${latestTaggedVersion.stripPrefix("v")}-$commitsSinceTaggedVersion-$gitHash-SNAPSHOT"
  }
}

trait MorphirPublishModule extends CiReleaseModule with JavaModule with DependencyCheckModule {
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

trait MorphirCrossScalaModule extends CommonCrossModule {
  override def scalaVersion: T[String] = T(crossScalaVersion)
}

trait MorphirScalaModule extends CommonScalaModule {}
trait MorphirTestModule  extends CommonTestModule  {}

trait CommonScalaModule extends ScalaModule with CommonCoursierModule with ScalafmtModule with ScalaMetalsSupport {
  self =>
  def crossScalaVersion: String
  def scalaVersion: T[String] = T(crossScalaVersion)
  def semanticDbVersion       = T.input(Vers.semanticDb(partialVersion()))

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
  def scalacOptions = T {
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

  def getAdditionalScalacOptions(props:Properties, partialVersion:Option[(Int,Int)]):Seq[String] = {
    val allProps =
      Option(props.getProperty("scalac.options.additional"))
        .map(str => str.split(' ').toSeq).getOrElse(Seq.empty)
    partialVersion match {
      case None => allProps
      case Some((major,minor)) =>
        val majorProps =
          Option(props.getProperty(s"scalac.$major.x.options.additional"))
            .map(str => str.split(' ').toSeq).getOrElse(Seq.empty)
        val majorMinorProps =
          Option(props.getProperty(s"scalac.$major.$minor.options.additional"))
            .map(str => str.split(" ").toSeq).getOrElse(Seq.empty)
        allProps ++ majorProps ++ majorMinorProps
    }
  }
  def compilerPluginDependencies(selectedScalaVersion: String): Agg[Dep] =
    if (selectedScalaVersion.startsWith("3.")) {
      Agg(org.`scala-lang`.`scala3-compiler`(selectedScalaVersion))
    } else {
      Agg()
    }

  def scalacOptions(scalaVersion: String, optimize: Boolean) = {

    val commonOptions = Seq("-language:implicitConversions")

    val versionParts = scalaVersion.split("\\.")
    val extraOptions = versionParts match {
      case Array("2", _, _) =>
        Seq("-Yrangepos", "-Xsource:3.0")
      case Array("3", _, _) =>
        Seq("-Xignore-scala2-macros", "-Yretain-trees")
      case _ =>
        Seq()
    }
    commonOptions ++ extraOptions
  }
}

trait CommonCrossModule extends CrossScalaModule with CommonCoursierModule with CommonScalaModule {
  override def scalaVersion: T[String] = T(crossScalaVersion)
}

trait CommonTestModule extends TestModule with CommonCoursierModule {
  def ivyDeps       = super.ivyDeps() ++ Agg(dev.zio.zio, dev.zio.`zio-test`, dev.zio.`zio-test-sbt`)
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
