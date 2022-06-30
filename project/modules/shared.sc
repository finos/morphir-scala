import $file.^.deps, deps.{Deps, ScalaVersions}

import mill._, mill.scalalib._, mill.scalajslib._, scalafmt._
import Deps._

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

trait MorphirPublishModule extends PublishModule with JavaModule {
  import mill.scalalib.publish._
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "org.finos.morphir",
    url = "https://github.com/coursier/coursier",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("coursier", "coursier"),
    developers = Seq(
      Developer("alexarchambault", "Alex Archambault", "https://github.com/alexarchambault")
    )
  )
  def publishVersion = T(buildVersion)
  def javacOptions = T {
    super.javacOptions() ++ Seq("-source", "8", "-target", "8")
  }
}

trait MorphirCrossScalaModule extends CommonCrossModule {}

trait MorphirTestModule extends CommonTestModule {}

trait CommonCrossModule extends CrossScalaModule with ScalafmtModule {
  def scalacOptions = T.task {
    val extraOptions = if (this.crossScalaVersion.startsWith("2.")) {
      Seq("-Yrangepos")
    } else {
      Seq()
    }
    super.scalacOptions() ++ extraOptions
  }
}

trait CommonTestModule extends TestModule {
  def ivyDeps       = super.ivyDeps() ++ Agg(dev.zio.zio, dev.zio.`zio-test`, dev.zio.`zio-test-sbt`)
  def testFramework = "zio.test.sbt.ZTestFramework"
}
