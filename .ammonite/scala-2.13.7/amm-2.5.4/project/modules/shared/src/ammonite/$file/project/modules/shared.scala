
package ammonite
package $file.project.modules
import _root_.ammonite.interp.api.InterpBridge.{
  value => interp
}
import _root_.ammonite.interp.api.InterpBridge.value.{
  exit,
  scalaVersion
}
import _root_.ammonite.interp.api.IvyConstructor.{
  ArtifactIdExt,
  GroupIdExt
}
import _root_.ammonite.compiler.CompilerExtensions.{
  CompilerInterpAPIExtensions,
  CompilerReplAPIExtensions
}
import _root_.ammonite.runtime.tools.{
  browse,
  grep,
  time,
  tail
}
import _root_.ammonite.compiler.tools.{
  desugar,
  source
}
import _root_.mainargs.{
  arg,
  main
}
import _root_.ammonite.repl.tools.Util.{
  PathRead
}
import ammonite.$file.project.{
  deps
}
import ammonite.$file.project.modules.{
  dependencyCheck
}


object shared{
/*<script>*/import $ivy.$                                                    
import $file.$     , deps.{Deps, ScalaVersions}
import $file.$              , dependencyCheck.DependencyCheckModule
import mill._, mill.scalalib._, mill.scalajslib._, scalafmt._
import de.tobiasroeser.mill.vcs.version._
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

trait MorphirPublishModule extends PublishModule with JavaModule with DependencyCheckModule {
  import mill.scalalib.publish._
  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "morphir",
    url = "https://github.com/finos/morphir4s",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("finos", "morphir4s"),
    developers = Seq(
      Developer("DamianReeves", "Damian Reeves", "https://github.com/damianreeves")
    )
  )
  def publishVersion = VcsVersion.vcsState().format()
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
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "shared"
  /*</generated>*/
}
