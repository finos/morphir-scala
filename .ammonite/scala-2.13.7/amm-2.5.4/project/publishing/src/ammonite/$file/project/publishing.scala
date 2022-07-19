
package ammonite
package $file.project
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


object publishing{
/*<script>*/import mill._, scalalib._
import scala.concurrent.duration._

def ghOrg  = "finos"
def ghName = "morphir4s"

def mavenOrg        = "io.get-coursier"
def sonatypeBaseUri = "https://s01.oss.sonatype.org" //"https://oss.sonatype.org/service/local"

def publishSonatype(
    data: Seq[PublishModule.PublishData],
    log: mill.api.Logger
): Unit = {

  val credentials = sys.env("SONATYPE_USERNAME") + ":" + sys.env("SONATYPE_PASSWORD")
  val pgpPassword = sys.env("PGP_PASSWORD")
  val timeout     = 10.minutes

  val artifacts = data.map { case PublishModule.PublishData(a, s) =>
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
    uri = s"${sonatypeBaseUri}/service/local",
    snapshotUri = s"${sonatypeBaseUri}/content/repositories/snapshots",
    credentials = credentials,
    signed = true,
    gpgArgs = Seq(
      "--detach-sign",
      "--batch=true",
      "--yes",
      "--pinentry-mode",
      "loopback",
      "--passphrase",
      pgpPassword,
      "--armor",
      "--use-agent"
    ),
    readTimeout = timeout.toMillis.toInt,
    connectTimeout = timeout.toMillis.toInt,
    log = log,
    awaitTimeout = timeout.toMillis.toInt,
    stagingRelease = isRelease
  )

  publisher.publishAll(isRelease, artifacts: _*)
}
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "publishing"
  /*</generated>*/
}
