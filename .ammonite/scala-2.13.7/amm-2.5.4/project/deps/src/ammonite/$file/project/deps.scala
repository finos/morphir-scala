
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


object deps{
/*<script>*/import mill._, scalalib._

object Deps {
  case object com {
    case object lihaoyi {
      val sourcecode = ivy"com.lihaoyi::sourcecode::0.2.8"
    }
    case object softwaremill {
      case object common {
        val tagging = ivy"com.softwaremill.common::tagging::2.3.3"
      }
    }
  }
  case object dev {
    case object zio {
      val zio: Dep            = ivy"dev.zio::zio::${Versions.zio}"
      val `zio-prelude`       = ivy"dev.zio::zio-prelude::${Versions.`zio-prelude`}"
      val `zio-streams`       = ivy"dev.zio::zio-streams::${Versions.zio}"
      val `zio-test`          = ivy"dev.zio::zio-test::${Versions.zio}"
      val `zio-test-magnolia` = ivy"dev.zio::zio-test-magnolia::${Versions.zio}"
      val `zio-test-sbt`      = ivy"dev.zio::zio-test-sbt::${Versions.zio}"
    }
  }
  case object io {
    case object lemonlabs {
      val `scala-uri` = ivy"io.lemonlabs::scala-uri:4.0.2"
    }
  }
  case object org {
    case object scalameta {
      val munit: mill.scalalib.Dep = ivy"org.scalameta::munit::${Versions.munit}"
      println(s"$munit")
      val `munit-scalacheck` =
        ivy"org.scalameta::munit-scalacheck::${Versions.munit}"

    }
  }
}

object Versions {
  val munit         = "1.0.0-M4"
  val zio           = "2.0.0"
  val `zio-prelude` = "1.0.0-RC15"
}

object ScalaVersions {
  val all      = Seq(scala213, scala3x)
  def scala213 = "2.13.8"
  def scala3x  = "3.1.3"
}
/*</script>*/ /*<generated>*/
def $main() = { scala.Iterator[String]() }
  override def toString = "deps"
  /*</generated>*/
}
