import mill._, scalalib._

object Deps {
  case object com {
    case object lihaoyi {
      val sourcecode = ivy"com.lihaoyi::sourcecode::0.3.0"
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
