import mill._, scalalib._

object Deps {

  case object ch {
    case object epfl {
      case object scala {
        val `tasty-query` = ivy"ch.epfl.scala::tasty-query::${Versions.`tasty-query`}"
      }
    }
  }
  case object com {
    case object beachape {
      val enumeratum = ivy"com.beachape::enumeratum::${Versions.enumeratum}"
    }
    case object github {
      case object arturopala {
        val `buffer-and-slice` = ivy"com.github.arturopala::buffer-and-slice:${Versions.`buffer-and-slice`}"
      }

      case object ghik {
        val `silencer-lib`    = ivy"com.github.ghik:::silencer-lib:${Versions.silencer}"
        val `silencer-plugin` = ivy"com.github.ghik:::silencer-plugin:${Versions.silencer}"
      }
    }
    case object lihaoyi {
      val castor         = ivy"com.lihaoyi::castor::${Versions.castor}"
      val geny           = ivy"com.lihaoyi::geny::${Versions.geny}"
      val mainargs       = ivy"com.lihaoyi::mainargs::${Versions.`mainargs`}"
      val `os-lib`       = ivy"com.lihaoyi::os-lib::${Versions.`os-lib`}"
      val sourcecode     = ivy"com.lihaoyi::sourcecode::0.3.0"
      val pprint         = ivy"com.lihaoyi::pprint::0.8.1"
      val ujson          = ivy"com.lihaoyi::ujson::${Versions.upickle}"
      val upickle        = ivy"com.lihaoyi::upickle::${Versions.upickle}"
      val `upickle-core` = ivy"com.lihaoyi::upickle-core::${Versions.upickle}"
    }

    case object outr {
      val scribe = ivy"com.outr::scribe::${Versions.scribe}"
    }
    case object softwaremill {
      case object common {
        val tagging = ivy"com.softwaremill.common::tagging::2.3.4"
      }
    }
  }
  case object dev {
    case object zio {
      val `izumi-reflect`      = ivy"dev.zio::izumi-reflect::${Versions.`izumi-reflect`}"
      val zio: Dep             = ivy"dev.zio::zio::${Versions.zio}"
      val `zio-cli`            = ivy"dev.zio::zio-cli::${Versions.`zio-cli`}"
      val `zio-json`: Dep      = ivy"dev.zio::zio-json::${Versions.`zio-json`}"
      val `zio-json-golden`    = ivy"dev.zio::zio-json-golden::${Versions.`zio-json`}"
      val `zio-parser`         = ivy"dev.zio::zio-parser::${Versions.`zio-parser`}"
      val `zio-prelude`        = ivy"dev.zio::zio-prelude::${Versions.`zio-prelude`}"
      val `zio-prelude-macros` = ivy"dev.zio::zio-prelude-macros::${Versions.`zio-prelude`}"
      val `zio-process`        = ivy"dev.zio::zio-process::${Versions.`zio-process`}"
      val `zio-schema`         = ivy"dev.zio::zio-streams::${Versions.`zio-schema`}"
      val `zio-streams`        = ivy"dev.zio::zio-streams::${Versions.zio}"
      val `zio-test`           = ivy"dev.zio::zio-test::${Versions.zio}"
      val `zio-test-magnolia`  = ivy"dev.zio::zio-test-magnolia::${Versions.zio}"
      val `zio-test-sbt`       = ivy"dev.zio::zio-test-sbt::${Versions.zio}"
    }
  }
  case object io {
    case object bullet {
      def `borer-core`(scalaVersion: String): Dep = ivy"io.bullet::borer-core::${Versions.borer(scalaVersion)}"
      def `borer-core`(scalaVersionParts: Seq[String]): Dep =
        ivy"io.bullet::borer-core::${Versions.borer(scalaVersionParts)}"

      def `borer-derivation`(scalaVersion: String): Dep =
        ivy"io.bullet::borer-derivation::${Versions.borer(scalaVersion)}"

      def `borer-derivation`(scalaVersionParts: Seq[String]): Dep =
        ivy"io.bullet::borer-derivation::${Versions.borer(scalaVersionParts)}"
    }
    case object `get-coursier` {
      val coursier = ivy"io.get-coursier::coursier::${Versions.coursier}"
    }
    case object lemonlabs {
      val `scala-uri` = ivy"io.lemonlabs::scala-uri::4.0.3"
    }
  }
  case object org {
    case object `scala-lang` {
      def `scala-compiler`(scalaVersion: String): Dep  = ivy"org.scala-lang:scala-compiler:$scalaVersion"
      def `scala-reflect`(scalaVersion: String): Dep   = ivy"org.scala-lang:scala-reflect:$scalaVersion"
      def `scala3-compiler`(scalaVersion: String): Dep = ivy"org.scala-lang::scala3-compiler:$scalaVersion"
      def `scala3-tasty-inspector`(scalaVersion: String): Dep =
        ivy"org.scala-lang::scala3-tasty-inspector::$scalaVersion"
    }
    case object scalameta {
      val munit: mill.scalalib.Dep = ivy"org.scalameta::munit::${Versions.munit}"
      println(s"$munit")
      val `munit-scalacheck` =
        ivy"org.scalameta::munit-scalacheck::${Versions.munit}"

    }

    case object typelevel {
      val `paiges-core` = ivy"org.typelevel::paiges-core::${Versions.paiges}"
    }
  }
}

object Versions {
  val castor = "0.2.1"

  val enumeratum = "1.7.2"

  def borer(scalaVersion: String): String =
    borer(scalaVersion.split('.'))

  def borer(scalaVersionParts: Seq[String]): String =
    scalaVersionParts match {
      case Seq("3", _, _)    => "1.10.1"
      case Seq("2", "13", _) => "1.8.0"
      case _                 => "1.6.3"
    }

  val `buffer-and-slice` = "1.57.0"

  def semanticDb(partialVersion: Option[(Int, Int)]): String =
    partialVersion match {
      case Some((2, _)) => "4.5.11"
      case _            => "4.5.11"
    }

  val coursier        = "2.1.3"
  val geny            = "1.0.0"
  val `izumi-reflect` = "2.3.6"
  val munit           = "1.0.0-M4"
  val mainargs        = "0.5.0"
  val `os-lib`        = "0.9.1"
  val paiges          = "0.4.2"
  val scribe          = "3.10.7"
  val silencer        = "1.4.2"
  val `tasty-query`   = "0.5.6"
  val upickle         = "3.0.0-M1"
  val zio             = "2.0.13"
  val `zio-cli`       = "0.4.0"
  val `zio-json`      = "0.5.0"
  val `zio-parser`    = "0.1.9"
  val `zio-prelude`   = "1.0.0-RC19"
  val `zio-process`   = "0.7.2"
  val `zio-schema`    = "0.4.11"
}

object ScalaVersions {
  val all      = Seq(scala213, scala3x)
  def scala213 = "2.13.10"
  def scala3x  = "3.2.2"

  def scalaJSVersion     = "1.13.1"
  def scalaNativeVersion = "0.4.12"
}
