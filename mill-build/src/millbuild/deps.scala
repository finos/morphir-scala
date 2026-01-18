package millbuild

import mill.*, scalalib.*
import millbuild.crossplatform.DevMode

object Deps {

  object ch {
    object epfl {
      object scala {
        val `tasty-query` = ivy"ch.epfl.scala::tasty-query::${Versions.`tasty-query`}"
      }
    }
  }

  object co {
    object fs2 {
      val `fs2-core` = ivy"co.fs2::fs2-core::${Versions.fs2}"
      val `fs2-io`   = ivy"co.fs2::fs2-io::${Versions.fs2}"
    }
  }

  object com {
    object beachape {
      val enumeratum = ivy"com.beachape::enumeratum::${Versions.enumeratum}"
        .exclude("org.scala-lang" -> "scala3-library_3")
    }

    object eed3si9n {
      object expecty {
        val expecty = ivy"com.eed3si9n.expecty::expecty::${Versions.expecty}"
      }
    }

    object geirsson {
      object metaconfig {
        val core    = ivy"com.geirsson::metaconfig-core::${Versions.metaconfig}"
        val docs    = ivy"com.geirsson::metaconfig-docs::${Versions.metaconfig}"
        val json    = ivy"com.geirsson::metaconfig-json::${Versions.metaconfig}"
        val pprint  = ivy"com.geirsson::metaconfig-pprint::${Versions.metaconfig}"
        val sconfig = ivy"com.geirsson::metaconfig-sconfig::${Versions.metaconfig}"
      }
    }

    object github {
      object arturopala {
        val `buffer-and-slice` = ivy"com.github.arturopala::buffer-and-slice:${Versions.`buffer-and-slice`}"
      }

      object ghik {
        val `silencer-lib`    = ivy"com.github.ghik:::silencer-lib:${Versions.silencer}"
        val `silencer-plugin` = ivy"com.github.ghik:::silencer-plugin:${Versions.silencer}"
      }

      object lolgab {
        val `scala-native-crypto` = ivy"com.github.lolgab::scala-native-crypto::${Versions.`scala-native-crypto`}"
      }
    }
    object lihaoyi {
      val castor         = ivy"com.lihaoyi::castor::${Versions.castor}"
      val fansi          = ivy"com.lihaoyi::fansi::${Versions.fansi}"
      val geny           = ivy"com.lihaoyi::geny::${Versions.geny}"
      val mainargs       = ivy"com.lihaoyi::mainargs::${Versions.`mainargs`}"
      val `os-lib`       = ivy"com.lihaoyi::os-lib::${Versions.`os-lib`}"
      val sourcecode     = ivy"com.lihaoyi::sourcecode::0.4.2"
      val pprint         = ivy"com.lihaoyi::pprint::0.9.0"
      val ujson          = ivy"com.lihaoyi::ujson::${Versions.upickle}"
      val upickle        = ivy"com.lihaoyi::upickle::${Versions.upickle}"
      val `upickle-core` = ivy"com.lihaoyi::upickle-core::${Versions.upickle}"
    }

    object outr {
      val scribe = ivy"com.outr::scribe::${Versions.scribe}"
    }
    object softwaremill {
      object common {
        val tagging = ivy"com.softwaremill.common::tagging::2.3.4"
      }

      object magnolia_2 {
        val magnolia = ivy"com.softwaremill.magnolia1_2::magnolia::1.1.8"
      }

      object magnolia_3 {
        val magnolia = ivy"com.softwaremill.magnolia1_3::magnolia::1.3.9"
      }
    }
  }
  object dev {
    object zio {
      val `izumi-reflect`      = ivy"dev.zio::izumi-reflect::${Versions.`izumi-reflect`}"
      val zio: Dep             = ivy"dev.zio::zio::${Versions.zio}"
      val `zio-cli`            = ivy"dev.zio::zio-cli::${Versions.`zio-cli`}"
      val `zio-config`         = config()
      val `zio-interop-cats`   = ivy"dev.zio::zio-interop-cats::${Versions.`zio-interop-cats`}"
      val `zio-json`: Dep      = ivy"dev.zio::zio-json::${Versions.`zio-json`}"
      val `zio-json-golden`    = ivy"dev.zio::zio-json-golden::${Versions.`zio-json`}"
      val `zio-parser`         = ivy"dev.zio::zio-parser::${Versions.`zio-parser`}"
      val `zio-nio`            = ivy"dev.zio::zio-nio::${Versions.`zio-nio`}"
      val `zio-prelude`        = prelude()
      val `zio-prelude-macros` = prelude.macros
      val `zio-process`        = ivy"dev.zio::zio-process::${Versions.`zio-process`}"
      val `zio-streams`        = ivy"dev.zio::zio-streams::${Versions.zio}"
      val `zio-test`           = ivy"dev.zio::zio-test::${Versions.zio}"
      val `zio-test-magnolia`  = ivy"dev.zio::zio-test-magnolia::${Versions.zio}"
      val `zio-test-sbt`       = ivy"dev.zio::zio-test-sbt::${Versions.zio}"

      object config {
        def apply(): Dep = ivy"dev.zio::zio-config::${Versions.`zio-config`}"
        val magnolia     = ivy"dev.zio::zio-config-magnolia::${Versions.`zio-config`}"
        val refined      = ivy"dev.zio::zio-config-refined::${Versions.`zio-config`}"
        val typesafe     = ivy"dev.zio::zio-config-typesafe::${Versions.`zio-config`}"
      }

      object prelude {
        def apply(): Dep = ivy"dev.zio::zio-prelude::${Versions.`zio-prelude`}"
        val macros       = ivy"dev.zio::zio-prelude-macros::${Versions.`zio-prelude`}"
      }

      object schema {
        val `avro`       = ivy"dev.zio::zio-schema-avro::${Versions.`zio-schema`}"
        val `bson`       = ivy"dev.zio::zio-schema-bson::${Versions.`zio-schema`}"
        val `core`       = ivy"dev.zio::zio-schema-core::${Versions.`zio-schema`}"
        val `derivation` = ivy"dev.zio::zio-schema-derivation::${Versions.`zio-schema`}"
        val `json`       = ivy"dev.zio::zio-schema-json::${Versions.`zio-schema`}"
        val `msg-pack`   = ivy"dev.zio::zio-schema-msg-pack::${Versions.`zio-schema`}"
      }
    }
  }
  object io {
    object bullet {
      def `borer-core`(scalaVersion: String): Dep = ivy"io.bullet::borer-core::${Versions.borer(scalaVersion)}"
      def `borer-core`(scalaVersionParts: Seq[String]): Dep =
        ivy"io.bullet::borer-core::${Versions.borer(scalaVersionParts)}"

      def `borer-derivation`(scalaVersion: String): Dep =
        ivy"io.bullet::borer-derivation::${Versions.borer(scalaVersion)}"

      def `borer-derivation`(scalaVersionParts: Seq[String]): Dep =
        ivy"io.bullet::borer-derivation::${Versions.borer(scalaVersionParts)}"
    }
    object `get-coursier` {
      val coursier = ivy"io.get-coursier::coursier::${Versions.coursier}"
    }
    object github {
      object cquiroz {
        val `scala-java-time`      = ivy"io.github.cquiroz::scala-java-time::${Versions.`scala-java-time`}"
        val `scala-java-time-tzdb` = ivy"io.github.cquiroz::scala-java-time-tzdb::${Versions.`scala-java-time`}"
      }
    }
    object lemonlabs {
      val `scala-uri` = ivy"io.lemonlabs::scala-uri::4.0.3"
    }
  }
  object org {
    object `scala-lang` {

      object modules {
        val `scala-collection-contrib` = ivy"org.scala-lang.modules::scala-collection-contrib::0.4.0"
      }

      def `scala-compiler`(scalaVersion: String): Dep =
        if (scalaVersion.startsWith("3")) ivy"org.scala-lang::scala3-compiler:$scalaVersion"
        else ivy"org.scala-lang:scala-compiler:$scalaVersion"
      def `scala-library`(scalaVersion: String): Dep   = ivy"org.scala-lang:scala-library:$scalaVersion"
      def `scala-reflect`(scalaVersion: String): Dep   = ivy"org.scala-lang:scala-reflect:$scalaVersion"
      def `scala3-compiler`(scalaVersion: String): Dep = ivy"org.scala-lang::scala3-compiler:$scalaVersion"
      def `scala3-tasty-inspector`(scalaVersion: String): Dep =
        ivy"org.scala-lang::scala3-tasty-inspector::$scalaVersion"
    }

    object scalameta {}

    object typelevel {
      val `cats-core`   = cats.core
      val `paiges-core` = ivy"org.typelevel::paiges-core::${Versions.paiges}"
      val `scalac-compat-annotation` =
        ivy"org.typelevel::scalac-compat-annotation:${Versions.`scalac-compat-annotation`}"
      val spire = ivy"org.typelevel::spire::${Versions.spire}"

      object cats {
        val core = ivy"org.typelevel::cats-core::${Versions.cats}"
      }
    }
  }
}

object Versions {
  val castor = "0.2.1"
  val cats   = "2.12.0"

  val enumeratum = "1.7.5"

  def borer(scalaVersion: String): String =
    borer(scalaVersion.split('.').toIndexedSeq)

  def borer(scalaVersionParts: Seq[String]): String =
    scalaVersionParts match {
      case Seq("3", _, _)    => "1.14.0"
      case Seq("2", "13", _) => "1.8.0"
      case _                 => "1.6.3"
    }

  val `buffer-and-slice` = "1.57.0"

  def semanticDb(partialVersion: Option[(Int, Int)]): String =
    partialVersion match {
      case Some((2, _)) => "4.8.11"
      case _            => "4.8.11"
    }

  val coursier                   = "2.1.4"
  val expecty                    = "0.16.0"
  val fansi                      = "0.5.0"
  val fs2                        = "3.11.0"
  val geny                       = "1.1.1"
  val `izumi-reflect`            = "2.3.10"
  val metaconfig                 = "0.11.1"
  val mainargs                   = "0.5.0"
  val `os-lib`                   = "0.11.4"
  val paiges                     = "0.4.4"
  val `scala-java-time`          = "2.6.0"
  val `scala-native-crypto`      = "0.0.4"
  val `scalac-compat-annotation` = "0.1.4"
  val scoverage                  = "2.1.1"
  val scribe                     = "3.11.9"
  val silencer                   = "1.4.2"
  val spire                      = "0.18.0"
  val `tasty-query`              = "0.5.6"
  val upickle                    = "3.0.0-M1"
  val zio                        = "2.1.11"

  val `zio-config`       = "4.0.3"
  val `zio-cli`          = "0.7.0"
  val `zio-interop-cats` = "23.1.0.3"
  val `zio-json`         = "0.7.3"
  val `zio-nio`          = "2.0.2"
  val `zio-parser`       = "0.1.9"
  val `zio-prelude`      = "1.0.0-RC31"
  val `zio-process`      = "0.7.2"
  val `zio-schema`       = "0.4.12"
}

object ScalaVersions {
  import DevMode._
  val all      = if (devMode) Seq(scala3x) else Seq(scala213, scala3x)
  def scala213 = "2.13.16"
  def scala3x  = "3.7.4"

  def scalaJSVersion     = "1.18.2"
  def scalaNativeVersion = "0.5.7"
  def millScalaVersion   = "2.13.16"
}

object MillVersions {
  val all = Seq("0.11.0", "1.0.5")
  // Compute Mill binary version similar to Scala Native binary version
  def millBinaryVersion(millVersion: String): String = {
    val parts = millVersion.split('.').take(2)
    parts.mkString(".")
  }
}
