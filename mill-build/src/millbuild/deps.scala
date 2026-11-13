package millbuild

import mill.*, scalalib.*

object Deps {

  object com {
    object github {
      object ghostdogpr {
        val `caliban-client` = mvn"com.github.ghostdogpr::caliban-client::${Versions.caliban}"
      }
    }
    object lihaoyi {
      val `os-lib`                 = mvn"com.lihaoyi::os-lib::${Versions.`os-lib`}"
      val osLibJvm                 = mvn"com.lihaoyi:os-lib_3:${Versions.`os-lib`}"
      val sourcecodeJvm            = mvn"com.lihaoyi::sourcecode:${Versions.sourcecode}"
      val ujson                    = mvn"com.lihaoyi::ujson::${Versions.upickle}"
      val upickle                  = mvn"com.lihaoyi::upickle::${Versions.upickle}"
      val upickleJvm               = mvn"com.lihaoyi::upickle:${Versions.upickle}"
      val `upickle-core`           = mvn"com.lihaoyi::upickle-core::${Versions.upickle}"
      def upickleManaged: Seq[Dep] = Seq(ujson, upickle, `upickle-core`)
    }
  }
  object io {
    object getkyo {
      val `kyo-config`        = mvn"io.getkyo::kyo-config::${Versions.kyo}"
      val `kyo-core`          = mvn"io.getkyo::kyo-core::${Versions.kyo}"
      val `kyo-data`          = mvn"io.getkyo::kyo-data::${Versions.kyo}"
      val `kyo-http`          = mvn"io.getkyo::kyo-http::${Versions.kyo}"
      val `kyo-jsonrpc`       = mvn"io.getkyo::kyo-jsonrpc::${Versions.kyo}"
      val `kyo-prelude`       = mvn"io.getkyo::kyo-prelude::${Versions.kyo}"
      val `kyo-schema`        = mvn"io.getkyo::kyo-schema::${Versions.kyo}"
      val `kyo-schema-json`   = mvn"io.getkyo::kyo-schema-json::${Versions.kyo}"
      val `kyo-schema-yaml`   = mvn"io.getkyo::kyo-schema-yaml::${Versions.kyo}"
      val `kyo-system`        = mvn"io.getkyo::kyo-system::${Versions.kyo}"
      val `kyo-ui`            = mvn"io.getkyo::kyo-ui::${Versions.kyo}"
      val `kyo-zio`           = mvn"io.getkyo::kyo-zio::${Versions.kyo}"
      val `kyo-test-api`      = mvn"io.getkyo::kyo-test-api::${Versions.kyo}"
      val `kyo-test-prop`     = mvn"io.getkyo::kyo-test-prop::${Versions.kyo}"
      val `kyo-test-runner`   = mvn"io.getkyo::kyo-test-runner::${Versions.kyo}"
      val `kyo-test-snapshot` = mvn"io.getkyo::kyo-test-snapshot::${Versions.kyo}"

      /** Every kyo artifact this build may resolve, all at [[Versions.kyo]]. */
      def managed: Seq[Dep] = Seq(
        `kyo-config`,
        `kyo-core`,
        `kyo-data`,
        `kyo-http`,
        `kyo-jsonrpc`,
        `kyo-prelude`,
        `kyo-schema`,
        `kyo-schema-json`,
        `kyo-schema-yaml`,
        `kyo-system`,
        `kyo-ui`,
        `kyo-zio`,
        `kyo-test-api`,
        `kyo-test-prop`,
        `kyo-test-runner`,
        `kyo-test-snapshot`
      )
    }
  }
  object org {
    object apache {
      object commons {
        val `commons-compress` = mvn"org.apache.commons:commons-compress:${Versions.`commons-compress`}"
      }
    }

    object `scala-lang` {
      def `scala-compiler`(scalaVersion: String): Dep =
        if (scalaVersion.startsWith("3")) mvn"org.scala-lang::scala3-compiler:$scalaVersion"
        else mvn"org.scala-lang:scala-compiler:$scalaVersion"
      def `scala-library`(scalaVersion: String): Dep          = mvn"org.scala-lang:scala-library:$scalaVersion"
      def `scala-reflect`(scalaVersion: String): Dep          = mvn"org.scala-lang:scala-reflect:$scalaVersion"
      def `scala3-compiler`(scalaVersion: String): Dep        = mvn"org.scala-lang::scala3-compiler:$scalaVersion"
      def `scala3-tasty-inspector`(scalaVersion: String): Dep =
        mvn"org.scala-lang::scala3-tasty-inspector::$scalaVersion"
    }
  }

  /**
   * Artifacts that share a [[Versions]] pin with at least one sibling. Mill `depManagement` applies this so YAML can
   * omit those versions. When a suite gains a member, add it to that object's `managed`.
   */
  def managedSuites: Seq[Dep] =
    com.lihaoyi.upickleManaged ++
      io.getkyo.managed
}

object Versions {
  val caliban            = "3.1.5"
  val `commons-compress` = "1.28.0"
  val jmh                = "1.37"
  val kyo                = "1.0.0-RC7"
  val `os-lib`           = "0.11.10"
  val scoverage          = "2.5.2"
  val semanticDb         = "4.8.11"
  val sourcecode         = "0.4.4"
  val upickle            = "4.4.3"
  val utest              = "0.9.5"
}

object ScalaVersions {
  val scala3 = "3.9.0"

  def scalaJSVersion     = "1.22.0"
  def scalaNativeVersion = "0.5.12"

  /**
   * The Scala version Mill itself is built with. Mill plugins compile with this, not [[scala3]]: a plugin compiled with
   * a newer Scala emits TASTy that the consumer's Mill metabuild compiler cannot read. Move it with `.mill-version`.
   */
  def millScalaVersion = "3.8.4"

  // Backwards compatibility aliases
  def scala3x = scala3
}

object MillVersions {
  val all            = Seq("1.2.0-RC1-46-16168f")
  val oldest         = all.head
  val platformSuffix = "_mill1"
}
