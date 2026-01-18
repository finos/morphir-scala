package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.config.yaml.*
import com.typesafe.config.ConfigFactory
import zio.Config
import millbuild.crossplatform.DevMode
import upickle.default.*

final case class ScalaSettings(
    defaultVersion: String = ScalaSettings.defaultVersion,
    scala213Version: String = ScalaSettings.defaultScala213Version,
    scala3xVersion: String = ScalaSettings.defaultScala3xVersion,
    crossScalaVersions: List[String] = ScalaSettings.defaultCrossScalaVersions
) derives ReadWriter

object ScalaSettings {
  import DevMode.*

  val config: Config[ScalaSettings] = deriveConfig[ScalaSettings]
  lazy val default: ScalaSettings   = ScalaSettings()
  lazy val defaultVersion           = defaultScala3xVersion

  val defaultScala213Version = "2.13.16"
  val defaultScala3xVersion  = "3.7.4"
  val defaultCrossScalaVersions: List[String] =
    if (devMode) List(defaultScala3xVersion) else List(defaultScala3xVersion)
}
