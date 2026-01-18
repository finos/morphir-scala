package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.config.yaml.*
import com.typesafe.config.ConfigFactory
import zio.Config
import upickle.default.*

final case class ScalaJsBuildSettings(enable: Boolean = true, version: String = ScalaJsBuildSettings.defaultVersion) derives ReadWriter

object ScalaJsBuildSettings {
  val config                             = deriveConfig[ScalaJsBuildSettings]
  lazy val default: ScalaJsBuildSettings = ScalaJsBuildSettings()
  lazy val defaultVersion                = "1.16.0"
}
