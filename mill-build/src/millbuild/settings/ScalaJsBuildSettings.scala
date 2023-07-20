package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import com.typesafe.config.ConfigFactory
import zio.Config

final case class ScalaJsBuildSettings(enable: Boolean = true, version: String = ScalaJsBuildSettings.defaultVersion)

object ScalaJsBuildSettings {
  val config                             = deriveConfig[ScalaJsBuildSettings]
  lazy val default: ScalaJsBuildSettings = ScalaJsBuildSettings()
  lazy val defaultVersion                = "1.13.1"

  implicit lazy val rw: upickle.default.ReadWriter[ScalaJsBuildSettings] = upickle.default.macroRW
}
