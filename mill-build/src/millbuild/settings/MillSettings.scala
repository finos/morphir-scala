package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.Config
import upickle.default.*

case class MillSettings(
    scalaVersion: String = MillSettings.defaultScalaVersion
) derives ReadWriter

object MillSettings {
  val config: Config[MillSettings]   = deriveConfig[MillSettings]
  lazy val default: MillSettings     = MillSettings()
  lazy val defaultScalaVersion       = "2.13.16"
}
