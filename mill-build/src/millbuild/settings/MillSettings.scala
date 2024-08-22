package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.Config

case class MillSettings(
    scalaVersion: String = MillSettings.defaultScalaVersion
)

object MillSettings {
  val config: Config[MillSettings]                          = deriveConfig[MillSettings]
  lazy val default: MillSettings                            = MillSettings()
  lazy val defaultScalaVersion                              = "2.13.13"
  implicit val rw: upickle.default.ReadWriter[MillSettings] = upickle.default.macroRW
}
