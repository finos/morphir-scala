package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.Config

final case class SbtSettings(scalaVersion: String = SbtSettings.defaultScalaVersion)

object SbtSettings {
  val config: Config[SbtSettings]                          = deriveConfig[SbtSettings]
  lazy val default: SbtSettings                            = SbtSettings()
  lazy val defaultScalaVersion                             = "2.12.8"
  implicit val rw: upickle.default.ReadWriter[SbtSettings] = upickle.default.macroRW
}
