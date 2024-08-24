package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import com.typesafe.config.ConfigFactory
import zio.Config

final case class ScalaNativeBuildSettings(
    enable: Boolean = false,
    version: String = ScalaNativeBuildSettings.defaultVersion
)

object ScalaNativeBuildSettings {
  val config: Config[ScalaNativeBuildSettings] = deriveConfig[ScalaNativeBuildSettings]
  lazy val default: ScalaNativeBuildSettings   = ScalaNativeBuildSettings()

  lazy val defaultVersion = "0.4.17"

  implicit lazy val rw: upickle.default.ReadWriter[ScalaNativeBuildSettings] = upickle.default.macroRW
}
