package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.config.yaml.*
import com.typesafe.config.ConfigFactory
import zio.Config
import upickle.default.*

final case class ScalaNativeBuildSettings(
    enable: Boolean = false,
    version: String = ScalaNativeBuildSettings.defaultVersion
) derives ReadWriter

object ScalaNativeBuildSettings {
  val config: Config[ScalaNativeBuildSettings] = deriveConfig[ScalaNativeBuildSettings]
  lazy val default: ScalaNativeBuildSettings   = ScalaNativeBuildSettings()

  lazy val defaultVersion = "0.4.17"
}
