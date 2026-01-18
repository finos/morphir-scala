package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.*
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.config.yaml.*
import com.typesafe.config.ConfigFactory
import zio.Config
import upickle.default.*

final case class JvmBuildSettings(enable: Boolean = true) derives ReadWriter

object JvmBuildSettings {
  val config                         = deriveConfig[JvmBuildSettings]
  lazy val default: JvmBuildSettings = JvmBuildSettings()
}
