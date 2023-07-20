package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import com.typesafe.config.ConfigFactory
import zio.Config

final case class JvmBuildSettings(enable: Boolean = true)

object JvmBuildSettings {
  val config                         = deriveConfig[JvmBuildSettings]
  lazy val default: JvmBuildSettings = JvmBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[JvmBuildSettings] = upickle.default.macroRW
}