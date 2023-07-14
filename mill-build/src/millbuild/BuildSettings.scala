package millbuild
import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._

final case class BuildSettings(
    jvm: JvmBuildSettings = JvmBuildSettings(),
    js: ScalaJsBuildSettings = ScalaJsBuildSettings(),
    native: ScalaNativeBuildSettings = ScalaNativeBuildSettings()
)

object BuildSettings {
  lazy val default: BuildSettings = BuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[BuildSettings] = upickle.default.macroRW

  lazy val buildSettingsConfig = deriveConfig[BuildSettings]

  lazy val buildUserHoconFileSource =
    ConfigProvider.fromHoconFile((os.pwd / "build.user.conf").toIO)

  lazy val buildUserYamlFileSource =
    ConfigProvider.fromYamlPath((os.pwd / "build.user.yaml").wrapped)

  lazy val hoconFallbackSource = ConfigProvider.fromHoconString(
    """
      |jvm {
      |  enable = true
      |}
      |js {
      |  enable = true
      |}
      |native {
      |  enable = true
      |}
      |""".stripMargin
  )

  def load(): BuildSettings = Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(
      loadSettings()
    ).getOrThrowFiberFailure()
  }

  def loadSettings() =
    read(buildSettingsConfig from (buildUserHoconFileSource orElse buildUserYamlFileSource orElse hoconFallbackSource))

}

final case class JvmBuildSettings(enable: Boolean = true)
object JvmBuildSettings {
  lazy val default: JvmBuildSettings = JvmBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[JvmBuildSettings] = upickle.default.macroRW
}

final case class ScalaJsBuildSettings(enable: Boolean = true)
object ScalaJsBuildSettings {
  lazy val default: ScalaJsBuildSettings = ScalaJsBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[ScalaJsBuildSettings] = upickle.default.macroRW
}

final case class ScalaNativeBuildSettings(enable: Boolean = true)
object ScalaNativeBuildSettings {
  lazy val default: ScalaNativeBuildSettings = ScalaNativeBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[ScalaNativeBuildSettings] = upickle.default.macroRW
}
