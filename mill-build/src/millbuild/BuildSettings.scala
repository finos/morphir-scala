package millbuild
import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config._
import zio.config.magnolia.deriveConfig
import zio.config.typesafe._
import zio.config.yaml._
import com.typesafe.config.ConfigFactory
import zio.Config

final case class BuildSettings(
    jvm: JvmBuildSettings = JvmBuildSettings(),
    js: ScalaJsBuildSettings = ScalaJsBuildSettings(),
    native: ScalaNativeBuildSettings = ScalaNativeBuildSettings()
)

object BuildSettings {
  val config: Config[BuildSettings] = deriveConfig[BuildSettings]
  lazy val default: BuildSettings   = BuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[BuildSettings] = upickle.default.macroRW

  lazy val buildUserHoconFileConfigProvider: ConfigProvider =
    ConfigProvider.fromHoconFile((os.pwd / "build.user.conf").toIO)

  lazy val buildEnvConfigProvider: ConfigProvider =
    ConfigProvider.envProvider.nested("morphir_build")

  lazy val propertiesFileConfigProvider: ConfigProvider =
    ConfigProvider.propsProvider.nested("morphir.build")

    // .fromPropertiesFile((os.pwd / "build.user.properties").toIO)

  // lazy val buildUserYamlFileSource =
  //   ConfigProvider.fromYamlPath((os.pwd / "build.user.yaml").wrapped)

  lazy val hoconFallbackSource: ConfigProvider = ConfigProvider.fromHoconString(
    """
      |jvm {
      |  enable = true
      |}
      |js {
      |  enable = false
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
    read(
      BuildSettings.config from (
        buildEnvConfigProvider orElse propertiesFileConfigProvider orElse
          buildUserHoconFileConfigProvider /* orElse buildUserYamlFileSource  orElse hoconFallbackSource*/
      )
    )

}

final case class JvmBuildSettings(enable: Boolean = true)
object JvmBuildSettings {
  val config                         = deriveConfig[JvmBuildSettings]
  lazy val default: JvmBuildSettings = JvmBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[JvmBuildSettings] = upickle.default.macroRW
}

final case class ScalaJsBuildSettings(enable: Boolean = true)
object ScalaJsBuildSettings {
  val config                             = deriveConfig[ScalaJsBuildSettings]
  lazy val default: ScalaJsBuildSettings = ScalaJsBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[ScalaJsBuildSettings] = upickle.default.macroRW
}

final case class ScalaNativeBuildSettings(enable: Boolean = true)
object ScalaNativeBuildSettings {
  val config                                 = deriveConfig[ScalaNativeBuildSettings]
  lazy val default: ScalaNativeBuildSettings = ScalaNativeBuildSettings()

  implicit lazy val rw: upickle.default.ReadWriter[ScalaNativeBuildSettings] = upickle.default.macroRW
}
