package millbuild.settings

import zio.{ConfigProvider, Unsafe, Runtime}
import zio.config.{read as zioRead, *}
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*
import zio.config.yaml.*
import com.typesafe.config.ConfigFactory
import zio.Config
import upickle.default.{ReadWriter, Reader, Writer}

final case class BuildSettings(
    jvm: JvmBuildSettings = JvmBuildSettings(),
    js: ScalaJsBuildSettings = ScalaJsBuildSettings(),
    native: ScalaNativeBuildSettings = ScalaNativeBuildSettings(),
    mill: MillSettings = MillSettings(),
    scala: ScalaSettings = ScalaSettings()
) derives ReadWriter

object BuildSettings {

  val config: Config[BuildSettings] = deriveConfig[BuildSettings]
  lazy val default: BuildSettings   = BuildSettings()

  lazy val buildUserHoconFileConfigProvider: ConfigProvider =
    ConfigProvider.fromHoconFile((os.pwd / "build.user.conf").toIO)

  lazy val buildEnvConfigProvider: ConfigProvider =
    ConfigProvider.envProvider.nested("morphir_build")

  lazy val propertiesFileConfigProvider: ConfigProvider =
    ConfigProvider.propsProvider.nested("morphir.build")

  // .fromPropertiesFile((os.pwd / "build.user.properties").toIO)

  lazy val buildUserYamlFileConfigProvider =
    ConfigProvider.fromYamlPath((os.pwd / "build.user.yaml").wrapped)

  def load(): BuildSettings = Unsafe.unsafe { implicit u =>
    Runtime.default.unsafe.run(
      loadSettings()
    ).getOrThrowFiberFailure()
  }

  def loadSettings() =
    zioRead(
      BuildSettings.config from (
        buildEnvConfigProvider orElse
          propertiesFileConfigProvider orElse
          buildUserHoconFileConfigProvider // orElse
          // buildUserYamlFileConfigProvider
      )
    )

}
