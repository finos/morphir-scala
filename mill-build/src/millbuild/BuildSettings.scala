package millbuild
import metaconfig._
import metaconfig.sconfig._
import metaconfig.Configured.NotOk
import metaconfig.Configured.Ok

final case class BuildSettings(
    enableJVM: Boolean = true,
    enableScalaJS: Boolean = true,
    enableScalaNative: Boolean = true
)

object BuildSettings {
  lazy val default: BuildSettings = BuildSettings()
  implicit lazy val surface: generic.Surface[BuildSettings] =
    generic.deriveSurface[BuildSettings]
  implicit lazy val decoder: ConfDecoder[BuildSettings] =
    generic.deriveDecoder[BuildSettings](default)
  implicit lazy val decoderEx: ConfDecoderEx[BuildSettings] =
    generic.deriveDecoderEx[BuildSettings](default)
  implicit lazy val encoder: ConfEncoder[BuildSettings] =
    generic.deriveEncoder[BuildSettings]

  implicit lazy val rw: upickle.default.ReadWriter[BuildSettings] = upickle.default.macroRW

  def load(): BuildSettings = {
    val userConfPath = os.pwd / "build.user.conf"
    if (os.exists(userConfPath)) {
      Conf.parseInput(Input.File(userConfPath.wrapped)) match {
        case NotOk(error) => default
        case Ok(conf)     => BuildSettings.decoderEx.read(Option(default), conf).getOrElse(default)
      }
    } else {
      default
    }
  }

}
