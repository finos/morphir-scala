package millbuild
import mill.*
import mill.api.ExternalModule
import millbuild.settings.*

object MyBuild extends ExternalModule {

  lazy val cachedBuildSettings = BuildSettings.load()

  def buildSettings = Task.Input {
    BuildSettings.load()
  }

  def devMode = Task.Input { Task.env.getOrElse("MORPHIR_SCALA_DEV_MODE", false) == "true" }

  def showBuildSettings() = Task.Command {
    val settings = buildSettings()
    pprint.pprintln(settings)
    settings
  }

  lazy val millDiscover = mill.api.Discover[this.type]
}
