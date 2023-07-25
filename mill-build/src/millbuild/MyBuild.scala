package millbuild
import mill._
import mill.define.ExternalModule
import millbuild.settings._

object MyBuild extends ExternalModule {

  lazy val cachedBuildSettings = BuildSettings.load()

  def buildSettings = T.input {
    BuildSettings.load()
  }

  def showBuildSettings() = T.command {
    val settings = buildSettings()
    pprint.pprintln(settings)
    settings
  }

  lazy val millDiscover = mill.define.Discover[this.type]
}
