package millbuild
import mill.*
import mill.api.ExternalModule

object MyBuild extends ExternalModule {
  lazy val millDiscover = mill.api.Discover[this.type]
}
