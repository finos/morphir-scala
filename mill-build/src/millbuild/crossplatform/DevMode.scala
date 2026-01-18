package millbuild.crossplatform
import mill.*

object DevMode {
  val devMode: Boolean = System.getenv("MORPHIR_SCALA_DEV_MODE") == "true"
}
