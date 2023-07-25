package millbuild.crossplatform
import mill._

object DevMode {
  val devMode: Boolean = System.getenv("MORPHIR_SCALA_DEV_MODE") == "true"
}
