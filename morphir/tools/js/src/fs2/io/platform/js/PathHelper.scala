package fs2.io.platform.js

object PathHelper {
  val homeDir: String = fs2.io.internal.facade.os.homedir()
}
