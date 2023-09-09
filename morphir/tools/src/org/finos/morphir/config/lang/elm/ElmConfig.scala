package org.finos.morphir.config.lang.elm
import org.finos.morphir.util.vfile.VPath

final case class ElmConfig(elmHome: VPath)
object ElmConfig {
  val default: ElmConfig = {
    val elmHome = VPath.userHome / ".elm"
    ElmConfig(elmHome = elmHome)
  }
}
