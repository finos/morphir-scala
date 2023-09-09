package org.finos.morphir.config.lang.elm
import org.finos.morphir.util.vfile.VFilePath

final case class ElmConfig(elmHome: VFilePath)
object ElmConfig {
  val default: ElmConfig = {
    val elmHome = VFilePath.userHome / ".elm"
    ElmConfig(elmHome = elmHome)
  }
}
