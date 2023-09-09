package org.finos.morphir.util.vfile

import fs2.io.file.Path
import java.nio.file.{Path => JPath, Paths}

trait VFilePathPlatformSpecific
trait VFilePathCompanionPlatformSpecific {
  def userHome: VFilePath = VFilePath(Path(System.getProperty("user.home")))
}
