package org.finos.morphir.util.vfile

import fs2.io.file.Path
import java.nio.file.{Path => JPath, Paths}

trait VPathPlatformSpecific
trait VPathCompanionPlatformSpecific {
  def userHome: VPath = VPath(Path(System.getProperty("user.home")))
}
