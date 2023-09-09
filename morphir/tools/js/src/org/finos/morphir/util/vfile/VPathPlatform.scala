package org.finos.morphir.util.vfile

import fs2.io.file.Path
import java.nio.file.{Path => JPath, Paths}
import fs2.io.platform.js.PathHelper

trait VPathPlatformSpecific
trait VPathCompanionPlatformSpecific {

  def userHome: VPath = VPath(Path(PathHelper.homeDir))
}
