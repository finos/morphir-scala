package org.finos.morphir.util.vfile

import fs2.io.file.Path
import fs2.io.platform.js.PathHelper

abstract class VPathCompanionPlatformSpecific {

  def userHome: VPath = VPath(Path(PathHelper.homeDir))
}
