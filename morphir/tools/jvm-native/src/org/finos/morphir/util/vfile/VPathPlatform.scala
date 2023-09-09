package org.finos.morphir.util.vfile

import fs2.io.file.Path
import java.nio.file.{Path => JPath, Paths}

trait VPathPlatformSpecific
trait VPathCompanionPlatformSpecific {
  def apply(path: JPath): VPath       = VPath(Path.fromNioPath(path))
  def fromNioPath(path: JPath): VPath = VPath(Path.fromNioPath(path))
  def userHome: VPath                 = VPath(Path(System.getProperty("user.home")))
}
