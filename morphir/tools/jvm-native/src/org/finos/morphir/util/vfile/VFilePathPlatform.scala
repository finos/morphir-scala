package org.finos.morphir.util.vfile

import fs2.io.file.Path
import java.nio.file.{Path => JPath, Paths}

trait VFilePathPlatformSpecific
trait VFilePathCompanionPlatformSpecific {
  def apply(path: JPath): VFilePath       = VFilePath(Path.fromNioPath(path))
  def fromNioPath(path: JPath): VFilePath = VFilePath(Path.fromNioPath(path))
  def userHome: VFilePath                 = VFilePath(Path(System.getProperty("user.home")))
}
