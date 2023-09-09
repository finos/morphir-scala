package org.finos.morphir.util.vfile

import fs2.io.file.Path

abstract class VPathCompanionApi {
  def apply(path: java.nio.file.Path): VPath       = VPath(Path.fromNioPath(path))
  def fromNioPath(path: java.nio.file.Path): VPath = VPath(Path.fromNioPath(path))
  def userHome: VPath                              = VPath(Path(System.getProperty("user.home")))
}
