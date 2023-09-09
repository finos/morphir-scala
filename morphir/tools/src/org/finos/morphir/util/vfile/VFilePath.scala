package org.finos.morphir.util.vfile

import fs2.io.file.Path
final case class VFilePath(path: Path) extends VFilePathPlatformSpecific { self =>

  /**
   * Joins the given path segments together using the platform-specific separator as a delimiter, then normalizes the
   * resulting path.
   */
  def /(name: String): VFilePath = VFilePath(path / name)

  def elements: List[String] = path.names.map(_.toString).toList
}

object VFilePath extends VFilePathCompanionPlatformSpecific {
  def combine(x: VFilePath, y: VFilePath): VFilePath = VFilePath(x.path / y.path)
}
