package org.finos.morphir.util.vfile

import fs2.io.file.Path
final case class VPath(path: Path) extends VPathPlatformSpecific { self =>

  /**
   * Joins the given path segments together using the platform-specific separator as a delimiter, then normalizes the
   * resulting path.
   */
  def /(name: String): VPath = VPath(path / name)

  def elements: List[String]    = path.names.map(_.toString).toList
  override def toString: String = path.toString
  override def hashCode(): Int  = path.hashCode()
}

object VPath extends VPathCompanionPlatformSpecific {
  def combine(x: VPath, y: VPath): VPath = VPath(x.path / y.path)
}
