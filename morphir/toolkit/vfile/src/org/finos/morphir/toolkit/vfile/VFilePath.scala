package org.finos.morphir.toolkit.vfile

import java.net.URI
import java.nio.file.{Path, Paths}

sealed trait VFilePath { self =>
  lazy val fullPath: String = self match {
    case VFilePath.FromPath(path) => path.toString
    case VFilePath.FromURI(path)  => Paths.get(path).toString
  }
  final override def toString: String = fullPath
}

object VFilePath {
  def apply(path: Path): VFilePath   = FromPath(path)
  def apply(pathURI: URI): VFilePath = FromURI(pathURI)
  final case class FromPath(path: Path) extends VFilePath
  final case class FromURI(path: URI)   extends VFilePath

}
