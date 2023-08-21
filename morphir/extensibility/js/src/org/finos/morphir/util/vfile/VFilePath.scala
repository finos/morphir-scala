package org.finos.morphir.util.vfile

import java.net.URI

sealed trait VFilePath { self =>
  lazy val fullPath: String = self match {
    case VFilePath.FromURI(path) => path.toString
  }
  final override def toString: String = fullPath
}

object VFilePath {
  def apply(pathURI: URI): VFilePath = FromURI(pathURI)
  final case class FromURI(path: URI) extends VFilePath
}
