package org.finos
package morphir
package ir
package module

import org.finos.morphir.ir.{Name, Path, QName}
import zio.Chunk

final case class QualifiedModuleName(namespace: Path, localName: Name) {
  def %(name: Name): QName = QName(toPath, name)

  def toModuleName: ModuleName = ModuleName(toPath)

  lazy val toPath: Path         = namespace / localName
  override def toString: String = toPath.toString
}

object QualifiedModuleName {
  def fromPath(path: Path): QualifiedModuleName = path.segments match {
    case Chunk()     => QualifiedModuleName(Path.empty, Name.empty)
    case Chunk(name) => QualifiedModuleName(Path.empty, name)
    case ns :+ name  => QualifiedModuleName(Path(ns), name)
    case names =>
      val ns   = names.take(names.length - 1)
      val name = names.last
      QualifiedModuleName(Path(ns), name)
  }

  def fromString(input: String): QualifiedModuleName = fromPath(Path.fromString(input))

  implicit def toModuleName(qualifiedModuleName: QualifiedModuleName):ModuleName = qualifiedModuleName.toModuleName

  private[morphir] def unsafeMake(namespace: String*)(nameSegments: String*): QualifiedModuleName = {
    val ns        = namespace.foldLeft(Path.empty) { case (path, pathStr) => path / Path.fromString(pathStr) }
    val localName = Name.unsafeMake(nameSegments: _*)
    QualifiedModuleName(ns, localName)
  }
}
