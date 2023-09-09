package org.finos.morphir.util.vfile

import cats.kernel.Monoid
import cats.syntax.all._
import fs2.io.file.Path
object VPath extends VPathCompanionPlatformSpecific {
  val empty: VPath                       = new VPath(Monoid[Path].empty)
  def combine(x: VPath, y: VPath): VPath = new VPath(x.path / y.path)
  def compare(x: VPath, y: VPath): Int   = x.path.compare(y.path)
}

final case class VPath private[vfile](path: Path) extends Product with Serializable {

  def /(name: String): VPath = new VPath(path / name)

  def /(that: VPath): VPath = VPath(path / that.path)

  def absolute: VPath                      = VPath(path.absolute)
  def elements: List[String]               = path.names.map(_.toString).toList
  def endsWith(input: String): Boolean = path.endsWith(input)
  def endsWith(that: VPath): Boolean       = path.endsWith(that.path)
  def ext: String                          = path.extName
  def fileName: VPath                      = VPath(path.fileName)
  def fileNameString: String               = path.fileName.toString
  def isAbsolute: Boolean                  = path.isAbsolute

  def normalize: VPath                    = VPath(path.normalize)
  def parent: Option[VPath]               = path.parent.map(VPath(_))
  def resolve(name: String): VPath        = VPath(path.resolve(name))
  def resolve(that: VPath): VPath         = VPath(path.resolve(that.path))
  def resolveSibling(name: String): VPath = VPath(path.resolveSibling(name))
  def resolveSibling(that: VPath): VPath  = VPath(path.resolveSibling(that.path))
  def startsWith(that: String): Boolean   = path.startsWith(that)
  def startsWith(that: VPath): Boolean    = path.startsWith(that.path)

  override def toString: String = path.toString
}


