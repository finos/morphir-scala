package org.finos.morphir.util.vfile
import cats.kernel.Monoid
import cats.syntax.all._
import fs2.io.file.Path
final case class VPath(path: Path) extends VPathPlatformSpecific { self =>

  /**
   * Joins the given path segments together using the platform-specific separator as a delimiter, then normalizes the
   * resulting path.
   */
  def /(name: String): VPath = VPath(path / name)

  /**
   * Joins the given path segments together using the platform-specific separator as a delimiter, then normalizes the
   * resulting path.
   */
  def /(path: VPath): VPath = VPath(self.path / path.path)

  /** Returns a VPath object representing the absolute path of this path. */
  def absolute: VPath                 = VPath(path.absolute)
  def elements: List[String]          = path.names.map(_.toString).toList
  def endsWith(path: String): Boolean = self.path.endsWith(path)
  def endsWith(path: VPath): Boolean  = self.path.endsWith(path.path)
  def ext: String                     = path.extName
  def fileName: VPath                 = VPath(path.fileName)
  def fileNameString: String          = path.fileName.toString
  def isAbsolute: Boolean             = path.isAbsolute

  def normalize: VPath                    = VPath(path.normalize)
  def parent: Option[VPath]               = path.parent.map(VPath(_))
  def resolve(name: String): VPath        = VPath(path.resolve(name))
  def resolve(path: VPath): VPath         = VPath(self.path.resolve(path.path))
  def resolveSibling(name: String): VPath = VPath(path.resolveSibling(name))
  def resolveSibling(path: VPath): VPath  = VPath(self.path.resolveSibling(path.path))
  def startsWith(path: String): Boolean   = self.path.startsWith(path)
  def startsWith(path: VPath): Boolean    = self.path.startsWith(path.path)

  override def toString: String = path.toString
  override def hashCode(): Int  = path.hashCode()
}

object VPath extends VPathCompanionPlatformSpecific {
  val empty: VPath                       = VPath(Monoid[Path].empty)
  def combine(x: VPath, y: VPath): VPath = VPath(x.path / y.path)
  def compare(x: VPath, y: VPath): Int   = x.path.compare(y.path)
}
