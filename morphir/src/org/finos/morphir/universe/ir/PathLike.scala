package org.finos.morphir.universe.ir

import scala.annotation.tailrec
import zio.Chunk
private[morphir] trait PathLike {
  self =>

  def ++(that: Path): Path  = Path(segments ++ that.segments)
  def ::(name: Name): QName = QName(self.toPath, name)

  /** Indicates whether this path is empty. */
  def isEmpty: Boolean = toList.isEmpty

  def toList: List[Name]

  def toPath: Path =
    self match {
      case path: Path => path
      case _          => Path(self.segments)
    }

  /** Constructs a new path by combining this path with the given name. */
  def /(name: Name): Path = Path(segments ++ List(name))

  /** Constructs a new path by combining this path with the given path. */
  def /(that: PathLike): Path = Path(segments ++ that.toList)
  // def %(other: Path): PackageAndModulePath =
  //   PackageAndModulePath(PackageName(self), ModulePath(other))

  def zip(other: Path): (Path, Path) = (self.toPath, other)

  def segments: IndexedSeq[Name]
  def chunks: Chunk[Name]

  def toString(f: Name => String, separator: String): String =
    toList.map(f).mkString(separator)

  /** Checks if this path is a prefix of provided path */
  def isPrefixOf(path: PathLike): Boolean = Path.isPrefixOf(self, path)
}
