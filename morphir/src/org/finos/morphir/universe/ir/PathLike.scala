package org.finos.morphir.universe.ir

import scala.annotation.tailrec

private[morphir] trait PathLike {
  self =>

  def ++(that: Path): Path  = Path(toList ++ that.toList)
  def ::(name: Name): QName = QName(self.toPath, name)

  /** Indicates whether this path is empty. */
  def isEmpty: Boolean = toList.isEmpty

  def toList: List[Name]

  def toPath: Path =
    self match {
      case path: Path => path
      case _          => Path(self.toList)
    }

  /** Constructs a new path by combining this path with the given name. */
  def /(name: Name): Path = Path(toList ++ List(name))

  /** Constructs a new path by combining this path with the given path. */
  def /(that: PathLike): Path = Path(toList ++ that.toList)
  // def %(other: Path): PackageAndModulePath =
  //   PackageAndModulePath(PackageName(self), ModulePath(other))

  def zip(other: Path): (Path, Path) = (self.toPath, other)

  def toString(f: Name => String, separator: String): String =
    toList.map(f).mkString(separator)

  /** Checks if this path is a prefix of provided path */
  def isPrefixOf(path: PathLike): Boolean = Path.isPrefixOf(self, path)
}
