package org.finos.morphir.ir

import scala.annotation.tailrec
import Name.Name
import QName.QName
object Path {
  final case class Path(toList: List[Name]) {
    self =>

    def ++(that: Path): Path = Path(toList ++ that.toList)

    /** Constructs a new path by combining this path with the given name. */
    def /(name: Name): Path = Path(toList ++ List(name))

    /** Constructs a new path by combining this path with the given path. */
    def /(that: Path): Path = Path(toList ++ that.toList)
    // def %(other: Path): PackageAndModulePath =
    //   PackageAndModulePath(PackageName(self), ModulePath(other))

    // def %(name: Name): ModuleName = ModuleName(self, name)
    def ::(name: Name): QName = QName(self, name)

    /** Indicates whether this path is empty. */
    def isEmpty: Boolean = toList.isEmpty

    def zip(other: Path): (Path, Path) = (self, other)

    def toString(f: Name => String, separator: String): String =
      toList.map(f).mkString(separator)

    /** Checks if this path is a prefix of provided path */
    def isPrefixOf(path: Path): Boolean = Path.isPrefixOf(self, path)
  }

  object Path {
    val empty: Path = Path(List.empty)

    def apply(first: String, rest: String*): Path =
      wrap((first +: rest).map(Name.fromString).toList)

    def apply(first: Name, rest: Name*): Path =
      wrap((first +: rest).toList)

    private[morphir] def wrap(value: List[Name]): Path = Path(value)

    private[morphir] def wrap(value: Array[Name]): Path = Path(value.toList)

    def fromString(str: String): Path = {
      val separatorRegex = """[^\w\s]+""".r
      wrap(separatorRegex.split(str).map(Name.fromString).toList)
    }

    def toString(f: Name => String, separator: String, path: Path): String =
      path.toString(f, separator)

    @inline def fromList(names: List[Name]): Path = wrap(names)

    @inline def toList(path: Path): List[Name] = path.toList.toList

    /** Checks if the first provided path is a prefix of the second path */
    @tailrec
    def isPrefixOf(prefix: Path, path: Path): Boolean = (prefix.toList, path.toList) match {
      case (Nil, _) => true
      case (_, Nil) => false
      case (prefixHead :: prefixTail, pathHead :: pathTail) =>
        if (prefixHead == pathHead)
          isPrefixOf(
            Path.fromList(prefixTail),
            Path.fromList(pathTail)
          )
        else false
    }

    private[morphir] def unsafeMake(parts: Name*): Path = Path(parts.toList)
  }
}
