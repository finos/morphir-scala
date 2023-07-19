package org.finos.morphir.ir

import org.finos.morphir.datamodel.namespacing.*
import zio.Chunk
import zio.prelude.*

import scala.annotation.tailrec

import Module.{QualifiedModuleName, ModuleName}

final case class Path(segments: Chunk[Name]) { self =>

  def ++(that: Path): Path = Path(segments ++ that.segments)

  /** Constructs a new path by combining this path with the given name. */
  def /(name: Name): Path = Path(segments ++ Chunk(name))

  /** Constructs a new path by combining this path with the given path. */
  def /(that: Path): Path = Path(segments ++ that.segments)
  // def %(other: Path): PackageAndModulePath =
  //   PackageAndModulePath(PackageName(self), ModulePath(other))

  // def %(name: Name): ModuleName = ModuleName(self, name)
  def ::(name: Name): QName = QName(self, name)

  /** Indicates whether this path is empty. */
  def isEmpty: Boolean               = segments.isEmpty
  def zip(other: Path): (Path, Path) = (self, other)

  def render(implicit renderer: Path.Renderer): String = renderer(self)
  def render(separator: String)(implicit nameRenderer: Name.Renderer): String =
    render(Path.Renderer(separator, nameRenderer))

  def toList: List[Name] = segments.toList

  def toNamespace(implicit renderer: Name.Renderer = Name.Renderer.TitleCase): Namespace = {
    val nsSegments = Namespace.segments(segments.map(_.render))
    Namespace.fromIterable(nsSegments)
  }

  def toString(f: Name => String, separator: String): String =
    segments.map(f).mkString(separator)

  /** Checks if this path is a prefix of provided path */
  def isPrefixOf(path: Path): Boolean = Path.isPrefixOf(self, path)
}

object Path {
  val empty: Path = Path(Chunk.empty)

  def apply(first: String, rest: String*): Path =
    wrap((first +: rest).map(Name.fromString).toList)

  def apply(first: Name, rest: Name*): Path =
    wrap((first +: rest).toList)

  private[morphir] def wrap(value: List[Name]): Path  = Path(Chunk.fromIterable(value))
  private[morphir] def wrap(value: Array[Name]): Path = Path(Chunk.fromIterable(value))

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    wrap(separatorRegex.split(str).map(Name.fromString).toList)
  }

  def toString(f: Name => String, separator: String, path: Path): String =
    path.toString(f, separator)

  @inline def fromList(names: List[Name]): Path = wrap(names)
  def fromIterable(names: Iterable[Name]): Path = Path(Chunk.fromIterable(names))

  @inline def toList(path: Path): List[Name] = path.segments.toList

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

  private[morphir] def unsafeMake(parts: Name*): Path = Path(Chunk.fromIterable(parts))

  final case class Renderer(separator: String, nameRenderer: Name.Renderer) extends (Path => String) {
    def apply(path: Path): String        = path.toString(nameRenderer, separator)
    final def render(path: Path): String = apply(path)
  }

  object Renderer {
    val CamelCase: Renderer = Renderer(".", Name.Renderer.CamelCase)
    val KebabCase: Renderer = Renderer(".", Name.Renderer.KebabCase)
    val SnakeCase: Renderer = Renderer(".", Name.Renderer.SnakeCase)
    val TitleCase: Renderer = Renderer(".", Name.Renderer.TitleCase)
  }
}
