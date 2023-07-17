package org.finos.morphir.universe.ir

import scala.annotation.tailrec
import zio.prelude.*
import fansi.Str
final case class Path(toList: List[Name]) extends PathLike {
  self =>
  def render(implicit renderer: Path.Renderer): String = renderer(self)
  def render(separator: String)(implicit nameRenderer: Name.Renderer): String =
    render(Path.Renderer(separator, nameRenderer))
}

object Path {
  val empty: Path = Path(List.empty)

  def apply(first: String, rest: String*): Path =
    wrap((first +: rest).map(Name.fromString).toList)

  def apply(first: Name, rest: Name*): Path =
    if (rest.isEmpty) wrap(List(first))
    else wrap((first +: rest).toList)

  private[morphir] def wrap(value: List[Name]): Path = Path(value)

  private[morphir] def wrap(value: Array[Name]): Path = Path(value.toList)

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    wrap(separatorRegex.split(str).map(Name.fromString).toList)
  }

  def toString(f: Name => String, separator: String, path: Path): String =
    path.toString(f, separator)

  @inline def fromIterable(names: Iterable[Name]): Path = Path(names.toList)
  @inline def fromList(names: List[Name]): Path         = wrap(names)

  @inline def toList(path: Path): List[Name] = path.toList.toList

  /** Checks if the first provided path is a prefix of the second path */
  @tailrec
  def isPrefixOf(prefix: PathLike, path: PathLike): Boolean = (prefix.toList, path.toList) match {
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
