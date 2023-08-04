package org.finos.morphir.universe.ir

import org.finos.morphir.datamodel.namespacing.*
import scala.annotation.tailrec
import zio.prelude.*
import fansi.Str
import zio.Chunk
final case class Path(segments: IndexedSeq[Name]) extends PathLike {
  self =>

  def chunks: Chunk[Name] = Chunk.fromIterable(segments)

  def render(implicit renderer: Path.Renderer): String = renderer(self)
  def render(separator: String)(implicit nameRenderer: Name.Renderer): String =
    render(Path.Renderer(separator, nameRenderer))

  def toList: List[Name] = segments.toList

  def toPackageName(implicit renderer: Name.Renderer = Name.Renderer.TitleCase): PackageName = {
    val nsSegments = PackageName.segments(segments.map(_.render))
    PackageName.fromIterable(nsSegments)
  }

  def toNamespace(implicit renderer: Name.Renderer = Name.Renderer.TitleCase): Namespace = {
    val nsSegments = Namespace.segments(segments.map(_.render))
    Namespace.fromIterable(nsSegments)
  }
}

object Path {
  val empty: Path = Path(IndexedSeq.empty)

  def apply(first: String, rest: String*): Path =
    wrap((first +: rest).map(Name.fromString).toList)

  def apply(first: Name, rest: Name*): Path =
    if (rest.isEmpty) wrap(List(first))
    else wrap((first +: rest).toList)

  private[morphir] def wrap(value: IndexedSeq[Name]): Path = Path(value)
  private[morphir] def wrap(value: List[Name]): Path       = Path(value.toIndexedSeq)

  private[morphir] def wrap(value: Array[Name]): Path = Path(value.toIndexedSeq)

  def fromString(str: String): Path = {
    val separatorRegex = """[^\w\s]+""".r
    wrap(separatorRegex.split(str).map(Name.fromString).toList)
  }

  def toString(f: Name => String, separator: String, path: Path): String =
    path.toString(f, separator)

  @inline def fromArray(names: Array[Name]): Path       = Path(names.toIndexedSeq)
  @inline def fromIterable(names: Iterable[Name]): Path = Path(names.toIndexedSeq)
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

  private[morphir] def unsafeMake(parts: Name*): Path = Path(parts.toIndexedSeq)

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
