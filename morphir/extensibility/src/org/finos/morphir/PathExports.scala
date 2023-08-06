package org.finos.morphir
import scala.annotation.tailrec

private[morphir] trait PathExports { self: NameExports =>

  final case class Path(segments: Vector[Name]) {
    self =>

    def ++(that: Path): Path = Path(segments ++ that.segments)
//  def ::(name: Name): QName = QName(self.toPath, name)

    /** Indicates whether this path is empty. */
    def isEmpty: Boolean = toList.isEmpty

    def toList: List[Name] = segments.toList

    def toPath: Path =
      self match {
        case path: Path => path
        case _          => Path(self.segments)
      }

    /** Constructs a new path by combining this path with the given name. */
    def /(name: Name): Path = Path(segments ++ List(name))

    /** Constructs a new path by combining this path with the given path. */
    def /(that: Path): Path = Path(segments ++ that.toList)
    // def %(other: Path): PackageAndModulePath =
    //   PackageAndModulePath(PackageName(self), ModulePath(other))

    def zip(other: Path): (Path, Path) = (self.toPath, other)

    def toString(f: Name => String, separator: String): String =
      toList.map(f).mkString(separator)

    /** Checks if this path is a prefix of provided path */
    def isPrefixOf(path: Path): Boolean                 = Path.isPrefixOf(self, path)
    def render(implicit renderer: PathRenderer): String = renderer(self)
    def render(separator: String)(implicit nameRenderer: NameRenderer): String =
      render(PathRenderer(separator, nameRenderer))

    // def toPackageName(implicit renderer: Name.Renderer = Name.Renderer.TitleCase): PackageName = {
    //   val nsSegments = PackageName.segments(segments.map(_.render))
    //   PackageName.fromIterable(nsSegments)
    // }

    // def toNamespace(implicit renderer: Name.Renderer = Name.Renderer.TitleCase): Namespace = {
    //   val nsSegments = Namespace.segments(segments.map(_.render))
    //   Namespace.fromIterable(nsSegments)
    // }
  }

  object Path {
    val empty: Path = Path(Vector.empty)

    def apply(first: String, rest: String*): Path =
      wrap((first +: rest).map(Name.fromString).toList)

    def apply(first: Name, rest: Name*): Path =
      if (rest.isEmpty) wrap(List(first))
      else wrap((first +: rest).toList)

    private[morphir] def wrap(value: Vector[Name]): Path = Path(value)
    private[morphir] def wrap(value: List[Name]): Path   = Path(value.toVector)
    private[morphir] def wrap(value: Array[Name]): Path  = Path(value.toVector)

    def fromString(str: String): Path = {
      val separatorRegex = """[^\w\s]+""".r
      fromArray(separatorRegex.split(str).map(Name.fromString))
    }

    def toString(f: Name => String, separator: String, path: Path): String =
      path.toString(f, separator)

    @inline def fromArray(names: Array[Name]): Path       = Path(names.toVector)
    @inline def fromIterable(names: Iterable[Name]): Path = Path(names.toVector)
    @inline def fromList(names: List[Name]): Path         = Path(names.toVector)

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

    private[morphir] def unsafeMake(parts: Name*): Path = Path(parts.toVector)
  }

  final case class PathRenderer(separator: String, nameRenderer: NameRenderer) extends (Path => String) {
    def apply(path: Path): String        = path.toString(nameRenderer, separator)
    final def render(path: Path): String = apply(path)
  }

  object Renderer {
    val CamelCase: PathRenderer = PathRenderer(".", NameRenderer.CamelCase)
    val KebabCase: PathRenderer = PathRenderer(".", NameRenderer.KebabCase)
    val SnakeCase: PathRenderer = PathRenderer(".", NameRenderer.SnakeCase)
    val TitleCase: PathRenderer = PathRenderer(".", NameRenderer.TitleCase)
  }
}
