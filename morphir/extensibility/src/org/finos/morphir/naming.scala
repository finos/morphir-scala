package org.finos.morphir
import scala.annotation.tailrec
import zio.=!=
import zio.prelude.*

object naming
    extends FQNameExports
    with ModuleNamingExports
    with NamingContextExports
    with NamingErrorExports
    with PackageNameExports
    with QNameExports {

  implicit def modulePathToModuleName(modulePath: ModulePath): ModuleName = ModuleName(modulePath.value)
  implicit def moduleNameToModulePath(moduleName: ModuleName): ModulePath = ModulePath(moduleName.value)

  final case class Name private (toList: List[String]) extends AnyVal {
    self =>
    def :+(that: String): Name = Name(self.toList :+ that)

    def +:(that: String): Name = Name(that +: self.toList)

    def ++(that: Name): Name = Name(self.toList ++ that.toList)

    // def /(that: Name): Path = Path(IndexedSeq(self, that))

    def humanize: List[String] = {
      val words                        = toList
      val join: List[String] => String = abbrev => abbrev.map(_.toUpperCase()).mkString("")

      @tailrec
      def loop(
          prefix: List[String],
          abbrev: List[String],
          suffix: List[String]
      ): List[String] =
        suffix match {
          case Nil =>
            abbrev match {
              case Nil => prefix
              case _   => prefix ++ List(join(abbrev))
            }
          case first :: rest =>
            if (first.length() == 1)
              loop(prefix, abbrev ++ List(first), rest)
            else
              abbrev match {
                case Nil => loop(prefix ++ List(first), List.empty, rest)
                case _ =>
                  loop(prefix ++ List(join(abbrev), first), List.empty, rest)
              }
        }

      loop(List.empty, List.empty, words.toList)
    }

    /**
     * Maps segments of the `Name`.
     */
    def mapParts(f: String => String): Name = Name(self.toList.map(f))

    def mkString(f: String => String)(sep: String): String =
      toList.map(f).mkString(sep)

    def render(implicit renderer: Name.Renderer): String = renderer(self)

    def toUpperCase: String = mkString(part => part.toUpperCase)("")

    // def toLocalName(implicit renderer: Name.Renderer): LocalName = {
    //   val localNameStr = render
    //   LocalName(localNameStr)
    // }

    def toLowerCase: String =
      mkString(part => part.toLowerCase)("")

    def toCamelCase: String =
      toList match {
        case Nil => ""
        case head :: tail =>
          (head :: tail.map(_.capitalize)).mkString("")
      }

    def toKebabCase: String =
      humanize.mkString("-")

    def toSnakeCase: String =
      humanize.mkString("_")

    def toTitleCase: String =
      toList
        .map(_.capitalize)
        .mkString("")

    override def toString: String = toList.mkString("[", ",", "]")
  }

  object Name {

    val empty: Name = Name(Nil)

    private[morphir] def wrap(value: List[String]): Name = Name(value)

    private[morphir] def wrap(value: Array[String]): Name = Name(value.toList)

    def apply(first: String, rest: String*): Name =
      fromIterable(first +: rest)

    private val pattern = """([a-zA-Z][a-z]*|[0-9]+)""".r

    @inline def fromList(list: List[String]): Name = fromIterable(list)

    def fromIterable(iterable: Iterable[String]): Name =
      wrap(iterable.flatMap(str => pattern.findAllIn(str)).map(_.toLowerCase).toList)

    def fromString(str: String): Name =
      Name(pattern.findAllIn(str).toList.map(_.toLowerCase()))

    /**
     * Creates a new name from a chunk of strings without checking.
     */
    private[morphir] def unsafeMake(value: List[String]): Name = Name(value)

    private[morphir] def unsafeMake(exactSegments: String*): Name = Name(exactSegments.toList)

    def toList(name: Name): List[String] = name.toList

    @inline def toTitleCase(name: Name): String = name.toTitleCase

    @inline def toCamelCase(name: Name): String = name.toCamelCase

    @inline def toSnakeCase(name: Name): String = name.toSnakeCase

    @inline def toKebabCase(name: Name): String = name.toKebabCase

    @inline def toHumanWords(name: Name): List[String] = name.humanize

    object VariableName {
      def unapply(name: Name): Option[String] =
        Some(name.toCamelCase)
    }

    type Renderer = Renderer.Type
    object Renderer extends Subtype[Name => String] {
      val CamelCase: Renderer = Renderer(Name.toCamelCase)
      val KebabCase: Renderer = Renderer(Name.toKebabCase)
      val SnakeCase: Renderer = Renderer(Name.toSnakeCase)
      val TitleCase: Renderer = Renderer(Name.toTitleCase)

      implicit val default: Renderer = TitleCase

      implicit class RendererOps(val self: Renderer) extends AnyVal {
        def apply(name: Name): String  = self(name)
        def render(name: Name): String = self(name)
      }
    }

  }

  type Namespace = Namespace.Type

  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace  = Namespace(Path.fromList(parts.toList))
    def fromPath(path: Path): Namespace = wrap(path)

    implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(namespace.value / name)

      @inline def toPath: Path = namespace.value

      @inline def value: Path = unwrap(namespace)
    }
  }

  sealed trait NodePathStep
  object NodePathStep {
    def childByName(input: String): NodePathStep = ChildByName(Name.fromString(input))
    def childByIndex(index: Int): NodePathStep   = ChildByIndex(index)

    final case class ChildByName(name: Name)  extends NodePathStep
    final case class ChildByIndex(index: Int) extends NodePathStep
  }

  type NodePath = NodePath.Type
  object NodePath extends Subtype[List[NodePathStep]] {
    import NodePathStep.*
    val empty: NodePath = wrap(List.empty)

    def apply(first: Name, rest: Name*): NodePath     = wrap((first :: rest.toList).map(ChildByName(_)))
    def apply(first: String, rest: String*): NodePath = wrap((first :: rest.toList).map(childByName(_)))

    @inline def fromIterable(iterable: Iterable[NodePathStep]): NodePath = wrap(iterable.toList)

    def fromString(input: String): NodePath =
      if (input.isEmpty()) empty
      else {
        fromIterable(input.split(":").map { stepString =>
          stepString.toIntOption match {
            case Some(index) => NodePathStep.childByIndex(index)
            case None        => NodePathStep.childByName(stepString)
          }
        })
      }

    def toString(nodePath: NodePath): String =
      if (nodePath.isEmpty) ""
      else {
        nodePath.map {
          case ChildByName(name)   => name.toCamelCase
          case ChildByIndex(index) => index.toString()
        }.mkString("#", ":", "")
      }

    implicit class NodePathOps(val self: NodePath) extends AnyVal {
      @inline def toList: List[NodePathStep] = unwrap(self)
    }
  }

  final case class Path(segments: Vector[Name]) extends PathLike {
    self =>

    def render(implicit renderer: Path.Renderer): String = renderer(self)
    def render(separator: String)(implicit nameRenderer: Name.Renderer): String =
      render(Path.Renderer(separator, nameRenderer))

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

    private[morphir] def unsafeMake(parts: Name*): Path = Path(parts.toVector)

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

  private[morphir] trait PathLike {
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
    def /(that: PathLike): Path = Path(segments ++ that.toList)
    // def %(other: Path): PackageAndModulePath =
    //   PackageAndModulePath(PackageName(self), ModulePath(other))

    def zip(other: Path): (Path, Path) = (self.toPath, other)

    def segments: Vector[Name]

    def toString(f: Name => String, separator: String): String =
      toList.map(f).mkString(separator)

    /** Checks if this path is a prefix of provided path */
    def isPrefixOf(path: PathLike): Boolean = Path.isPrefixOf(self, path)
  }

}
