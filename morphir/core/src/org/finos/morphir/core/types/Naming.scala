package org.finos
package morphir
package core.types

import morphir.prelude.*
import scala.annotation.tailrec

object Naming {
  final case class FQName(packagePath: PackageName, modulePath: ModulePath, localName: Name) {
    def getPackagePath: Path = packagePath.toPath

    def getModulePath: Path = modulePath.toPath

    def getModuleName: ModuleName = ModuleName(modulePath.toPath, localName)

    def toReferenceName: String = Seq(
      Path.toString(Name.toTitleCase, ".", packagePath.toPath),
      Path.toString(Name.toTitleCase, ".", modulePath.toPath),
      localName.toTitleCase
    ).mkString(".")

    override def toString: String = Array(
      Path.toString(Name.toTitleCase, ".", packagePath.toPath),
      Path.toString(Name.toTitleCase, ".", modulePath.toPath),
      Name.toCamelCase(localName)
    ).mkString(":")
  }

  object FQName {
//    def apply(packagePath: Path, modulePath: Path, localName: Name): FQName =
//      FQName(PackageName(packagePath), ModulePath(modulePath), localName)

    val fqName: Path => Path => Name => FQName = packagePath =>
      modulePath => localName => FQName(PackageName(packagePath), ModulePath(modulePath), localName)

    def fromQName(packagePath: Path, qName: QName): FQName =
      FQName(PackageName.fromPath(packagePath), QName.getModulePath(qName), QName.getLocalName(qName))

    def fromQName(qName: QName)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, ModulePath(QName.getModulePath(qName)), QName.getLocalName(qName))

    /** Get the package path part of a fully-qualified name. */
    def getPackagePath(fqName: FQName): Path = fqName.getPackagePath

    /** Get the module path part of a fully-qualified name */
    def getModulePath(fqName: FQName): Path = fqName.getModulePath

    /** Get the local name part of a fully-qualified name */
    def getLocalName(fqName: FQName): Name = fqName.localName

    /** Convenience function to create a fully-qualified name from 3 strings */
    def fqn(packageName: String, moduleName: String, localName: String): FQName =
      FQName(PackageName.fromString(packageName), ModulePath.fromString(moduleName), Name.fromString(localName))

    /** Convenience function to create a fully-qualified name from 2 strings with default package name */
    def fqn(moduleName: String, localName: String)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, ModulePath(Path.fromString(moduleName)), Name.fromString(localName))

    /** Convenience function to create a fully-qualified name from 1 string with defaults for package and module */
    def fqn(localName: String)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, options.defaultModule, Name.fromString(localName))

    def fqn(moduleName: ModuleName)(implicit options: FQNamingOptions): FQName =
      FQName(options.defaultPackage, ModulePath(moduleName.namespace), moduleName.localName)

    def toString(fqName: FQName): String = fqName.toString

    /** Parse a string into a FQName using splitter as the separator between package, module, and local names */
    def fromString(fqNameString: String, splitter: String)(implicit options: FQNamingOptions): FQName =
      fqNameString.split(splitter) match {
        case Array(packageNameString, moduleNameString, localNameString) =>
          fqn(packageNameString, moduleNameString, localNameString)
        case Array(moduleNameString, localNameString) =>
          fqn(moduleNameString, localNameString)
        case Array(localNameString) =>
          fqn(localNameString)
        case _ => throw ParserError(s"Unable to parse: [$fqNameString] into a valid FQName")
      }

    def fromString(fqNameString: String)(implicit options: FQNamingOptions): FQName =
      fromString(fqNameString, options.defaultSeparator)
  }

  final case class FQNamingOptions(defaultPackage: PackageName, defaultModule: ModulePath, defaultSeparator: String)

  object FQNamingOptions {
    implicit val default: FQNamingOptions = FQNamingOptions(PackageName(Path.empty), ModulePath(Path.empty), ":")
  }

  trait IName extends Any

  final case class Name private (toList: List[String]) extends AnyVal with IName {
    self =>
    def :+(that: String): Name = Name(self.toList :+ that)

    def +:(that: String): Name = Name(that +: self.toList)

    def ++(that: Name): Name = Name(self.toList ++ that.toList)

    def /(that: Name): Path = Path(List(self, that))

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

    def toUpperCase: String = mkString(part => part.toUpperCase)("")

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

  }

  sealed trait PathLike extends IName

  final case class Path(toList: List[Name]) extends PathLike {
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

  final case class QName(modulePath: Path, localName: Name) {
    @inline def toTuple: (Path, Name) = (modulePath, localName)

    override def toString: String =
      modulePath.toString(Name.toTitleCase, ".") + ":" + localName.toCamelCase

  }

  object QName {
    def toTuple(qName: QName): (Path, Name) = qName.toTuple

    def fromTuple(tuple: (Path, Name)): QName = QName(tuple._1, tuple._2)

    def fromName(modulePath: Path, localName: Name): QName = QName(modulePath, localName)

    def getLocalName(qname: QName): Name = qname.localName

    def getModulePath(qname: QName): ModulePath = ModulePath(qname.modulePath)

    def toString(qName: QName): String = qName.toString

    def fromString(str: String): Option[QName] =
      str.split(":") match {
        case Array(packageNameString, localNameString) =>
          Some(QName(Path.fromString(packageNameString), Name.fromString(localNameString)))
        case _ => None
      }
  }

  type Namespace = Namespace.Type
  object Namespace extends Subtype[Path] {
    def apply(parts: Name*): Namespace = Namespace(Path.fromList(parts.toList))

    implicit class NamespaceOps(val namespace: Namespace) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(namespace.value / name)
      @inline def toPath: Path      = namespace.value
      @inline def value: Path       = unwrap(namespace)
    }
  }

  /**
   * A module name is a unique identifier for a module within a package. It is represented by a pth, which is a list of
   * names.
   */
  type ModuleName = ModuleName.Type
  object ModuleName extends Subtype[Path] {

    def apply(path: Path, name: Name): ModuleName = ModuleName(path / name)
    def apply(input: String): ModuleName =
      ModuleName(Path(input.split('.').map(Name.fromString).toList))

    def fromString(input: String): ModuleName = ModuleName(input)

    implicit class ModuleNameOps(val self: ModuleName) extends AnyVal {
      @inline def localName: Name = name
      def name: Name =
        self match {
          case ModuleName(Path(Nil))      => Name.empty
          case ModuleName(Path(segments)) => segments.last
        }

      def namespace: Namespace = Namespace(Path(self.value.toList.dropRight(1)))
      def toPath: Path         = unwrap(self)
      def value: Path          = unwrap(self)
    }
  }

  type ModulePath = ModulePath.Type
  object ModulePath extends Subtype[Path] {
    def apply(parts: Name*): ModulePath     = wrap(Path.fromList(parts.toList))
    def fromString(str: String): ModulePath = wrap(Path.fromString(str))

    implicit class ModulePathOps(val modulePath: ModulePath) extends AnyVal {
      def /(name: Name): ModuleName = ModuleName(modulePath.value / name)
      @inline def toPath: Path      = modulePath.value
      @inline def value: Path       = unwrap(modulePath)
    }
  }

  /**
   * A qualified module name is a globally unique identifier for a module. It is represented by the combination of a
   * package name and the module name
   */
  final case class QualifiedModuleName(packageName: PackageName, module: ModuleName) {
    lazy val toPath: Path = packageName / module

    def toTuple: (Path, Path) = (packageName, module)
  }

  object QualifiedModuleName {
    //    object AsTuple {
    //      def unapply(name: QualifiedModuleName): Option[(Path, ModuleName)] =
    //        Some(name.toTuple)
    //    }
  }

  /**
   * A package name is a globally unique identifier for a package. It is represented by a `Path` which is a list of
   * names.
   */
  type PackageName = PackageName.Type

  object PackageName extends Subtype[Path] {
    def apply(firstPart: Name, rest: Name*): PackageName = wrap(Path(firstPart :: rest.toList))
    def fromPath(path: Path): PackageName                = wrap(path)
    def fromString(str: String): PackageName             = wrap(Path.fromString(str))

    implicit class PackageNameOps(val self: PackageName) extends AnyVal {
      def value: Path  = unwrap(self)
      def toPath: Path = unwrap(self)
    }
  }
}
