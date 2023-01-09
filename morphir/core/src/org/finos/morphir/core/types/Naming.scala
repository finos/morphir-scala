package org.finos.morphir.core.types

import scala.annotation.tailrec

object Naming:

  final case class Name private (toList: List[String]) extends AnyVal {
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

    private val pattern = """[a-zA-Z][a-z]*|[0-9]+""".r

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

  final case class Path(segments: List[Name]) {
    self =>

    def ++(that: Path): Path = Path(segments ++ that.segments)

    /** Constructs a new path by combining this path with the given name. */
    def /(name: Name): Path = Path(segments ++ List(name))

    /** Constructs a new path by combining this path with the given path. */
    def /(that: Path): Path = Path(segments ++ that.segments)
    // def %(other: Path): PackageAndModulePath =
    //   PackageAndModulePath(PackageName(self), ModulePath(other))

    // def %(name: Name): ModuleName = ModuleName(self, name)
    def ::(name: Name): QName = QName(self, name)

    /** Indicates whether this path is empty. */
    def isEmpty: Boolean = segments.isEmpty

    def zip(other: Path): (Path, Path) = (self, other)

    def toList: List[Name] = segments.toList

    def toString(f: Name => String, separator: String): String =
      segments.map(f).mkString(separator)

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

    def getModulePath(qname: QName): Path = qname.modulePath

    def toString(qName: QName): String = qName.toString

    def fromString(str: String): Option[QName] =
      str.split(":") match {
        case Array(packageNameString, localNameString) =>
          Some(QName(Path.fromString(packageNameString), Name.fromString(localNameString)))
        case _ => None
      }
  }
