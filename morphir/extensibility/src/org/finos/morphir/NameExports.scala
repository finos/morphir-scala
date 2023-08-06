package org.finos.morphir
import scala.annotation.tailrec

private[morphir] trait NameExports {

  sealed case class Name private (toList: List[String]) {
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

    def render(implicit renderer: NameRenderer): String = renderer(self)

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

  }

  abstract class NameRenderer(rendererName: String) extends (Name => String) {
    final def apply(name: Name): String = render(name)
    def render(name: Name): String
    override def toString: String = s"$rendererName Name Renderer"
  }

  object NameRenderer {
    def apply(rendererName: String)(f: Name => String): NameRenderer = new NameRenderer(rendererName) {
      override def render(name: Name): String = f(name)
    }

    val CamelCase: NameRenderer = NameRenderer("CamelCase")(Name.toCamelCase)
    val KebabCase: NameRenderer = NameRenderer("KebabCase")(Name.toKebabCase)
    val SnakeCase: NameRenderer = NameRenderer("SnakeCase")(Name.toSnakeCase)
    val TitleCase: NameRenderer = NameRenderer("TitleCase")(Name.toTitleCase)

    implicit val default: NameRenderer = TitleCase
  }
}
