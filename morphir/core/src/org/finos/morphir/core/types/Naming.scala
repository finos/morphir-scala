package org.finos.morphir.core.types

import scala.annotation.tailrec

object Naming:

  sealed trait IName

  final class Name private (private[core] val textRepr: Name.TextRepr) extends IName:
    self =>

    import Name.*

    def render(using renderer: Name.TextReprRenderer): String = renderer(textRepr)

    def toList: List[String] = textRepr.toList

    override def toString: String = textRepr.encodedValue

    override def hashCode(): Int = textRepr.hashCode

    override def equals(that: Any): Boolean = that match
      case other: Name => self.textRepr == other.textRepr
      case _           => false

  end Name

  object Name:
    // val ElidedChar: Char = '✘' // '\u2718' // ✘
    val SplitChar: Char  = '•'
    val SplitStr: String = SplitChar.toString

    def apply(input: CharSequence): Name = new Name(TextRepr(input))

    def fromList(words: List[String]): Name = Name(words.mkString(SplitStr))
    def fromString(input: String): Name     = new Name(TextRepr(input))

    opaque type TextRepr = String

    object TextRepr:
      private val splitPattern = """([a-zA-Z][a-z]*|[0-9]+)""".r

      def apply(input: CharSequence): TextRepr = encode(input)

      def encode(input: CharSequence): TextRepr =
        splitPattern.findAllMatchIn(input).map(_.matched.toLowerCase).mkString(SplitStr)

      def unencoded(input: CharSequence): TextRepr = input.toString

    end TextRepr

    extension (repr: TextRepr)
      def isEmpty: Boolean = repr.isEmpty
      def humanize: List[String] =
        val words                        = repr.toList
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

        loop(List.empty, List.empty, words)
      end humanize

      def encodedValue: String = repr
      def toList: List[String] =
        if repr.isEmpty then List.empty
        else repr.split(SplitChar).toList

    end extension

    trait TextReprRenderer:
      def apply(repr: TextRepr): String

    object TextReprRenderer:
      def raw: TextReprRenderer = repr => repr.toString

      def snakeCase: TextReprRenderer = repr => repr.split(SplitChar).mkString("_")

  end Name

  case class Path(toList: List[Name]) extends IName

  opaque type ModuleName <: Path = Path
  object ModuleName:
    def apply(path: Path): ModuleName = path
