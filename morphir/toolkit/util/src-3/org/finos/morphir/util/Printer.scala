package org.finos.morphir.util

import scala.util.NotGiven

trait Printer[-A]:
  extension (a: A)
    def text: Printer.Text
  def tprint: String = text.render

object Printer:
  type Renderer = Text => String
  object Renderer:
    val NL: String = System.lineSeparator

    val Simple: Renderer = {
      case Text.Section(title, items) =>
        val titleStr = title.render
        val itemsStr = items.map(_.render).mkString(NL)
        s"$titleStr$NL$itemsStr"
      case Text.Run(v)       => v.toString()
      case Text.Group(items) => items.map(Simple).mkString(System.lineSeparator())
    }

    extension (s: String) def newline: String = s + System.lineSeparator()

  enum Text:
    self =>
    case Run(value: String)
    case Group(items: List[Text])
    case Section(title: Text, items: List[Text])

    def render(renderer: Renderer): String =
      renderer(self)

    def render: String =
      render(Renderer.Simple)
  end Text

  object Text:
    def group(items: Iterable[Text]): Group = Text.Group(items.toList)
  end Text

  given [A](using Printer[A]): Printer[Seq[A]] with
    extension (a: Seq[A]) def text: Text = Text.group(a.map(_.text))

  given Printer[Boolean] with
    extension (a: Boolean) def text: Text = Text.Run(a.toString)

  given Printer[Int] with
    extension (a: Int) def text: Text = Text.Run(a.toString)

  given Printer[String] with
    extension (a: String) def text: Text = Text.Run(a)

  given [A](using NotGiven[Printer[A]]): Printer[A] with
    extension (a: A) def text: Text = Text.Run(a.toString)
