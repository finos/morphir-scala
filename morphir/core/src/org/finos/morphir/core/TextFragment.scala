package org.finos.morphir.core

enum TextFragment:
  self =>
  import TextFragment.*
  case Raw(rawValue: String)
  case Marked(rawValue: String, markers: Array[Int])

  def rawValue: String
  inline def isMarked: Boolean =
    self match
      case Raw(_)       => false
      case Marked(_, _) => true

  def render(using renderer: Renderer): String =
    self match
      case Raw(v)                                => v
      case Marked(v, markers) if markers.isEmpty => v
      case Marked(v, markers)                    => ???
  override def toString: String = summon[Renderer](self)

object TextFragment:
  trait Renderer:
    def apply(run: TextFragment): String

  object Renderer:
    given Renderer = run => run.rawValue
  inline def apply(text: String): TextFragment = raw(text)
  def apply(text: String)(findMarkers: String => Seq[Int]): TextFragment =
    val len     = text.length
    val markers = findMarkers(text).filter(n => n >= 0 && n < len)
    TextFragment.Marked(text, markers.toArray)

  def raw(text: String): TextFragment = TextFragment.Raw(text)
end TextFragment
