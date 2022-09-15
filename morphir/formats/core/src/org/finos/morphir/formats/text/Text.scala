package org.finos.morphir.formats.text
import org.finos.morphir.formats.HasContent

enum Text extends HasContent[String]:
  case Plain(content: String)
  case Redacted(content: String)

  override def toString(): String = content

object Text:
  given Conversion[Text, String] = _.content
