package org.finos.morphir.formats
import org.finos.morphir.formats.text.Text

object errors:
  enum FormatError:
    case TextParseError(message: String, text: Text)
