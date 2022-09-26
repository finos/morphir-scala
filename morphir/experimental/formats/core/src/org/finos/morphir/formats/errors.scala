package org.finos.morphir.formats
import org.finos.morphir.formats.text.Text

object errors:
  enum FormatError(message: String) extends Exception(message):
    case TextParseError(message: String, maybeText: Option[Text]) extends FormatError(message)

  object FormatError:
    def textParseError(message: String): FormatError.TextParseError =
      FormatError.TextParseError(message, None)
    def textParseError(message: String, text: Text): FormatError.TextParseError =
      FormatError.TextParseError(message, Option(text))

  enum DecodingError(message: String) extends Exception(message):
    case FormatError(message: String, formatError: errors.FormatError) extends DecodingError(message)
    // case MissingField(fieldName: String) extends DecodingError
    // case InvalidField(fieldName: String, message: String) extends DecodingError
    // case InvalidValue(message: String) extends DecodingError
