package morphir.langkit.markdown

/** A failure from the markdown parser. */
final case class ParseError(message: String) extends Exception(message)
