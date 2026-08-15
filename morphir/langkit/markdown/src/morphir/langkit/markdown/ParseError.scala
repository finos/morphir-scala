package morphir.langkit.markdown

/** A failure from the stub markdown parser. */
final case class ParseError(message: String) extends Exception(message)
