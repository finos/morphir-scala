package morphir.langkit.markdown

import morphir.langkit.core.scanner.*

/** A failure from the markdown parser. */
sealed abstract class ParseError(val message: String) extends Exception(message)

object ParseError:
  final case class Syntax(override val message: String) extends ParseError(message)

  final case class Scan(error: ScanFailure) extends ParseError(renderScanFailure(error))

  def apply(message: String): Syntax = Syntax(message)

  def unapply(error: ParseError): Some[String] = Some(error.getMessage)

  private def renderScanFailure(error: ScanFailure): String =
    val renderedPhase = error.phase.fold("")(phase => s" during ${phase.value}")
    s"Markdown scan failed at offset ${error.offset.toInt}$renderedPhase: ${error.exceeded}"
