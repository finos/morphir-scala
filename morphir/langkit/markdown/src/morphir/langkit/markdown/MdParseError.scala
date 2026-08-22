package morphir.langkit.markdown

import kyo.*
import morphir.MorphirException
import morphir.langkit.core.scanner.*

/** A failure from the markdown parser. */
sealed abstract class MdParseError(val message: String) extends MorphirException(message)

object MdParseError:
  final case class Syntax(override val message: String) extends MdParseError(message)

  final case class Scan(error: ScanFailure) extends MdParseError(renderScanFailure(error))

  def apply(message: String): Syntax = Syntax(message)

  def unapply(error: MdParseError): Some[String] = Some(error.getMessage)

  private def renderScanFailure(error: ScanFailure): String =
    val renderedPhase = error.phase.fold("")(phase => s" during ${phase.value}")
    s"Markdown scan failed at offset ${error.offset.toInt}$renderedPhase: ${error.exceeded}"
