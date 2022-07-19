package morphir.ld
import java.net.URI
import java.net.URISyntaxException
import scala.util.control.NonFatal

final case class Iri private (toURI: URI) extends AnyVal {}

object Iri {
  sealed trait ParseError
  object ParseError {
    final case class SyntaxError(input: String, reason: String, index: Int) extends ParseError
    final case class InvalidUri(input: String)                              extends ParseError
  }

  def fromString(input: String): Either[ParseError, Iri] =
    try
      Right(Iri(new URI(input)))
    catch {
      case e: URISyntaxException  => Left(ParseError.SyntaxError(input, e.getReason, e.getIndex))
      case NonFatal(_: Throwable) => Left(ParseError.InvalidUri(input))
    }

  def unsafeFromString(iri: String): Iri = new Iri(new URI(iri))

}
