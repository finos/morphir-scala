package morphir.ld
import io.lemonlabs.uri.Uri
import scala.util.control.NonFatal

final case class Iri private (toUri: Uri) extends AnyVal {}

object Iri {

  def parseOption(input: String): Option[Iri] = Uri.parseOption(input).map(Iri(_))
  def unsafeFromString(iri: String): Iri = Iri(Uri.parse(iri))

}
