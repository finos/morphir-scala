package morphir.ld

sealed trait Term
object Term {
  final case class Iri(value: morphir.ld.Iri) extends Term
  final case class Literal(value: String)     extends Term
  final case class Blank(value: String)       extends Term
}
