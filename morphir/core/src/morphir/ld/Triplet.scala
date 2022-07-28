package morphir.ld

final case class Triplet(subject: Term, predicate: Term, obj: Term) {
  @inline def `object`: Term = obj
}
