package org.finos.morphir.foundations

object NewtypeTestTypes {
  type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override inline def assertion =
      (Assertion.greaterThanOrEqualTo(0))

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }
}
