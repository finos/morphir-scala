package org.finos.morphir.util

object NewtypeTestTypes {
   type Natural = Natural.Type
  object Natural extends Subtype[Int] {
    override def assertion =
      assert(Assertion.greaterThanOrEqualTo(0))

    val two: Natural = Natural(2)

    def unsafeWrap(int: Int): Natural = wrap(int)
  }
}
