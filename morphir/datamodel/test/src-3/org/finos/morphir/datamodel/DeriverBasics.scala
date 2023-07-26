package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.*

class DataEncoderBasics extends munit.FunSuite {

  given rootName: GlobalDatamodelContext with {
    override def value = root / "test" % ns / "enumwrapper"
  }

  case class Person(name: String, age: Int)
  def decode[T](t: T)(implicit deriver: Deriver[T]) =
    deriver.derive(t)

  test("Decode With Implicit") {
    val john         = Person("John", 42)
    val deriveManual = decode(john)
    val derived      = Deriver.gen[Person].derive(john)
    assertEquals(deriveManual, derived)
  }

  case class PersonWithDeriver(name: String, age: Int) derives Deriver

  test("Decode With Derive") {
    val john         = PersonWithDeriver("John", 42)
    val deriveManual = PersonWithDeriver.derived$Deriver.derive(john)
    val derived      = Deriver.gen[PersonWithDeriver].derive(john)
    assertEquals(deriveManual, derived)
  }
}
