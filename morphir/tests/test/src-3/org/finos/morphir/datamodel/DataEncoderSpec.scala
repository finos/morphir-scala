package org.finos.morphir.datamodel

import org.finos.morphir.naming._
import org.finos.morphir.datamodel._
import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._
import zio.{test => _}

object DataEncoderSpec extends MorphirBaseSpec {
  def spec = suite("DataEncoderSpec")(
    test("Decode With Implicit") {
      val john         = Person("John", 42)
      val deriveManual = decode(john)
      val derived      = Deriver.gen[Person].derive(john)
      assertTrue(derived == deriveManual)
    },
    test("Decode With Derive") {
      val john         = PersonWithDeriver("John", 42)
      val deriveManual = PersonWithDeriver.derived$Deriver.derive(john)
      val derived      = Deriver.gen[PersonWithDeriver].derive(john)
      assertTrue(derived == deriveManual)
    }
  )

  given rootName: GlobalDatamodelContext with {
    override def value = root / "test" % "enumwrapper"
  }

  case class Person(name: String, age: Int)

  case class PersonWithDeriver(name: String, age: Int)
      derives Deriver

  def decode[T](t: T)(implicit deriver: Deriver[T]) =
    deriver.derive(t)
}
