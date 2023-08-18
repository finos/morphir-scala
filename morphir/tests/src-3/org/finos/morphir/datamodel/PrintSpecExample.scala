package org.finos.morphir.datamodel

import org.finos.morphir.naming._

/**
 * PrintSpec allows you to print out an "Elmish" representation of a Morphir concept. The focus of `PrintSpec` is to
 * allow you to see what the type specification/definition of a concept would look like in Elm.
 */
object PrintSpecExample {
  implicit val gnsImpl: GlobalDatamodelContext = new GlobalDatamodelContext {
    def value = root / "morphir" % "test" / "PrintSpecExample"
  }

  case class Person(name: String, age: Int)

  sealed trait Foo
  case object Bar                               extends Foo
  case class Baz(value: String, person: Person) extends Foo

  def main(args: Array[String]): Unit = {
    // Obtain an instance of a Deriver for the Foo type
    val derive = Deriver.gen[Foo]
    println(PrintSpec.of(derive.concept))
  }
}
