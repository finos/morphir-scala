package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.{*, given}

object PrintSpecExample {
  import EnumGns._
  implicit val gnsImpl: GlobalDatamodelContext = new GlobalDatamodelContext {
    def value = gns
  }

  case class Person(name: String, age: Int)

  sealed trait Foo
  case object Bar                               extends Foo
  case class Baz(value: String, person: Person) extends Foo

  def main(args: Array[String]): Unit = {
    val derive = DataEncoder.gen[Foo]
    println(PrintSpec.of(derive.concept))
  }
}
