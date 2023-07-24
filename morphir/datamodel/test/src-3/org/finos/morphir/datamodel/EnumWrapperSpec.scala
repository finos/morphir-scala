package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.namespacing.*

class EnumWrapperSpec extends munit.FunSuite {
  case class MyBool(value: Boolean)

  given rootName: GlobalDatamodelContext with {
    override def value = root / "test" % ns / "enumwrapper"
  }

  def enumMaker(label: String, data: Data, concept: Concept) =
    SingleEnumWrapper(label, concept, rootName.value)

  test("Bool Deriver") {
    given SpecificDeriver[MyBool] = Data.Boolean.deriveEnumWrapper("MyBoolLabel", _.value)
    val myBoolData                = Data.Boolean(true)
    val maker                     = enumMaker("MyBoolLabel", myBoolData, Concept.Boolean)
    val myBool                    = MyBool(true)
    val myBoolDeriver             = Deriver.gen[MyBool]
    assertEquals(myBoolDeriver.concept, maker.concept)
    assertEquals(myBoolDeriver.derive(myBool), maker.construct(myBoolData))
  }
}
