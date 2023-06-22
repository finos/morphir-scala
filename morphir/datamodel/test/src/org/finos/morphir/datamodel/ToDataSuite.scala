package org.finos.morphir.datamodel
class ToDataSuite extends munit.FunSuite with ToDataSuiteVersionSpecific {
  test("It should be possible to convert a Boolean value to Data") {
    val trueValue  = Data.Boolean(true)
    val falseValue = Data.Boolean(false)
    assertEquals((trueValue, falseValue), (Data.True, Data.False))
  }
}
