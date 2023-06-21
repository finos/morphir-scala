package org.finos.morphir.datamodel

import org.finos.morphir.datamodel.{Data, Deriver}
trait ToDataSuiteVersionSpecific {
  self: ToDataSuite =>

  test("Deriver toData for an int should work") {
    val actual = Deriver.toData(42)
    assertEquals(actual, Data.Int(42))
  }
}
