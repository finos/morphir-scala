package org.finos
package morphir
package util

class ScopedVarSpec extends munit.FunSuite {
  test("ScopedVars can be assigned to and retrieved within a scope") {
    val flag  = new ScopedVar[Boolean]
    val total = new ScopedVar[Long]
    val (flagValue, totalValue) = ScopedVar.scoped(
      flag  := true,
      total := 999
    ) {
      (flag.get, total.get)
    }
    assertEquals(flagValue == true, totalValue == 999L)
  }

  test("When unitialized, 'get' throws a ScopedVar.Unitialized exception") {
    val notInitialized = new ScopedVar[String]
    intercept[ScopedVar.Unitialized](notInitialized.get)
  }
}
