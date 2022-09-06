package morphir.util

import zio.test.*
object ScopedVarSpec extends ZIOSpecDefault:
  def spec = suite("ScopedVarSpec")(
    test("ScopedVars can be assigned to and retrieved within a scope"){
      val flag = new ScopedVar[Boolean]
      val total = new ScopedVar[Long]
      val (flagValue,totalValue) = ScopedVar.scoped(
        flag := true,
        total := 999
      ){
        (flag.get, total.get)
      }
      assertTrue(flagValue == true, totalValue == 999L)
    },
    test("When unitialized, 'get' throws a ScopedVar.Unitialized exception"){
      val notInitialized = new ScopedVar[String]
      assert(notInitialized.get)(Assertion.throwsA[ScopedVar.Unitialized])
    }
  )
