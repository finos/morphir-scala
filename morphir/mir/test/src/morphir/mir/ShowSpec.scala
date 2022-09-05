package morphir.mir

import morphir.mir.{Spec => MIRSpec}
import zio.test.*
import zio.test.Assertion.*
import zio.ZIO
import morphir.util.UnreachableException
object ShowSpec extends ZIOSpecDefault:
  def spec = suite("ShowSpec")(globalSuite, localSuite)

  def globalSuite = suite("Global")(
    test("It should show a Global.Top"){
      val global = Global.Top("morphir.sdk.Bool")
      assertTrue(Show(global) == "morphir.sdk.Bool")
    },
    test("It should throw an UnreachableException when showing a Global.None"){
      val global = Global.None
      assert(Show(global))(throwsA[UnreachableException])      
    },
    test("It should show a Global.Member"){
      val module = Global.Top("morphir.sdk.Bool")
      val global = module.member(MIRSpec.Func)
      assertTrue(Show(global) == "morphir.sdk.Bool/")
    }
  )

  def localSuite = suite("Local")(
    test("It should support showing a Local"){
      val local = Local(255)
      assertTrue(Show(local) == "%255")
    }
  )

