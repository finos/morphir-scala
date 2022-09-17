package org.finos.morphir.mir

import org.finos.morphir.testing.MorphirBaseSpec
import zio.test._

object AccessControlledSpec extends MorphirBaseSpec {
  def spec = suite("AccessControlled")(
    // test("Supports for comprehensions") {
    //   val code =
    //     """
    //           for {
    //               publicInt <- AccessControlled.publicAccess(10)
    //               privateStr <- AccessControlled.privateAccess("hello")
    //           } yield (publicInt, privateStr)
    //           """
    //   assertM(typeCheck(code))(isRight)
    // },
    test("Equality") {
      assertTrue(
        AccessControlled.publicAccess(10) == AccessControlled.publicAccess(10),
        AccessControlled.publicAccess(10) != AccessControlled.privateAccess(10),
        AccessControlled.publicAccess(10).value == AccessControlled.privateAccess(10).value,
        AccessControlled.publicAccess(10).access != AccessControlled.privateAccess(10).access
      )
    }
  )
}
