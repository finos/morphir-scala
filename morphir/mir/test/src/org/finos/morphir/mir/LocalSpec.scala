package org.finos.morphir.mir

import zio.test.*
object LocalSpec extends ZIOSpecDefault:
  def spec = suite("LocalSpec")(
    test("Local should be able to be created") {
      val local = Local(100)
      assertTrue(local.id == 100L)
    },
    test("Local should be able to be shown") {
      val local = Local(100)
      assertTrue(local.show == "%100")
    },
    test("Local should support extractors") {
      val local = Local(1999)
      assertTrue(Local.unapply(local) == Some(1999L))
    }
  )
