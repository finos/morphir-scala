package org.finos.morphir.testing.munit

import zio.*

class DemoSuite extends ZSuite {
    testZ("1 + 1 = 2") {
    for {
      a <- ZIO.attempt(1)
      b <- ZIO.attempt(1)
    } yield assertEquals(a + b, 2)
  }
}
