package org.finos.morphir.testing.munit

import zio.*

class DemoSuite extends ZSuite {
  testZ("1 + 1 = 2") {
    for {
      a <- ZIO.attempt(1)
      b <- ZIO.attempt(1)
    } yield assertEquals(a + b, 2)
  }

  testZ("false OR true should be true") {
    val effect = ZIO.succeed(false || true)
    assertZ(effect)
  }

  testZ("strings are the same") {
    val effect = ZIO.succeed("string")
    assertNoDiffZ(effect, "string")
  }

  testZ("values are the same") {
    val effect = ZIO.succeed(42)
    assertEqualsZ(effect, 42)
  }

  testZ("effect should fail") {
    val effect = ZIO.fail(new IllegalArgumentException("BOOM!"))
    effect.interceptFailure[IllegalArgumentException]
  }

  testZ("effect should fail with message") {
    val effect = ZIO.fail(new IllegalArgumentException("BOOM!"))
    effect.interceptFailureMessage[IllegalArgumentException]("BOOM!")
  }

  testZ("effect should die") {
    val effect = ZIO.die(new IllegalArgumentException("BOOM!"))
    effect.interceptDefect[IllegalArgumentException]
  }
}
