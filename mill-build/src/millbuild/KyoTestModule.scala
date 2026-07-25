package millbuild

import mill.*, scalalib.*

/**
 * [[TestModule]] mixin wiring in kyo-test, the Kyo project's own sbt test framework
 * (https://github.com/getkyo/kyo/tree/main/kyo-test), analogous to the built-in [[TestModule.ZioTest]].
 *
 * Unlike zio-test-sbt, kyo-test publishes a different `sbt.testing.Framework` class per platform rather than reusing
 * one name everywhere — [[KyoTest]] is for the JVM; use [[KyoTestJS]] on a `ScalaJSModule`'s test object.
 */
trait KyoTest extends TestModule {
  override def testFramework: T[String] = "kyo.test.runner.SbtFramework"
}

/** [[KyoTest]] variant for `ScalaJSTests` — kyo-test's JS runner uses a different framework class. */
trait KyoTestJS extends TestModule {
  override def testFramework: T[String] = "kyo.test.runner.JsFramework"
}
