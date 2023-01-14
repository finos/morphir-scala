package org.finos
package morphir.lang.ir

import morphir.testing.MorphirBaseSpec
import org.finos.morphir.ir.Module.ModuleName
import zio.test.*
object BundlerSpec extends MorphirBaseSpec {
  def spec = suite("BundlerSpec")(
    bundleSuite
  )

  def bundleSuite = suite("When calling bundle")(
    test("it should return the correct module names and count") {
      val (bundleName, actual) = bundle("MyBundle")(
        morphir.samples.Hello,
        morphir.samples.Basics
      )
      pprint.pprintln(actual)
      assertTrue(
        bundleName == "MyBundle",
        actual(0) == ModuleName.fromString("org.finos.morphir.samples.Hello"),
        actual(1) == ModuleName.fromString("org.finos.morphir.samples.Basics"),
        actual.size == 2
      )
    }
  )
}
