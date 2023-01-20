package org.finos
package morphir
package lang.scaladsl

import morphir.ir.Module.ModuleName
import morphir.ir.Package.PackageName
import morphir.testing.MorphirBaseSpec
import zio.test.*
import Syntax.{Bundle, BundleInfo}

object BundlerSpec extends MorphirBaseSpec {
  def spec = suite("BundlerSpec")(
    bundleSuite
  )

  def bundleSuite = suite("When calling bundle")(
    test("it should return the correct module names and count") {
      val packageName = PackageName.fromString("org.finos.morphir")
      val bundleInfo = BundleInfo(
        name = packageName
      )
      // -- 3
      val actual = bundle(bundleInfo)(
        morphir.samples.Hello,
        morphir.samples.Basics
      )
      pprint.pprintln(actual)
      assertTrue(
        actual.info == BundleInfo(packageName),
        // TODO: Make it so that the module names do not include the package name portion
        // i.e. the name should be samples.Hello and samples.Basics
        actual.modules(0).name == ModuleName.fromString("org.finos.morphir.samples.Hello"),
        actual.modules(1).name == ModuleName.fromString("org.finos.morphir.samples.Basics"),
        actual.modules.size == 2
      )
    }
  )
}
